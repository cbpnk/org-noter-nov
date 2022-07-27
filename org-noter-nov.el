;;; org-noter-nov.el --- Integration with Nov.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Homepage: https://github.com/cbpnk/org-noter-nov
;; Keywords: org-noter epub
;; Package-Requires: ((org-noter-core "1.5.0") (nov "0.4.0"))
;; Version: 1.5.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'org-noter-core)
(declare-function nov-render-document "ext:nov")
(defvar nov-documents-index)
(defvar nov-file-name)

(defvar-local org-noter-nov--timer nil
  "Timer for synchronizing notes after scrolling.")

(defun org-noter-nov--scroll-handler (&rest _)
  (when org-noter-nov--timer (cancel-timer org-noter-nov--timer))
  (unless org-noter--inhibit-location-change-handler
    (setq org-noter-nov--timer (run-with-timer 0.25 nil 'org-noter--doc-location-change-handler))))

(defun org-noter-nov--get-buffer-file-name ()
  (bound-and-true-p nov-file-name))

(add-hook 'org-noter-get-buffer-file-name-hook #'org-noter-nov--get-buffer-file-name)

(defun org-noter-nov--doc-approx-location (major-mode &optional precise-info _force-new-ref)
  (org-noter--with-valid-session
   (when (eq (org-noter--session-doc-mode session) 'nov-mode)
     (cons nov-documents-index (if (or (numberp precise-info)
                                       (and (consp precise-info)
                                            (numberp (car precise-info))
                                            (numberp (cdr precise-info))))
                                   precise-info
                                 (max 1 (/ (+ (window-start) (window-end nil t)) 2)))))))

(add-hook 'org-noter--doc-approx-location-hook #'org-noter-nov--doc-approx-location)

(defun org-noter-nov--mode-supported (major-mode)
  (eq major-mode 'nov-mode))

(add-hook 'org-noter--mode-supported-hook #'org-noter-nov--mode-supported)


(defun org-noter-nov--set-up-document (major-mode)
  (when (org-noter-nov--mode-supported major-mode)
    (advice-add 'nov-render-document :after 'org-noter-nov--scroll-handler)
    (add-hook 'window-scroll-functions 'org-noter-nov--scroll-handler nil t)
    t))

(add-hook 'org-noter--set-up-document-hook #'org-noter-nov--set-up-document)

(defun org-noter-nov--tear-down-document ()
  (advice-remove 'nov-render-document 'org-noter-nov--scroll-handler))

(add-hook 'org-noter--tear-down-document-hook #'org-noter-nov--tear-down-document)

(defun org-noter-nov--pretty-print-location (location)
  (org-noter--with-valid-session
   (when (org-noter-nov--mode-supported (org-noter--session-doc-mode session))
     (format "%s" (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 1))
                      (org-noter--get-location-page location)
                    location)))))

(add-hook 'org-noter--pretty-print-location-hook #'org-noter-nov--pretty-print-location)


(defun org-noter-nov--get-precise-info (major-mode)
  (when (org-noter-nov--mode-supported major-mode)
    (if (region-active-p)
        (cons (mark) (point))
      (let (event)
        (while (not (and (eq 'mouse-1 (car event))
                         (eq (selected-window) (posn-window (event-start event)))))
          (setq event (read-event "Click where you want the start of the note to be!")))
        (posn-point (event-start event))))))

(add-hook 'org-noter--get-precise-info-hook #'org-noter-nov--get-precise-info)

(defun org-noter-nov--doc-goto-location (mode location)
  (when (org-noter-nov--mode-supported mode)
    (setq nov-documents-index (org-noter--get-location-page location))
    (nov-render-document)
    (goto-char (org-noter--get-location-top location))
    ;; NOTE(nox): This needs to be here, because it would be issued anyway after
    ;; everything and would run org-noter-nov--scroll-handler.
    (recenter)))

(add-hook 'org-noter--doc-goto-location-hook #'org-noter-nov--doc-goto-location)

(defun org-noter-nov--get-current-view (mode)
  (when (org-noter-nov--mode-supported mode)
    (vector 'nov
            (org-noter-nov--doc-approx-location mode (window-start))
            (org-noter-nov--doc-approx-location mode (window-end nil t)))))

(add-hook 'org-noter--get-current-view-hook #'org-noter-nov--get-current-view)

(defun org-noter-nov--get-selected-text (mode)
  (when (and (org-noter-nov--mode-supported mode) (region-active-p))
    (buffer-substring-no-properties (mark) (point))))

(add-hook 'org-noter-get-selected-text-hook #'org-noter-nov--get-selected-text)


;; Shamelessly stolen code from Yuchen Li.
;; This code is originally from org-noter-plus package.
;; At https://github.com/yuchen-lea/org-noter-plus

(defun org-noter--handle-nov-toc-item (ol depth)
  (mapcar (lambda (li)
            (mapcar (lambda (a-or-ol)
                      (pcase-exhaustive (dom-tag a-or-ol)
                        ('a
                         (vector :depth depth
                                 :title (dom-text a-or-ol)
                                 :href (esxml-node-attribute 'href a-or-ol)))
                        ('ol
                         (org-noter--handle-nov-toc-item a-or-ol
                                                         (1+ depth)))))
                    (dom-children li)))
          (dom-children ol)))

(defun org-noter-nov--create-skeleton (mode)
  "Epub outline with nov link."
  (when (org-noter-nov--mode-supported mode)
    (require 'esxml)
    (require 'nov)
    (require 'dom)
    (org-noter--with-valid-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (let* ((toc-path (cdr (aref nov-documents 0)))
                (toc-tree (with-temp-buffer
                            (insert (nov-ncx-to-html toc-path))
                            (replace-regexp "\n"
                                            ""
                                            nil
                                            (point-min)
                                            (point-max))
                            (libxml-parse-html-region (point-min)
                                                      (point-max))))
                (origin-index nov-documents-index)
                (origin-point (point)))
           (dolist (item
                    (nreverse (flatten-tree (org-noter--handle-nov-toc-item toc-tree 1))))
             (let ((relative-level  (aref item 1))
                   (title  (aref item 3))
                   (url (aref item 5)))
               (apply 'nov-visit-relative-file
                      (nov-url-filename-and-target url))
               (when (not (integerp nov-documents-index))
                 (setq nov-documents-index 0))
               (push (vector title (list nov-documents-index (point)) relative-level) output-data)))
           (push (vector "Skeleton" (list 0) 1) output-data)

           (nov-goto-document origin-index)
           (goto-char origin-point)))
       (save-excursion
         (goto-char (org-element-property :end ast))
         (with-current-buffer (org-noter--session-notes-buffer session)
           (dolist (data output-data)
             (setq title          (aref data 0)
                   location       (aref data 1)
                   relative-level (aref data 2))

             (setq last-absolute-level (+ top-level relative-level)
                   level last-absolute-level)

             (org-noter--insert-heading level title)

             (when location
               (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

             (when org-noter-doc-property-in-notes
               (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
               (org-entry-put nil org-noter--property-auto-save-last-location "nil")))
           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))
       output-data))))

(add-hook 'org-noter-create-skeleton-functions #'org-noter-nov--create-skeleton)



(defun org-noter-nov--note-after-tipping-point (point location view)
  (when (eq (aref view 0) 'nov)
    (> (org-noter--get-location-top location)
       (+ (* point (- (cdr (aref view 2)) (cdr (aref view 1))))
          (cdr (aref view 1))))))

(add-hook 'org-noter--note-after-tipping-point-hook #'org-noter-nov--note-after-tipping-point)

(defun org-noter-nov--relative-position-to-view (location view)
  (when (eq (aref view 0) 'nov)
    (let ((view-top (aref view 1))
          (view-bot (aref view 2)))
      (cond ((org-noter--compare-locations '<  location view-top) 'before)
            ((org-noter--compare-locations '<= location view-bot) 'inside)
            (t                                                    'after)))))

(add-hook 'org-noter--relative-position-to-view-hook #'org-noter-nov--relative-position-to-view)

(provide 'org-noter-nov)
;;; org-noter-nov.el ends here
