;;; org-multiple-keymap.el --- Set keymap to elements, such as timestamp and priority. -*- lexical-binding: t -*-

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/org-multiple-keymap.el
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2015 myuhe all rights reserved.
;; Created: :15-03-15
;; Package-Requires: ((org "8.2.4") (emacs "24") (cl-lib "0.5"))
;; Keywords: convenience, org-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published byn
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:
;;
;; Put the org-multiple-keymap.el to your
;; load-path.
;; Add to .emacs:
;; (require 'org-multiple-keymap.el)
;;
;;; Changelog:
;; 2015-03-15 Initial release.

;;; Code:
(require 'cl-lib)
(require 'org-element)
(require 'eieio)

(defgroup org-multiple-keymap nil "Set keymap to elements, such as timestamp and priority."
  :tag "Org multiple keymap"
  :group 'org)

(defcustom org-mukey-source-list '(org-mukey-source-heading
                                   org-mukey-source-timestamp
                                   org-mukey-source-priority)
  "Number of days to get events before today."
  :group 'org-multiple-keymap
  :type '(repeat symbol))

(defvar org-mukey-heading-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-archive-subtree)
    (define-key map (kbd "d") (lambda () (org-todo 'done)))
    (define-key map (kbd "n") 'org-next-visible-heading)
    (define-key map (kbd "p") 'org-previous-visible-heading)
    (define-key map (kbd "f") 'org-do-demote)
    (define-key map (kbd "b") 'org-do-promote)
    (define-key map (kbd "u") 'outline-up-heading)
    map)
  "Keymap to control heading.")

(defvar org-mukey-timestamp-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'org-mukey-org-timestamp-down-day)
    (define-key map (kbd "P") 'org-mukey-org-timestamp-up-day)
    (define-key map (kbd "n") 'org-mukey-org-timestamp-down)
    (define-key map (kbd "p") 'org-mukey-org-timestamp-up)
    (define-key map (kbd "o") 'org-open-at-point)
    map)
  "Keymap to change date on timestamp element.")

(defvar org-mukey-priority-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'org-priority-down)
    (define-key map (kbd "p") 'org-priority-up)
    map)
  "Keymap to change priority on priority cookie.")

(defvar org-multiple-keymap-minor-mode nil)

;;; metaclass
(defclass org-mukey-source ()
  ((beg-list
    :type function
    :accessor org-mukey-source-get-beg
    :documentation "Return a List of point that beginning of overlay range.")
   (end-list
    :type function
    :accessor org-mukey-source-get-end
    :documentation "Return a List of point that end of overlay range.")
   (keymap
    :type symbol
    :accessor org-mukey-source-get-keymap
    :documentation "Keymap to Org-mode element.")
   (parse-function
    :type function
    :accessor org-mukey-source-get-parsefunc
    :documentation "Overlay for new Org-mode element."))
  :documentation "A meta class to define org-mukey-source.")

;;; heading class
(defclass org-mukey-source-heading (org-mukey-source)
  ((beg-list :initform 'org-mukey-make-heading-begin)
   (end-list :initform 'org-mukey-make-heading-end)
   (keymap :initform org-mukey-heading-map)
   (parse-function :initform 'org-mukey-heading-refresh)))

(defun org-mukey-make-heading-list ()
  "DOCSTRING"
  (save-excursion)
  (goto-char (point-max))
  (cl-loop while (re-search-backward org-heading-regexp nil t)
           collect (cons (point) (org-current-level))))

(defun org-mukey-make-heading-begin ()
  (reverse
   (mapcar 'car (org-mukey-make-heading-list))))

(defun org-mukey-make-heading-end ()
  (reverse
   (mapcar (lambda (cons)
             (+ (car cons) (cdr cons)))
           (org-mukey-make-heading-list))))

;;; timestamp class
(defclass org-mukey-source-timestamp (org-mukey-source)
  ((beg-list :initform 'org-mukey-make-timestamp-begin)
   (end-list :initform 'org-mukey-make-timestamp-end)
   (keymap :initform org-mukey-timestamp-map)
   (parse-function :initform 'org-mukey-timestamp-refresh)))

(defun org-mukey-make-timestamp-pos-list (list type)
  "DOCSTRING"
  `(,@(org-element-map list 'timestamp
        (lambda (hl) (org-element-property type hl) ))
    ,@(org-element-map (org-element-map list 'headline
                         (lambda (hl)
                           (org-element-property :deadline hl) ) ) 'timestamp
        (lambda (hl) (org-element-property type hl) ))
    ,@(org-element-map (org-element-map list 'headline
                         (lambda (hl)
                           (org-element-property :scheduled hl) ) ) 'timestamp
        (lambda (hl) (org-element-property type hl) ))
    ,@(org-element-map (org-element-map list 'headline
                         (lambda (hl)
                           (org-element-property :closed hl) ) ) 'timestamp
        (lambda (hl) (org-element-property type hl)))))

(defun org-mukey-make-timestamp-begin ()
    (let ((lst (org-element-parse-buffer)))
      (org-mukey-make-timestamp-pos-list lst :begin)))

(defun org-mukey-make-timestamp-end ()
    (let ((lst (org-element-parse-buffer)))
  (org-mukey-make-timestamp-pos-list lst :end)))

(defun org-mukey-heading-refresh (beg end len)
  "DOCSTRING"
  beg end len ;dummy
  (save-excursion
    (when (progn
            (beginning-of-line)
            (outline-on-heading-p t))
      (org-mukey-make-overlay
       (lambda () (point))
       (lambda () (re-search-forward "\\*+" nil t))
       'org-mukey-heading-map))))

(defun org-mukey-timestamp-refresh (beg end len)
  "DOCSTRING"
  beg end len ;dummy
  (save-excursion
    (when (org-at-timestamp-p t)
      (goto-char (1-(point)))
      (org-mukey-make-overlay
       (lambda () (re-search-forward "[]>]" nil t))
       (lambda () (re-search-backward "[[<]" nil t))
       'org-mukey-timestamp-map))))

;;; priority class
(defclass org-mukey-source-priority (org-mukey-source)
  ((beg-list :initform 'org-mukey-make-priority-begin)
   (end-list :initform 'org-mukey-make-priority-end)
   (keymap :initform org-mukey-priority-map)
   (parse-function :initform 'org-mukey-priority-refresh)))

(defun org-mukey-make-priority-begin ()
  "DOCSTRING"
  (save-excursion)
  (goto-char (point-max))
  (reverse
   (cl-loop while (re-search-backward org-priority-regexp nil t)
            collect (point))))

(defun org-mukey-make-priority-end ()
  "DOCSTRING"
  (save-excursion)
  (goto-char (point-min))
  (cl-loop while (re-search-forward org-priority-regexp nil t)
           collect (1-(point))))

(defun org-mukey-priority-refresh (beg end len)
  "DOCSTRING"
  beg end len ;dummy
  (save-excursion
    (re-search-backward ".*?\\(\\[ ?\\)" nil t)
    (when (looking-at org-priority-regexp)
      (org-mukey-make-overlay
       (lambda () (re-search-forward "[]]" nil t))
       (lambda () (re-search-backward "[[]" nil t))
       'org-mukey-priority-map))))

;;;###autoload
(define-minor-mode org-multiple-keymap-minor-mode
  "Toggle `org-multiple-keymap-minor-mode'.
With a prefix argument ARG, enable `org-multiple-keymap-minor-mode' if
ARG is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Key bindings (heading):
\\{org-mukey-heading-map}

Key bindings (timestamp):
\\{org-mukey-timestamp-map}

Key bindings (priority):
\\{org-mukey-priority-map}"
  :lighter ""
  (if org-multiple-keymap-minor-mode
      (org-mukey-set-keymap)
    (remove-overlays nil nil 'org-mukey-ov t)
    (dolist (source org-mukey-source-list)
      (remove-hook 'after-change-functions
                   (org-mukey-source-get-parsefunc (make-instance source)) t))))

(defun org-mukey-set-keymap ()
  (interactive)
  (save-excursion
    (dolist (source org-mukey-source-list)
      (let* ((ins (make-instance source))
             (map (org-mukey-source-get-keymap ins)))
        (cl-loop for begin in (funcall (org-mukey-source-get-beg ins))
                 for end in (funcall (org-mukey-source-get-end ins))
                 do
                 (org-mukey-make-overlay begin end map))
        (add-hook 'after-change-functions (org-mukey-source-get-parsefunc ins) nil t)))))

(defun org-mukey-make-overlay (beg end key)
  (let ((ov (make-overlay 
              (if (functionp beg) (funcall beg) beg)
              (if (functionp end) (funcall end) end))))
    (overlay-put ov 'face 'highlight)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'keymap (eval key))
    (overlay-put ov 'org-mukey-ov t)))

(defmacro org-mukey-make-function (fun)
  "Create a function for update overlay."
  (let ((fun-name (concat "org-mukey-" (symbol-name fun))))
    (list
     'progn
     `(defun ,(intern fun-name) ()
        ,(concat "Put overlay after `" (symbol-name fun) "'.")
        (interactive)
        (,fun)
        (save-excursion
          (let ((ov (make-overlay
                     (re-search-forward "[]>]" nil t)
                     (re-search-backward "[[<]" nil t))))
            (overlay-put ov 'face 'highlight)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'keymap org-mukey-timestamp-map)
            (overlay-put ov 'org-mukey-ov t)))))))

(org-mukey-make-function org-timestamp-up)
(org-mukey-make-function org-timestamp-down)
(org-mukey-make-function org-timestamp-up-day)
(org-mukey-make-function org-timestamp-down-day)

(provide 'org-multiple-keymap)
;;; org-multiple-keymap.el ends here
