;;; org-multiple-keymap.el --- Set keymap to elements, such as timestamp and priority. -*- lexical-binding: t -*-

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/org-multiple-keymap.el
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2015 myuhe all rights reserved.
;; Created: :15-03-15
;; Package-Requires: ((org "8.2.4"))
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
                 (overlay-put ov 'org-mukey-timestamp-ov t)))))))
        
(org-mukey-make-function org-timestamp-up)
(org-mukey-make-function org-timestamp-down)
(org-mukey-make-function org-timestamp-up-day)
(org-mukey-make-function org-timestamp-down-day)

(define-minor-mode org-multiple-keymap-minor-mode
  "Toggle `org-multiple-keymap-minor-mode'.
With a prefix argument ARG, enable `org-multiple-keymap-minor-mode' if
ARG is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Key bindings (per timestamp):
\\{org-mukey-timestamp-map}

Key bindings (per priority):
\\{org-mukey-priority-map}"
  :lighter ""
  (if org-multiple-keymap-minor-mode
        (org-mukey-set-keymap)
    (remove-overlays nil nil 'org-mukey-timestamp-ov t)
    (remove-overlays nil nil 'org-mukey-priority-ov t)))

(defun org-mukey-set-keymap ()
  (interactive)
  (let ((lst (org-element-parse-buffer)))
    (save-excursion
      (cl-loop for begin in (org-mukey-timestamp-pos-list lst :begin)
               for end in (org-mukey-timestamp-pos-list lst :end)
               do
               (let ((ov (make-overlay begin end)))
                 (overlay-put ov 'face 'highlight)
                 (overlay-put ov 'evaporate t)
                 (overlay-put ov 'keymap org-mukey-timestamp-map)
                 (overlay-put ov 'org-mukey-timestamp-ov t)))
      (cl-loop for begin in (org-mukey-make-prior-begin)
               for end in (org-mukey-make-prior-end)
               do
               (let ((ov (make-overlay begin end)))
                 (overlay-put ov 'face 'highlight)
                 (overlay-put ov 'evaporate t)
                 (overlay-put ov 'keymap org-mukey-priority-map)
                 (overlay-put ov 'org-mukey-priority-ov t))))))

(defun org-mukey-timestamp-pos-list (list type)
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
        (lambda (hl) (org-element-property type hl) ))))


(defun org-mukey-make-prior-begin ()
  "DOCSTRING"
  (save-excursion)
  (goto-char (point-max))
  (reverse
  (cl-loop while (re-search-backward org-priority-regexp nil t)
           collect (point))))

(defun org-mukey-make-prior-end ()
  "DOCSTRING"
  (save-excursion)
  (goto-char (point-min))
   (cl-loop while (re-search-forward org-priority-regexp nil t)
           collect (1-(point))))
  
(provide 'org-multiple-keymap)
;;; org-multiple-keymap.el ends here
