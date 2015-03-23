(require 'ert)
(require 'undercover)
(require 'cl-lib)

(undercover "org-multiple-keymap.el")

(require 'org-multiple-keymap)

(defun org-multiple-keymap-test-setup-buffer (name)
  (let ((buf (get-buffer-create name)))
    (set-buffer buf)
    (erase-buffer)
    (when (not (eq major-mode 'org-mode))
      (org-mode))
    (insert "* TEST
  This is org-multiple-keymap test.
* WAITING [#D] Test1   :@work: 
  SCHEDULED: <2015-03-22 日>

** Subtree [#A]
    <2015-03-22 日>
  
* TODO [#D] Test2      :@work:
    DEADLINE: <2015-02-07 土 +1m>
    This is note.
")))

(ert-deftest org-mukey-make-heading-alist-test ()
  (setq expected '((143 . 144) (105 . 107) (44 . 45) (1 . 2)))
  (org-multiple-keymap-test-setup-buffer "*test*")
  (setq actual (org-mukey-make-heading-alist))
  (should (equal expected actual)))

(ert-deftest org-mukey-make-timestamp-alist-test ()
  (setq expected (if (string= (substring (org-version) 0 3) "8.3")
                     '((89 . 103) (125 . 139) (188 . 206))
                   '((90 . 102) (126 . 138) (189 . 201))))
    (org-multiple-keymap-test-setup-buffer "*test*")
    (setq actual (org-mukey-make-timestamp-alist))
    (should (equal expected actual)))

(ert-deftest org-mukey-make-priority-alist-test ()
  (setq expected '((54 . 58) (116 . 119) (150 . 154)))
  (org-multiple-keymap-test-setup-buffer "*test*")
  (setq actual (org-mukey-make-priority-alist))
  (should (equal expected actual)))

(ert-deftest org-mukey-heading-refresh-test ()
  (org-multiple-keymap-test-setup-buffer "*test*")
  (goto-char 1)
  (org-mukey-heading-refresh 1 1 1)
  (should (eq (cl-caadr (overlay-get  (car (overlays-at (point))) 'keymap)) 117))
  (goto-char 106)
  (org-mukey-heading-refresh 1 1 1)
  (should (eq (cl-caadr (overlay-get  (car (overlays-at (point))) 'keymap)) 117))
  (goto-char 143)
  (org-mukey-heading-refresh 1 1 1)
  (should (eq (cl-caadr (overlay-get  (car (overlays-at (point))) 'keymap)) 117)))

(ert-deftest org-mukey-timestamp-refresh-test ()
  (org-multiple-keymap-test-setup-buffer "*test*")
  (goto-char 98)
  (org-mukey-timestamp-refresh 1 1 1)
  (should (eq (cl-caadr (overlay-get  (car (overlays-at (point))) 'keymap)) 111))
  (goto-char 126)
  (org-mukey-timestamp-refresh 1 1 1)
  (should (eq (cl-caadr (overlay-get  (car (overlays-at (point))) 'keymap)) 111))
  (goto-char 205)
  (org-mukey-timestamp-refresh 1 1 1)
  (should (eq (cl-caadr (overlay-get  (car (overlays-at (point))) 'keymap)) 111)))
  
(ert-deftest org-mukey-set-keymap-test ()
  (org-multiple-keymap-test-setup-buffer "*test*")
  (org-mukey-set-keymap)
  (should  (member 'org-mukey-heading-refresh after-change-functions))
  (should  (member 'org-mukey-timestamp-refresh after-change-functions))
  (should  (member 'org-mukey-priority-refresh after-change-functions)))

(ert-deftest org-multiple-keymap-minor-mode-test ()
  (org-multiple-keymap-test-setup-buffer "*test*")
  (org-multiple-keymap-minor-mode t)
  (should (member 'org-mukey-heading-refresh after-change-functions))
  (should (member 'org-mukey-timestamp-refresh after-change-functions))
  (should (member 'org-mukey-priority-refresh after-change-functions))
  (org-multiple-keymap-minor-mode -1)
  (should (not (member 'org-mukey-heading-refresh after-change-functions)))
  (should (not (member 'org-mukey-timestamp-refresh after-change-functions)))
  (should (not (member 'org-mukey-priority-refresh after-change-functions))))

(ert-deftest org-mukey-todo-done-test ()
  (org-multiple-keymap-test-setup-buffer "*test*")
  (goto-char 147)
  (org-mukey-todo-done)
  (buffer-substring 145 149)
  (should (string= (buffer-substring 145 149) "TODO")))
