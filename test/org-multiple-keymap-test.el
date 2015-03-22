(require 'ert)
(require 'undercover)
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

** Subtree 
    <2015-03-22 日>
  
* TODO [#D] Test2      :@work:
    DEADLINE: <2015-02-07 土 +1m>
    This is note.
")))

(ert-deftest org-mukey-make-heading-alist-test ()
  (setq expected '((139 . 140) (105 . 107) (44 . 45) (1 . 2)))
  (org-multiple-keymap-test-setup-buffer "*test*")
  (setq actual (org-mukey-make-heading-alist))
  (should (equal expected actual)))

(ert-deftest org-mukey-make-timestamp-alist-test ()
  (setq expected (if (string= (substring (org-version) 0 3) "8.3")
            '((89 . 103) (121 . 135) (184 . 202))
            '((90 . 102) (122 . 134) (185 . 197))))
    (org-multiple-keymap-test-setup-buffer "*test*")
    (setq actual (org-mukey-make-timestamp-alist))
    (should (equal expected actual)))


