;;; test-context-from-region.el --- Tests for ellm -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jean-Francois Arbour
;;
;; Author: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Created: May 11, 2024
;; Modified: May 11, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/Jef808/ellm
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for ellm
;;
;;; Code:

(require 'ert)
(require 'ellm)

(ert-deftest ellm-test--add-context-from-region-no-region ()
  "Test `ellm--add-context-from-region` without an active region."
  (with-temp-buffer
    (insert "Some text that won't be included as context.")
    (let ((prompt "A prompt."))
      (should (equal (ellm--add-context-from-region prompt)
                     prompt)))))

(ert-deftest ellm-test--add-context-from-region-active-region-non-org-mode ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "Some elisp code")
    (set-mark (point-min))
    (goto-char (point-max))
    (let* ((prompt "A prompt.")
           (expected-result (format ellm-prompt-context-fmt-string "emacs-lisp" "Some elisp code" prompt)))
      (should (equal (ellm--add-context-from-region prompt) expected-result)))))

(ert-deftest ellm-test--add-context-from-region-active-region-org-mode-not-in-src-block ()
  (with-temp-buffer
    (org-mode)
    (insert "Some text in org mode")
    (set-mark (point-min))
    (goto-char (point-max))
    (let* ((prompt "A prompt.")
           (expected-result (format ellm-prompt-context-fmt-string "org" "Some text in org mode" prompt)))
      (should (equal (ellm--add-context-from-region prompt) expected-result)))))

(ert-deftest ellm-test--add-context-from-region-active-region-org-mode-in-src-block ()
  (with-temp-buffer
    (org-mode)
    (let* ((code-content "print('Hello, world!')")
           (content (concat "#+begin_src python\n" code-content "\n#+end_src\n\nSome text")))
      (insert content)
      (goto-char (point-min))
      (search-forward "python\n")
      (set-mark (point))
      (search-forward "world!')")
      (let* ((prompt "A prompt.")
             (expected-result (format ellm-prompt-context-fmt-string "python" code-content prompt)))
        (should (equal (ellm--add-context-from-region prompt) expected-result))))))

(ert-deftest ellm-test--add-context-from-region-active-region-org-mode-overlapping-src-block ()
  (with-temp-buffer
    (org-mode)
    (let ((content "#+BEGIN_SRC python\nprint('Hello, world!')\n#+END_SRC\n\nSome text"))
      (insert content)
      (set-mark (point-min))
      (goto-char (point-max))
      (let* ((prompt "A prompt.")
             (expected-result (format ellm-prompt-context-fmt-string "org" content prompt)))
        (should (equal (ellm--add-context-from-region prompt) expected-result))))))

;;; test-context-from-region.el ends here
