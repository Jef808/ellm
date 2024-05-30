;;; test-messages-creation.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jean-Francois Arbour
;;
;; Author: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Created: May 24, 2024
;; Modified: May 24, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jfa/test-messages-creation
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ert)
(require 'ellm)

(ert-deftest test-ellm-message-creation ()
  (let ((msg (ellm-system-message-create :content "System message" :name "example_assistant")))
    (should (equal (ellm-message-content msg) "System message"))
    (should (equal (ellm-system-message-name msg) "System"))
    (should (equal (ellm-message-role msg) :system))))

;;; test-messages-creation.el ends here
