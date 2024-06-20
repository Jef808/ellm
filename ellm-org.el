;;; ellm-org.el --- Derived major mode to view the conversations files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jean-Francois Arbour
;;
;; Author: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Created: June 08, 2024
;; Modified: June 08, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jfa/ellm-org
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Derived major mode to view the conversations file
;;
;;; Code:

(require 'org)

(defun ellm-org-next-message ()
  "Move to the next User or Assistant message based on TYPE."
  (interactive)
  (save-restriction
    (save-excursion
      (ellm--goto-conversation-top)
      (org-narrow-to-subtree))
    (let ((message-regex "^\\*\\{1,8\\} +\\(User\\|Assistant\\)")
          match-pos)
      (save-excursion
        (when (looking-at message-regex) (forward-line))
        (setq match-pos (re-search-forward message-regex nil t)))
      (when match-pos
        (goto-char (match-beginning 0))))))

(defun ellm-org-previous-message ()
  "Move to the previous User or Assistant message based on TYPE."
  (interactive)
  (save-restriction
    (save-excursion
      (ellm--goto-conversation-top)
      (org-narrow-to-subtree))
    (let ((regex "^\\*\\{1,8\\} +\\(User\\|Assistant\\)"))
      (when (re-search-backward regex nil t)
        (goto-char (match-beginning 0))))))

(define-derived-mode ellm-org-mode org-mode "ellm-org"
  "Major mode for viewing conversations files.")

(provide 'ellm-org)
;;; ellm-org.el ends here
