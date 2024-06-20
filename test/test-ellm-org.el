;;; test-ellm-org.el --- Tests for the `ellm-org' module -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jean-Francois Arbour
;;
;; Author: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Created: May 15, 2024
;; Modified: May 15, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jfa/test-ellm-org
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for the `ellm-org' module
;;
;;; Code:

(require 'ert)
(require 'ellm)

(ert-deftest ellm-org-test--single-user-message-at-point ()
  (with-temp-buffer
    (org-mode)
    (insert "** User\nExample user message.")
    (goto-char (point-min))
    (let ((expected `((role . :user) (content . "Example user message.")))
          (actual (ellm-org--message-at-point)))
      (should (equal expected actual)))))


(ert-deftest ellm-org-test--single-assistant-message-at-point-with-property-drawer ()
    (with-temp-buffer
      (org-mode)
      (insert "** Assistant\n:PROPERTIES:\n:CUSTOM_ID: assistant\n:END:\nExample assistant message.")
      (goto-char (point-min))
      (let ((expected `((role . :assistant) (content . "Example assistant message.")))
            (actual (ellm-org--message-at-point)))
        (should (equal actual expected)))))

(ert-deftest ellm-org-test--get-conversation-messages ()
  (let ((test-data `(("data/conversation-1.org" . ,ellm-org-test--conversation-messages-1)
                     ("data/conversation-2.org" . ,ellm-org-test--conversation-messages-2)
                     ("data/conversation-3.org" . ,ellm-org-test--conversation-messages-3))))
    (dolist (data test-data)
      (with-current-buffer (find-file-noselect (car data))
        (should (messages-equal-p
                 (ellm-org--get-conversation-messages (point-min))
                 (cdr data)))))))

(defun messages-equal-p (msgs1 msgs2)
  "Return t if MSGS1 and MSGS2 are equal, nil otherwise."
  (and (equal (length msgs1) (length msgs2))
       (cl-every (lambda (msg1 msg2)
                   (and (eq (alist-get 'role msg1) (alist-get 'role msg2))
                        (string= (s-trim (alist-get 'content msg1)) (s-trim (alist-get 'content msg2)))))
                 msgs1 msgs2)))

(defvar ellm-org-test--conversation-messages-1
  (list
   `((role . :user)
     (content . "*** CONTEXT:
:PROPERTIES:
:CUSTOM_ID: context
:END:
#+begin_src org
,** User
What is the issue with this code?
,#+begin_src python
n = 0
while n < 10:
  if n % 10 == 0:
    n -= 1
  n += 1
,#+end_src

,** Assistant
The while loop never breaks so the program will hang.
#+end_src

*** PROMPT:
:PROPERTIES:
:CUSTOM_ID: prompt
:END:
I don't understand this, what is the syntax for a for loop in Python instead?"))
    `((role . :assistant) (content . "No worries! You can use a for loop instead. The basic syntax of a for loop in Python is as follows:
#+begin_src python
for var in iterable:
  # Do something
#+end_src"))
    `((role . :user) (content . "Thanks! And how about in C?"))
    `((role . :assistant) (content . "In C, the for loop syntax is slightly different:
#+begin_src c
for (initialization; condition; increment) {
  // Do something
}
#+end_src")))
  "A string containing a few conversations for testing purposes.")

(defvar ellm-org-test--conversation-messages-2
  (list
   `((role . :user)
     (content . "Can you explain the components of an HTTP request?"))
   `((role . :assistant)
     (content . "Certainly! An HTTP request consists of:

1. A request line
   - Method
   - Request-URI
   - HTTP version
2. Headers
   a. Content-Type
   b. Content-Length
   c. Others...
3. An empty line
4. Optionally, a message body"))
   `((role . :user)
     (content . "What about HTTP response?"))
   `((role . :assistant)
     (content . "An HTTP response contains:

1. A status line
   - HTTP version
   - Status code
   - Reason phrase
2. Headers
3. An empty line
4. An optional message body")))
  "A string containing a few conversations for testing purposes.")

(defvar ellm-org-test--conversation-messages-3
  (list
   `((role . :user)
     (content . "What are some common HTTP status codes?"))
   `((role . :assistant)
     (content . "Here are a few common HTTP status codes:

| Code | Reason             | Description                               |
|------+--------------------+-------------------------------------------|
| 200  | OK                 | The request has succeeded.                |
| 404  | Not Found          | The server can't find the requested URL.  |
| 500  | Internal Server Error | The server encountered an unexpected condition. |

For more information, visit [[https://developer.mozilla.org/en-US/docs/Web/HTTP/Status][MDN Web Docs]]."))
   `((role . :user)
     (content . "Are there any codes for redirection?"))
   `((role . :assistant)
     (content . "Yes, there are several redirection-related codes. Here are a few:

| Code | Reason               | Description                                           |
|------+----------------------+-------------------------------------------------------|
| 301  | Moved Permanently    | This and all future requests should be directed to the given URI. |
| 302  | Found (Temporary Redirect) | The URI of the requested resource has been changed temporarily. |")))
  "A string containing a few conversations for testing purposes.")

;;; ellm-org-test.el ends here
