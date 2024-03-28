;;; ellm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jean-Francois Arbour
;;
;; Author: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-Francois Arbour <jf.arbour@gmail.com>
;; Created: March 25, 2024
;; Modified: March 25, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/Jef808/emacs-llm
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'json)
(require 'url)
(require 'org)

(defgroup ellm nil
  "Make API calls to LLMs."
  :group 'tools
  :prefix "ellm-")

(defun ellm-get-openai-api-key ()
  "A function which retrieves your OpenAI API key."
  (getenv "OPENAI_API_KEY"))

(defconst ellm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defcustom ellm-default-model "gpt-4-turbo-preview"
  "The default model to use when making a prompt."
  :type 'string
  :group 'ellm)

(defcustom ellm-default-temperature 0.2
  "The default temperature to use when making a prompt."
  :type 'float
  :group 'ellm)

(defcustom ellm-default-max-tokens 10
  "The default temperature to use when making a prompt."
  :type 'integer
  :group 'ellm)

(defcustom ellm-time-format-string "%r"
  "The format string to use with `format-time-string' for displaying conversations."
  :type 'string
  :group 'ellm)

(defcustom ellm-debug-mode nil
  "If non nil, log each request and response to the `ellm--log-buffer-name' buffer."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-system-message "Format your answers with proper markdown syntax.
When using headings, always start at depth 3.
Be concise but always provide complete answers. Skip politeness, you are an expert in your field."
  "The system message to set the stage for new conversations."
  :type 'string
  :group 'ellm)

(defcustom ellm-context-fmt-string "In order to undersand the prompt, first consider the following context to understand the prompt:
```%s
%s
```"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--make-context-message' for usage details."
  :type 'string
  :group 'ellm)

(defvar ellm--current-conversation-id nil
  "The ID of the current conversation with the LLM API.")

(defvar ellm--current-conversation nil
  "An alist to store the current conversation history.")

(defconst ellm--log-buffer-name "*ELLM-Logs*"
  "Log buffer for LLM messages.")

(defvar ellm--major-mode-to-org-lang-alist
  '((python-mode . "python")
    (emacs-lisp-mode . "emacs-lisp")
    (org-mode . "org")
    ;; Add more mappings as needed
    )
  "Alist mapping major modes to Org mode source block languages.")

(defun ellm--major-mode-to-org-lang ()
  "Get the Org mode language corresponding to the current buffer's major mode."
  (cdr (assoc major-mode ellm--major-mode-to-org-lang-alist)))

(defun ellm-clear-current-conversation ()
  "Clear the conversation history."
  (interactive)
  (setq ellm--current-conversation nil)
  (ellm--log "\"Conversation history cleared.\"" "INFO"))

(defun ellm--log (data &optional label)
  "Log DATA with optional LABEL.
DATA is a valid json string."
  ;; Ensure the *ELLM-Logs* buffer exists and select it
  (with-current-buffer (get-buffer-create ellm--log-buffer-name)
    ;; Move to the end of the buffer
    (let ((p (goto-char (point-max))))
      ;; Insert the log entry
      (unless (bolp) (insert "\n"))
      (insert (format
               "{\"timestamp\": %S, %S: %s}"
               ;; "{\"timestamp\": %S, %S: %s}"
               (current-time-string)
               (or label "INFO")
               data))
      (json-pretty-print p (point-max)))))

(defun ellm--make-context-from-region ()
  "Create a context message from the selected region, if any.
This function uses the buffer's file extension and the selected
region inserted into the `ellm-context-fmt-string' to create the context."
  (when (use-region-p)
    (format ellm-context-fmt-string
            (or (ellm--major-mode-to-org-lang) "text")
            (buffer-substring-no-properties
             (region-beginning) (region-end)))))

(defun ellm--extract-system-message-content (message-list)
  "Extracts the content of the first message from the MESSAGE-LIST alist."
  (unless (not message-list)
    (let ((first-message (car message-list)))
      (when (equal (cdar first-message) "system")
        (cdadr first-message)))))

(defun ellm--extract-last-message-content (message-list)
  "Extracts the content of the first message from the MESSAGE-LIST alist."
  (unless (not message-list)
    (cdadar (last message-list))))

(defun ellm--extract-message-contents (message-alist)
  "Extract the contents of the messages from the MESSAGE-ALIST."
  (mapcar (lambda (message) (cdr (assoc "content" message))) message-alist))

(defun ellm--add-message (role content)
  "Add message with given ROLE and CONTENT to the conversation history."
  (setq ellm--current-conversation
        (append ellm--current-conversation
                (list `(("role" . ,role) ("content" . ,content))))))

(defun ellm--update-current-conversation (prompt)
  "Create the messages to send to the LLM api from the PROMPT string.
This creates a new user message from the prompt and
adds it to `ellm--current-conversation'.
In case `ellm--current-conversation' is nil, first initialize it with the
system message gotten with `ellm--make-context-from-region'."
  (if ellm--current-conversation
      (progn
        (ellm--add-message "system" ellm-system-message)
        (ellm--add-message "user" (concat (ellm--make-context-from-region) "\n" prompt)))
    (ellm--add-message "user" prompt)))

(defun ellm--make-request-body (messages max-tokens temperature model)
  "Create the request object which will be sent to OpenAI.
Set the MESSAGES, MAX-TOKENS, TEMPERATURE and MODEL to be used."
  `(("model" . ,model)
    ("messages" . ,messages)
    ("temperature" . ,temperature)
    ("max_tokens" . ,max-tokens)))

(defun ellm-chat (prompt &optional max-tokens temperature model)
  "Send the PROMPT to OpenAI using the marked region as context.
Optionally set the MAX-TOKENS, TEMPERATURE and MODEL to be used."
  (interactive "sEnter your prompt:")
  (let* ((effective-model (or model ellm-default-model))
         (effective-temperature (or temperature ellm-default-temperature))
         (effective-max-tokens (or max-tokens ellm-default-max-tokens))
         (messages (ellm--update-current-conversation prompt))
         (request-body (json-encode (ellm--make-request-body
                                     messages
                                     effective-max-tokens
                                     effective-temperature
                                     effective-model))))
    (when ellm-debug-mode (ellm--log request-body "REQUEST"))
    (ellm--openai-request request-body)))

(defun ellm--openai-request (body)
  "Send a request to OpenAI with the given BODY.
Pass the PROMPT to the response handler for visualizing the conversation.
Optionally, include the CONTEXT of the prompt for the response handler."
  (let* ((bearer-token (concat "Bearer " (ellm-get-openai-api-key)))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,bearer-token)))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data body))
    ;; Debug: Print the headers to *Messages* buffer for inspection.
    (message "Got here")
    (url-retrieve ellm--openai-api-url #'ellm--openai-handle-response)))

(defun ellm--extract-response-content (response-body)
  "Extract the text from the json RESPONSE-BODY.
This function is meant to be used with the response from the OpenAI API."
  ;; (ellm--log (format "{\"response body\":%s}" (buffer-substring-no-properties (point) (point-max))))
  ;; Parse the JSON string. The result is a hash table where each key is a string.
  (or
   (let* ((parsed-json (json-parse-string response-body))
          (choices (gethash "choices" parsed-json))
          ;; Note: `aref` is used to access elements of vectors (arrays) in Elisp.
          (first-choice (aref choices 0))
          (message (gethash "message" first-choice))
          (content (gethash "content" message))
          (_ (ellm--log response-body "RESPONSE")))
     ;; Unescape doubly-quoted strings in the response text
     (replace-regexp-in-string "\\\\\"" "\"" content)
     (ellm--log response-body "JSON-ERROR"))))

(defun ellm--markdown-to-org (markdown-string)
  "Convert a MARKDOWN-STRING into an Org-mode formatted string."
  (with-temp-buffer
    (insert markdown-string)
    (goto-char (point-min))

    ;; Convert Markdown headers to Org-mode headers
    (while (re-search-forward "^\\s-*\\(#+\\) \\(.*\\)$" nil t)
      (replace-match (concat (make-string (length (match-string 1)) ?*) " " (match-string 2))))

    ;; Convert Markdown bold and italic to Org-mode syntax
    (goto-char (point-min))
    (while (re-search-forward "\\(\\*\\*\\|__\\)\\(.*?\\)\\(\\*\\*\\|__\\)" nil t)
      (replace-match (concat "*" (match-string 2) "*")))

    ;; Convert Markdown code blocks to Org-mode syntax
    (goto-char (point-min))
    (while (re-search-forward "```\\(.*?\\)\n\\(\\(?:.*\n\\)*?\\)```" nil t)
      (let ((lang (match-string 1))
            (code (match-string 2)))
        (replace-match (concat "#+BEGIN_SRC " lang "\n" code "#+END_SRC\n"))))

    ;; Convert Markdown inline code to Org-mode syntax
    (goto-char (point-min))
    (while (re-search-forward "`\\(.*?\\)`" nil t)
      (replace-match (concat "~" (match-string 1) "~")))

    ;; Convert Markdown links to Org-mode syntax
    (goto-char (point-min))
    (while (re-search-forward "\\[\\(.*?\\)]\\(\\(\\[.*?]\\)\\|\\(([^)]*)\\)\\)" nil t)
      (replace-match (concat "[[" (match-string 4) "][" (match-string 1) "]]")))

    ;; Return the converted string
    (buffer-string)))

(defun ellm--openai-handle-response (status &optional)
  "Handle response. Information about the request is contained in STATUS.
Optionally, include the original PROMPT for visualizing the whole conversation.
You can also include the CONTEXT of the prompt for the response handler."
  ;; Check for error in the response
  (when (plist-get status :error)
    (ellm--log (plist-get status :error) "HTTP-ERROR")
    ;; (error "Request failed: %s" (plist-get status :error))
    )
  (goto-char url-http-end-of-headers)
  (let* ((response (ellm--extract-response-content (buffer-substring-no-properties (point) (point-max))))
         (response-content (ellm--markdown-to-org response))
         (context (ellm--extract-system-message-content ellm--current-conversation))
         (prompt (ellm--extract-last-message-content ellm--current-conversation))
         (conversations-buffer  (find-file-noselect "~/.llm/conversations.org")))
    (ellm--add-message "assistant" response-content)
    ;; (content (ellm--extract-and-wrap-content response)))
    ;; to the dedicated buffer in another window
     (with-current-buffer conversations-buffer
       ;; Optionally, clear the buffer before inserting new content
       (org-mode)
       ;;(erase-buffer)
       (org-fold-hide-sublevels 1)
       (goto-char (point-min))
       ;; Insert the response
       (org-insert-heading)
       (insert "Conversation " (current-time-string))
       (org-insert-subheading (point))
       (when context
         (insert "Context\n" context)
         (org-insert-heading))
       (insert "User")
       (org-insert-structure-template
        (concat "QUOTE\n" prompt))
       (org-insert-heading)
       (insert "Assistant\n" response-content)
       ;; Save the buffer to a file
       (save-buffer)
       (goto-char (point-min))
       (org-goto-first-child)
       ;; Finally, display the buffer with the response
       (display-buffer conversations-buffer))
     ;; (ellm--log response "ORG-INSERT-ERROR")
     ))

(defun ellm-load-conversations ()
  "Load the LLM conversation from a file."
  (interactive)
  (with-current-buffer (find-file-other-window "~/.llm/conversations.org")
    (org-mode)
    (org-fold-hide-sublevels 1)))

(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c /") #'ellm-chat)
            map)
  (if ellm-mode
      (message "ELLM mode enabled")
    (message "ELLM mode disabled")))

(provide 'ellm)
;;; ellm.el ends here
