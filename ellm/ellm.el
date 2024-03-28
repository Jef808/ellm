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

(defcustom ellm--openai-models-alist '((big . "gpt-4-turbo-preview")
                                       (medium . "gpt-3.5-turbo")
                                       (small . "gpt-3.5-turbo"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm--anthropic-models-alist '((big . "claude-3-opus-20240229")
                                          (medium . "claude-3-sonnet-20240229")
                                          (small . "claude-3-haiku-20240307"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm-default-prompt-params '((provider "openai"
                                         model :big
                                         temperature 0.2
                                         max-tokens 800))
  "The default configuration to use when making a prompt."
  :type 'list
  :group 'ellm)

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
Your goal is to execute the user's task using any CONTEXT or additional instruction they provide."
  "The system message to set the stage for new conversations."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-context-fmt-string "Carefully analyse the following block of code, I will give you a task which should be considered to be in the CONTEXT of that block.\n\n```%s\n%s\n```\n"
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

(defun ellm--make-prompt-maybe-contextual (prompt)
  "Use the active region if any to enhance the PROMPT with context.

If a region is marked, use the `ellm-context-fmt-string' template
to add context from that region. In that case, the current buffer's
major mode is used according to `ellm--major-mode-to-org-lang-alist'
for determining the language with which to format the context."
 (concat (when (use-region-p)
            (format ellm-prompt-context-fmt-string
                    (or (cdr (assoc major-mode ellm--major-mode-to-org-lang-alist)) "text")
                    (buffer-substring-no-properties (region-beginning) (region-end))))
          prompt))

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

(defun ellm--make-message (role content)
  "Create a message with the given ROLE and CONTENT."
  `(("role" . ,role)
    ("content" . ,content)))

(defun ellm--update-current-conversation (prompt)
  "Create the messages to send to the LLM api from the PROMPT string.
This creates a new user message from the prompt and
adds it to `ellm--current-conversation'.
In case `ellm--current-conversation' is nil, first initialize it with the
system message gotten with `ellm--make-prompt-maybe-contextual'."
  (unless ellm--current-conversation
      (progn
        (ellm--add-message "system" ellm-system-message)
        (ellm--add-message "user" (ellm--make-prompt-maybe-contextual prompt)))
    (ellm--add-message "user" prompt)))

(defun ellm--make-request-body (messages max-tokens temperature model)
  "Create the request object which will be sent to OpenAI.
Set the MESSAGES, MAX-TOKENS, TEMPERATURE and MODEL to be used."
  `(("model" . ,model)
    ("messages" . ,messages)
    ("temperature" . ,temperature)
    ("max_tokens" . ,max-tokens)))

(defun ellm--get-last-session-id ()
  "Get the session ID from the last conversation."
  ;; TODO
  )

(defun ellm--load-session (session-id)
  "Load the session with the given SESSION-ID."
  ;; TODO
  )

(defun ellm-chat (prompt &optional max-tokens temperature model)
  "Send the PROMPT to OpenAI using the marked region as context.

Optionally set the MAX-TOKENS, TEMPERATURE and MODEL to be used."
  (interactive "sEnter your prompt: ")
  (let* ((effective-model (or model ellm-default-model))
         (effective-temperature (or temperature ellm-default-temperature))
         (effective-max-tokens (or max-tokens ellm-default-max-tokens))
         (messages (list (ellm--make-message "system" ellm-system-message)
                         (ellm--make-message "user" (ellm--make-prompt-maybe-contextual prompt))))
         (request-body (json-encode (ellm--make-request-body
                                     messages
                                     effective-max-tokens
                                     effective-temperature
                                     effective-model)))
         (bearer-token (concat "Bearer " (ellm-get-openai-api-key)))
         (request-headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,bearer-token))))
    (unless (not ellm-debug-mode) (ellm--log request-body "REQUEST"))
    (let ((url-request-method "POST")
          (url-request-extra-headers request-headers)
          (url-request-data request-body))
      ;; Debug: Print the headers to *Messages* buffer for inspection.
      (url-retrieve ellm--openai-api-url #'ellm--handle-response
                    (list messages
                          effective-max-tokens
                          effective-temperature
                          effective-model)))))

(defun ellm--extract-response-content (response-body)
  "Extract the text from the json RESPONSE-BODY.

This function is meant to be used with the response from the OpenAI API."
  (or
   (let* ((parsed-json (json-parse-string response-body))
          (choices (gethash "choices" parsed-json))
          ;; Note: `aref` is used to access elements of vectors (arrays) in Elisp.
          (first-choice (aref choices 0))
          (message (gethash "message" first-choice))
          (content (gethash "content" message))
          (_ (unless (not ellm-debug-mode) (ellm--log response-body "RESPONSE"))))
     ;; ;; Unescape doubly-quoted strings in the response text
     (replace-regexp-in-string "\\\\\"" "\"" content))
     (ellm--log response-body "JSON-ERROR")))

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

    ;; Unescape doubly-quoted strings in the response text and
    ;; return the converted string
    (replace-regexp-in-string "\\\\\"" "\"" (buffer-string))
    ;; (buffer-string))
    ))

(defun ellm--handle-response (status messages max-tokens temperature model)
  "Handle response. Information about the request is contained in STATUS.
Optionally, include the original PROMPT for visualizing the whole conversation.
You can also include the CONTEXT of the prompt for the response handler."
  ;; Check for error in the response
  (when (plist-get status :error)
    (ellm--log (plist-get status :error) "HTTP-ERROR"))
  (when (not messages)
    (ellm--log (buffer-substring-no-properties (point) (point-max)) "NO-MESSAGES-IN-HANDLER"))
  (goto-char url-http-end-of-headers)
  (let* ((response (ellm--extract-response-content (buffer-substring-no-properties (point) (point-max))))
         (response-content (ellm--markdown-to-org response))
         ;(context (ellm--extract-system-message-content messages))
         ;(prompt (ellm--extract-last-message-content messages))
         (conversations-buffer  (find-file-noselect "~/.llm/conversations.org")))
    (push (ellm--make-message "assistant" response-content) messages)
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
       (org-id-get-create)
       (org-set-property "Timestamp" (format-time-string "%Y-%m-%d %H:%M:%S"))
       (org-set-property "Model" model)
       (org-set-property "Temperature" (number-to-string temperature))
       (org-set-property "Max-tokens" (number-to-string max-tokens))
       ;; (when context
       ;;   (org-insert-heading-respect-content)
       ;;   (insert "Context\n" context))
       (mapc
        (lambda (message)
          (let ((role (cdr (assoc "role" message)))
                (content (cdr (assoc "content" message))))
            (org-insert-heading)
            (insert (if (equal role "system") "Instructions\n" (concat role "\n")))
            (if (equal role "user")
              (org-insert-structure-template
               (concat "QUOTE\n" content))
              (insert content)))) (reverse messages))
       ;; Save the buffer to a file
       (save-buffer)
       (goto-char (point-min))
       (org-goto-first-child)
       ;; Finally, display the buffer with the response
       (display-buffer conversations-buffer))
     (ignore-errors (ellm--log response "ORG-INSERT-ERROR"))))

(defun ellm--retrieve-conversation-data (&optional conversation-id)
  "Retrieve conversation data for a given CONVERSATION-ID from conversations.org.

When no CONVERSATION-ID is provided, retrieve the data for the last entry."
  (with-current-buffer (find-file-noselect "~/.llm/conversations.org")
    (goto-char (point-min)) ; Start search from the beginning of the buffer
    (if (not conversation-id)
        ;; If no conversation ID is provided, retrieve the last entry
        (org-goto-first-child)
      (let ((found nil)
            (data nil))
        ;; If a conversation ID is provided, search for it
        (goto-char (point-min))
        ;; Search for the heading with the specified ID
        (while (and (not found)
                    (re-search-forward "^:ID:\s-+\(.*?\)$" nil t))
          (let ((id (match-string 1)))
            (when (string= id conversation-id)
              (setq found t))))
        ;; Once found, collect the required properties
        '((timestamp . (org-entry-get (point) "Timestamp"))
          (model . (org-entry-get (point) "Model"))
          (temperature . (org-entry-get (point) "Temperature"))
          (max-tokens . (org-entry-get (point) "Max-tokens")))))))

(defun ellm--org-insert-conversation (message)
  "Insert the MESSAGE into the conversations buffer.")

(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ; n") #'ellm-chat)
            (define-key map (kbd "C-c ; k") #'ellm-clear-current-conversation)
            map)
  (if ellm-mode
      (message "ELLM mode enabled")
    (message "ELLM mode disabled")))

(provide 'ellm)
;;; ellm.el ends here
