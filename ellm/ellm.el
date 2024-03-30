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

(defcustom ellm--conversations-file "~/.llm/conversations.org"
  "The file to store conversation history."
  :type 'string
  :group 'ellm)

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

(defcustom ellm-default-max-tokens 1000
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

(defcustom ellm-prompt-context-fmt-string "Here is the CONTEXT of your task:\n\n```%s\n%s\n```\n"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--make-context-message' for usage details."
  :type 'string
  :group 'ellm)

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

(defun ellm--extract-message-contents (message-alist)
  "Extract the contents of the messages from the MESSAGE-ALIST."
  (mapcar (lambda (message) (cdr (assoc "content" message))) message-alist))

(defun ellm--make-message (role content)
  "Create a message with the given ROLE and CONTENT."
  `(("role" . ,role)
    ("content" . ,content)))

(defun ellm--make-request-body (messages max-tokens temperature model)
  "Create the request object which will be sent to OpenAI.
Set the MESSAGES, MAX-TOKENS, TEMPERATURE and MODEL to be used."
  `(("model" . ,model)
    ("messages" . ,messages)
    ("temperature" . ,temperature)
    ("max_tokens" . ,max-tokens)))

(defun ellm-test (prefix prompt &optional max-tokens temperatur model)
  (interactive "P
sEnter your prompt: "
               (message (prefix-numeric-value))))

(defun ellm-chat (prompt &optional max-tokens temperature model)
  "Send the PROMPT to OpenAI using the marked region as context.

Optionally set the MAX-TOKENS, TEMPERATURE and MODEL to be used."
  (interactive "sEnter your prompt: ")
  (let* ((effective-model (or model ellm-default-model))
         (effective-temperature (or temperature ellm-default-temperature))
         (effective-max-tokens (or max-tokens ellm-default-max-tokens))
         (messages (list (ellm--make-message "user" (ellm--make-prompt-maybe-contextual prompt))
                         (ellm--make-message "system" ellm-system-message)))
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
    (buffer-string)))
    ;; (replace-regexp-in-string "\\\\\"" "\"" (buffer-string))))

(defun ellm--insert-response-into-org (messages model temperature max-tokens)
  "Insert the response along with conversation metadata into the org file.

Pass the response data as MESSAGES, MODEL, TEMPERATURE and MAX-TOKENS."
  (let ((conversations-buffer  (find-file-noselect ellm--conversations-file))
        (insertion-success nil)) ; Initialize a flag to track message insertion success
    (with-current-buffer conversations-buffer
      (org-mode)
      (org-fold-hide-sublevels 1)
      (goto-char (point-min))
      (org-insert-heading)
      (insert "Conversation " (current-time-string))
      (org-id-get-create)
      (org-set-property "Timestamp" (format-time-string "%Y-%m-%d %H:%M:%S"))
      (org-set-property "Model" model)
      (org-set-property "Temperature" (number-to-string temperature))
      (org-set-property "Max-tokens" (number-to-string max-tokens))
      (mapc
       #'(lambda (message)
           (let ((role (cdr (assoc "role" message)))
                 (content (cdr (assoc "content" message))))
             (org-insert-heading nil nil t)
             (org-demote)
             (insert (if (equal role "system") "Instructions\n" (concat role)))
             (when (equal role "user")
               (org-insert-structure-template
                (concat "QUOTE\n" content)))
             (when (equal role "assistant")
               (insert (concat "\n" content)))
             (setq insertion-success t))) ; Set the flag to true if messages are processed
       (reverse messages))
      (save-buffer)
      (goto-char (point-min))
      (org-goto-first-child)
      (display-buffer conversations-buffer))
    (unless insertion-success ; Check if the insertion was not successful
      (ignore-errors (ellm--log "ERROR inserting response into org file" "ERROR")))))

(defun ellm--handle-response (status messages max-tokens temperature model)
  "Handle response. Information about the request is contained in STATUS.

Include the MESSAGES, MAX-TOKENS, TEMPERATURE, and MODEL
so that we can persist the state of the conversation."
  ;; Check for error in the response
  (when (plist-get status :error)
    (ellm--log (plist-get status :error) "HTTP-ERROR"))
  (when (not messages)
    (ellm--log (buffer-substring-no-properties (point) (point-max)) "NO-MESSAGES-IN-HANDLER"))
  (goto-char url-http-end-of-headers)
  (let* ((response (ellm--extract-response-content (buffer-substring-no-properties (point) (point-max))))
         (response-content (ellm--markdown-to-org response)))
    (push (ellm--make-message "assistant" response-content) messages)
    ;; Call the new function to insert the response into the org file
    (ellm--insert-response-into-org messages model temperature max-tokens)))

(defun ellm--get-first-heading-id ()
  "Retrieve the ID of the first top-level heading of file at ORG-FILE-PATH."
  (with-current-buffer (find-file-noselect ellm--conversations-file)
    (save-excursion ;; Use save-excursion to preserve the original cursor position.
      (goto-char (point-min))
      (re-search-forward "^\* ")
      (org-entry-get (point) "ID"))))

(defun ellm--retrieve-conversation-by-id (id)
  "Retrieve a conversation by its unique ID along with its properties."
  (with-current-buffer (find-file-noselect ellm--conversations-file)
    (save-excursion
      (goto-char (point-min))
      ;; Search for the ID
      (let ((p (org-id-find id t)))
        (if p
            (progn
              (goto-char p)
              (let ((props (ellm--get-properties id))
                    (messages (progn
                                (ellm--get-messages-from-org-entry))))
                `(("props" . ,props) ("messages" . ,messages))))
          (ellm--log (format "\"Conversation with ID %s not found\"" id) "ERROR"))))))

(defun ellm--get-properties (id)
  "Retrieve and process properties for a given ID."
  (let* ((props-alist (ellm--org-get-property-drawer-alist))
         (props (list
                 (cons "ID" id)
                 (cons "Model" (cdr (assoc "Model" props-alist)))
                 (cons "Temperature" (cdr (assoc "Temperature" props-alist)))
                 (cons "Max-tokens" (cdr (assoc "Max-tokens" props-alist))))))
    props))

(defun ellm--get-messages-from-org-entry ()
  "Retrieve content of all subheadings, prepended with their titles."
  (org-goto-first-child)
  (let ((messages-content '()))
    (while (org-get-next-sibling) ; Move to the next sibling heading
      (let ((element (org-element-at-point)))
        (when (eq (org-element-type element) 'headline)
          ;; Collect the title and content of the subheading
          (let* ((title (org-element-property :title element))
                 (content (org-element-property :contents-begin element))
                 (end (org-element-property :contents-end element))
                 (formatted-title (format "** %s:\n" title))) ; Format the title
            (when (and content end)
              (push (concat formatted-title (buffer-substring-no-properties content end)) messages-content))))))
    (nreverse messages-content)))

(defun ellm--org-get-property-drawer-alist ()
  "Retrieve the property drawer of the current heading as an alist."
  (save-excursion
    (let ((properties nil))
      ;; Ensure we are at the beginning of the heading.
      (org-back-to-heading t)
      ;; Check if there is a property drawer.
      (when (org-get-property-block)
        (let ((drawer-range (org-get-property-block)))
          ;; Go through each line in the property drawer.
          (goto-char (car drawer-range))
          (while (< (point) (cdr drawer-range))
            (when (looking-at org-property-re)
              (let ((key (match-string-no-properties 2))
                    (value (match-string-no-properties 3)))
                ;; Add the property to the alist.
                (push (cons key value) properties)))
            (forward-line))))
      properties)))

(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ; n") #'ellm-chat)
            map)
  (if ellm-mode
      (message "ELLM mode enabled")
    (message "ELLM mode disabled")))

(provide 'ellm)
;;; ellm.el ends here
