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
(require 'org-id)
(require 'org-element)
(require 'org-fold)

(require 'markdown-mode)

(defgroup ellm nil
  "Make API calls to LLMs."
  :group 'tools
  :prefix "ellm-")

(defun ellm-get-openai-api-key ()
  "A function which retrieves your OpenAI API key."
  (getenv "OPENAI_API_KEY"))

(defun ellm-get-anthropic-api-key ()
  "A function which retrieves your Anthropic API key."
  (getenv "ANTHROPIC_API_KEY"))

(defconst ellm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--anthropic-api-url "https://api.anthropic.com/v1/messages"
  "The URL to send requests to the OpenAI API.")

(defcustom ellm--conversations-file "~/.llm/conversations.org"
  "The file to store conversation history."
  :type 'string
  :group 'ellm)

(defcustom ellm-provider 'openai
  "Default provider to use for API calls."
  :type '(choice
          (const :tag "OpenAI" openai)
          (const :tag "Anthropic" anthropic))
  :group 'ellm)

(defcustom ellm-model-size 'big
  "Default model size to use for API calls."
  :type '(choice
          (const :tag "Big" big)
          (const :tag "Medium" medium)
          (const :tag "Small" small))
  :group 'ellm)

(defcustom ellm--openai-models-alist `((big . "gpt-4-turbo-preview")
                                       (medium . "gpt-3.5-turbo")
                                       (small . "gpt-3.5-turbo"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm--anthropic-models-alist `((big . "claude-3-opus-20240229")
                                          (medium . "claude-3-sonnet-20240229")
                                          (small . "claude-3-haiku-20240307"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm-default-prompt-params '((provider "openai"
                                         model 'big
                                         temperature 0.2
                                         max-tokens 800))
  "The default configuration to use when making a prompt."
  :type 'list
  :group 'ellm)

(defcustom ellm-default-model "gpt-3.5-turbo" ;; "gpt-4-turbo-preview"
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

(defvar ellm--test-mode nil
  "If non-nil, set LLM parameters to lowest token cost for testing purposes.")

(defvar ellm--mock-response nil
  "If non-nil, use a mock response instead of making an API call.")

(defcustom ellm-system-message "Your goal is to execute the user's task, using **all** \
of the CONTEXT the user provides.
You should also ***always*** include a short summary title \
which describes the discussion for easy navigation through my conversation history. \n \
Separate it by a horizontal rule as follows:

\<Your title here\>

---

\<Your response here\>"
  "The system message to set the stage for new conversations."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-context-fmt-string "Here is the CONTEXT of your task:\n\n```%s\n%s\n```\n\n"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--make-context-message' for usage details."
  :type 'string
  :group 'ellm)

(defconst ellm--log-buffer-name "*ELLM-Logs*"
  "Log buffer for LLM messages.")

(defun ellm-set-provider ()
    "Set the API provider for My-Package."
    (interactive)
    (setq ellm-provider
          (intern (completing-read "Choose provider: " '(openai anthropic))))
    (message "...provider set to %s..." ellm-provider))

(defun ellm-set-model-size ()
    "Set the API provider for My-Package."
    (interactive)
    (setq ellm-model-size
          (intern (completing-read "Choose model size: " '(big medium small))))
    (let ((models-alist (if (eq ellm-provider 'openai)
                            ellm--openai-models-alist
                          ellm--anthropic-models-alist)))
      (setq ellm-default-model (cdr (assoc ellm-model-size models-alist)))
      (message "...model set to %s..." ellm-default-model)))

(defun ellm-set-max-tokens (max-tokens)
  "Set the `MAX-TOKENS' to use for the LLM prompt."
  (interactive "nMax tokens (between 1 and 4096): ")
  (cond ((and (integerp max-tokens)
           (and (>= max-tokens 1) (<= max-tokens 4096)))
         (setq ellm-default-max-tokens max-tokens)
         (message "...max-tokens set to %d..." max-tokens))
        (t (message "Error: Invalid input: %s" max-tokens))))

(defun ellm-set-temperature (temperature)
  "Set the `TEMPERATURE' to use for the LLM prompt."
  (interactive "nTemperature (between 0.0 and 2.0): ")
  (cond ((and (>= temperature 0) (<= temperature 2))
         (setq ellm-default-temperature temperature)
         (message "...temperature set to %.1f..." temperature))
        (t (message "Error: Invalid input: %s" temperature))))

(defun ellm--log (data &optional label should-encode-to-json)
  "Log `DATA' with an optional `LABEL'.
Will call `json-encode' on `DATA' if
`SHOULD-ENCODE-TO-JSON' is set to a non-nil value."
  (let* ((trimmed-data (string-trim data))
         (effective-data (if should-encode-to-json (json-serialize trimmed-data) trimmed-data))
         (formatted-log (format "{\"TIMESTAMP\": \"%s\", \"%s\": %s}"
                                (format-time-string "%Y-%m-%d %H:%M:%S")
                                (or label "INFO")
                                effective-data)))
    (with-current-buffer (get-buffer-create ellm--log-buffer-name)
      (goto-char (point-max))
      (unless (bolp) (newline))
      (let ((pos (point)))
        (insert formatted-log)
        (newline)
        (json-pretty-print pos (point-max))))))

(defun ellm--log-response-error (error)
  "Log the `ERROR' response."
  (ellm--log error "RESPONSE-ERROR" t))

(defun ellm--log-json-error (error)
  "Log the `ERROR' response."
  (ellm--log error "JSON-ERROR" t))

(defun ellm--log-http-error (status)
  "Log HTTP `STATUS' errors."
  (ellm--log (plist-get status :error) "HTTP-ERROR") t)

(defun ellm--log-http-redirect (status)
  "Log HTTP `STATUS' redirects."
  (ellm--log (plist-get status :redirect) "HTTP-REDIRECT") t)

(defun ellm--log-no-response-body ()
  "Log when there is no response body."
  (ellm--log "No response body (DNS resolve or TCP error)" "REQUEST-ERROR" t))

(defun ellm--log-response-format-error (response-content)
  "Log when the `RESPONSE-CONTENT' doesn't split a title."
  (ellm--log (format "No title or no body in the response content:\n%s" response-content)
             "RESPONSE-FORMAT-ERROR" t))

(defun ellm--log-request-body (body)
  "Log the request `BODY'."
  (when ellm-debug-mode (ellm--log body "REQUEST-BODY")))

(defun ellm--log-response-body (response-body)
  "Log the `RESPONSE-BODY'."
  (when ellm-debug-mode (ellm--log response-body "RESPONSE-BODY")))

(defun ellm--log-response-content (response-content)
  "Log the `RESPONSE-CONTENT'."
  (when ellm-debug-mode (ellm--log response-content "RESPONSE-CONTENT" t)))

(defun ellm--log-prompt-data (prompt-data)
  "Log the `PROMPT-DATA'."
  (when ellm-debug-mode (ellm--log prompt-data "PROMPT-DATA" t)))

(defun ellm--log-markdown-messages (markdown-string)
  "Log `MARKDOWN-STRING'."
  (when ellm-debug-mode
    (ellm--log (ellm--markdown-pretty-print markdown-string)
               "MESSAGES-FOR-MARKDOWN" t)))

(defun ellm--log-org-messages (org-string)
  "Log `ORG-STRING'."
  (when ellm-debug-mode
    (ellm--log (ellm--org-pretty-print org-string)
               "MESSAGES-FOR-ORG-MODE" t)))

(defun ellm--org-pretty-print (org-content)
  "Pretty print ORG-CONTENT."
  (with-temp-buffer
    (insert org-content)
    (org-mode)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellm--markdown-pretty-print (markdown-content)
  "Pretty print MARKDOWN-CONTENT."
  (with-temp-buffer
    (insert markdown-content)
    (markdown-mode)
    (let ((fill-column 80))
      (fill-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar ellm--major-mode-to-org-lang-alist
  '((python-mode . "python")
    (emacs-lisp-mode . "emacs-lisp")
    (org-mode . "org"))
  "Alist mapping major modes to Org mode source block languages.")

(defun ellm--make-prompt-maybe-contextual (prompt)
  "Use the active region if any to enhance the `PROMPT' with context.
If a region is marked, use the `ellm-context-fmt-string' template
to add context from that region. In that case, the current buffer's
major mode is used according to `ellm--major-mode-to-org-lang-alist'
for determining the language with which to format the context."
  (concat
   (when (use-region-p)
     (format ellm-prompt-context-fmt-string
             (or (cdr (assoc major-mode ellm--major-mode-to-org-lang-alist)) "text")
             (buffer-substring-no-properties (region-beginning) (region-end))))
   prompt))

(defun ellm--make-message (role content)
  "Create a message with the given ROLE and CONTENT."
  `((role . ,role) (content . ,content)))

(defun ellm--add-user-message (content prompt-data)
  "Add a user message with `CONTENT' to the `PROMPT-DATA'."
  (nconc (cdr (assoc 'messages prompt-data))
         (list (ellm--make-message :user content))))

(defun ellm--add-assistant-message (content prompt-data)
  "Add an assistant message with `CONTENT' to the `PROMPT-DATA'.
We set the title its provide as the new title."
  (let* ((split-content (ellm--separate-response content))
         (title (car split-content))
         (response (cdr split-content)))
    (unless (and title response)
      (ellm--log-response-format-error content))
    (when title
        (setf (alist-get 'title prompt-data nil nil #'eq) title))
    (nconc (cdr (assoc 'messages prompt-data))
           (list (ellm--make-message :assistant (or response content))))))

(defun ellm--get-prompt-config-alist ()
  "Get the current configuration for making an API call."
  `((temperature . ,ellm-default-temperature)
    (max_tokens . ,ellm-default-max-tokens)
    (model . ,ellm-default-model)))

(defun ellm--make-prompt-data-alist (messages &optional max-tokens temperature model)
  "Create the request object which will be sent to the LLM provider's api.
Set the MESSAGES, MAX-TOKENS, TEMPERATURE and MODEL to be used."
  `((messages . ,messages)
    (temperature . ,(or temperature ellm-default-temperature))
    (max_tokens . ,(or max-tokens ellm-default-max-tokens))
    (model . ,(or model ellm-default-model))))

(defun ellm--prepare-messages (prompt)
  "Prepare the messages list with a new user message based on PROMPT."
  (let ((new-user-message (ellm--make-message :user (ellm--make-prompt-maybe-contextual prompt))))
    (list (ellm--make-message :system ellm-system-message) new-user-message)))

(defun ellm--setup-request (prompt-data-alist)
  "Setup the api request headers and body.
The `PROMPT-DATA-ALIST' is as returned by `ellm--make-prompt-data-alist'.
Returns the json encoded request body and the headers,
as a cons cell."
  (let* ((request-body (json-encode prompt-data-alist))
         (bearer-token (concat "Bearer " (ellm-get-openai-api-key)))
         (request-headers `(("Content-Type" . "application/json") ("Authorization" . ,bearer-token))))
    (cons request-body request-headers)))

(defun ellm-chat (prompt &optional max-tokens temperature model)
  "Send the PROMPT to OpenAI using the marked region as context.
Optionally set the MAX-TOKENS, TEMPERATURE, and MODEL to be used."
  (interactive "sEnter your prompt: ")
  (let* ((messages (ellm--prepare-messages prompt))
         (prompt-data (ellm--make-prompt-data-alist
                       messages max-tokens temperature model))
         (request-setup (ellm--setup-request prompt-data))
         (request-body (car request-setup))
         (request-headers (cdr request-setup))
         (url ellm--openai-api-url)
         (url-request-method "POST")
         (url-request-extra-headers request-headers)
         (url-request-data request-body))
    (ellm--log-request-body request-body)
    (url-retrieve url #'ellm--handle-response (list prompt-data))))

(defun ellm--handle-response (status prompt-data)
  "Handle response. Information about the response is contained in STATUS.
The `PROMPT-DATA' alist include the messages, max-tokens, temperature, and model
so that we can persist the state of the conversation."
  (cond ((plist-get status :error)
         (ellm--log-http-error status))
        ((plist-get status :redirect)
         (ellm--log-http-redirect status))
        (t
         (goto-char url-http-end-of-headers)
         (if (<= (point-max) (point))
             (ellm--log-no-response-body)
           (let ((parsed-response (ellm--parse-json-response)))
             ;; (ellm--log parsed-response "PARSED-RESPONSE" t)
             (when parsed-response
               (ellm--add-response-to-conversation parsed-response prompt-data)))))))

(defun ellm--parse-json-response ()
  "Parse the JSON response from the API call."
  (condition-case error
      (let ((response-body
             (string-trim (buffer-substring-no-properties (point) (point-max)))))
        (ellm--log-response-body response-body)
        (json-parse-string response-body))
    (json-parse-error
     (ellm--log-json-error error)
     nil)))

(defun ellm--add-response-to-conversation (parsed-response prompt-data)
  "Process the PARSED-RESPONSE from the API call.
`PROMPT-DATA' includes the messages, max-tokens, temperature, and model"
  (let ((response-content (ellm--extract-response-content-openai parsed-response)))
    ;; (ellm--log-response-content response-content)
    (ellm--add-assistant-message response-content prompt-data)
    ;; (ellm--log-prompt-data prompt-data)
    (ellm--insert-conversation-into-org prompt-data)))

(defun ellm--extract-response-content-openai (response)
  "Extract the text from the json RESPONSE.
This function is meant to be used with the response from the OpenAI API."
  (condition-case error
      (let* ((choices (gethash "choices" response))
             ;; Note: `aref` is used to access elements of vectors (arrays) in Elisp.
             (first-choice (aref choices 0))
             (msg (gethash "message" first-choice)))
        (replace-regexp-in-string "\\\\\"" "\"" (gethash "content" msg)))
    (wrong-number-of-arguments (ellm--log-response-error error))
    ;; (:success (replace-regexp-in-string "\\\\\"" "\"" content))
    ))

(defun ellm--stringify-message (message)
  "Convert the `MESSAGE' to a string.
A message of the form
  `((role . :role) (content . \"content\"))'
is converted to the string
\"# Role
content\"."
  (let ((role
         (capitalize (substring (symbol-name (cdr (assoc 'role message))) 1)))
        (content (cdr (assoc 'content message)))) (format "# %s\n%s" role content)))

(defun ellm--messages-to-markdown-string (messages)
  "Convert the MESSAGES to a markdown string."
  (mapconcat #'ellm--stringify-message messages "\n\n"))

(defun ellm--markdown-to-org (markdown-string callback)
  "Convert MARKDOWN-STRING to `org-mode' format using pandoc.
Call CALLBACK with the result."
  (let ((process-connection-type nil)) ; Use a pipe
    (let ((process (make-process
                    :name "markdown-to-org"
                    :buffer "*markdown-to-org*"
                    :stderr (get-buffer-create "*markdown-to-org-error-logs*")
                    :command '("/usr/bin/pandoc" "-f" "markdown" "-t" "org" "--shift-heading-level-by=2")
                    :sentinel
                    (lambda (proc event)
                      (when (string= event "finished\n")
                        (with-current-buffer (process-buffer proc)
                          (let ((output (buffer-string)))
                            (funcall callback output))
                          (kill-buffer (process-buffer proc))))))))
      (process-send-string process markdown-string)
      (process-send-eof process))))

(defun ellm--markdown-to-org-using-pandoc (markdown-string)
  "Convert MARKDOWN-STRING from Markdown to Org using Pandoc."
  (let ((pandoc-command "pandoc -f markdown -t org --shift-heading-level-by=2"))
    (with-temp-buffer
      (insert markdown-string)
      (shell-command-on-region (point-min) (point-max) pandoc-command (current-buffer) t ellm--log-buffer-name)
      (buffer-string))))

(defun ellm--separate-response (response-content)
  "Take the `RESPONSE-CONTENT' string and extract the title.
Return a cons cell consisting of the title for its =car= and
the rest of the response for its =cdr=.
Note: the ~?~ in ~.*?~ makes the match non-greedy, ensuring it stops at the
first horizontal rule."
  (when (string-match "\\(.*?\\)--------------\\(.*\\)" response-content)
    (cons (string-trim (match-string 1 response-content))
          (string-trim (match-string 2 response-content)))))

(defun ellm--insert-conversation-into-org (prompt-data)
  "Insert the `PROMPT-DATA' into the org file."
  (let* ((messages (cdr (assoc 'messages prompt-data)))
         (model (cdr (assoc 'model prompt-data)))
         (temperature (cdr (assoc 'temperature prompt-data)))
         ;; (max-tokens (cdr (assoc 'max_tokens prompt-data)))
         (buffer (find-file-noselect ellm--conversations-file))
         (markdown-formatted-messages
          (ellm--messages-to-markdown-string messages))
         (org-formatted-messages
          (ellm--markdown-to-org-using-pandoc markdown-formatted-messages)))
    ;; (ellm--log-markdown-messages markdown-formatted-messages)
    ;; (ellm--log-org-messages org-formatted-messages)
    (with-current-buffer buffer
      (save-excursion
        (org-mode)
        (goto-char (point-min))
        (org-insert-heading)
        (insert "Conversation " (current-time-string) "\n")
        (org-id-get-create)
        (org-set-property "Timestamp" (format-time-string "%Y-%m-%d %H:%M:%S"))
        (org-set-property "Model" model)
        (org-set-property "Temperature" (number-to-string temperature))
        (insert org-formatted-messages)
        (save-buffer)
        (display-buffer (current-buffer))))))

(defun ellm--get-first-heading-id ()
  "Retrieve the ID of the first top-level heading of file at ORG-FILE-PATH."
  (with-current-buffer (find-file-noselect ellm--conversations-file)
    (save-excursion
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
          (ellm--log-error (format "\"Conversation with ID %s not found\"" id) "ERROR"))))))

(defun ellm--get-properties (id)
  "Retrieve and process properties for a given ID."
  (let ((props-alist (ellm--org-get-property-drawer-alist)))
    (list `("ID" . ,id)
          `("Model" . ,(cdr (assoc "Model" props-alist)))
          `("Temperature" . ,(cdr (assoc "Temperature" props-alist)))
          `("Max-tokens" . ,(cdr (assoc "Max-tokens" props-alist))))))

(defun ellm--get-messages-from-org-entry ()
  "Retrieve content of all subheadings, prepended with their titles."
  (org-goto-first-child)
  (let ((messages-content '()))
    (while (org-get-next-sibling)
      (let ((element (org-element-at-point)))
        (when (eq (org-element-type element) 'headline)
          (let* ((title (org-element-property :title element))
                 (content (org-element-property :contents-begin element))
                 (end (org-element-property :contents-end element))
                 (formatted-title (format "** %s:\n" title)))
            (when (and content end)
              (push (concat formatted-title (buffer-substring-no-properties content end)) messages-content))))))
    (nreverse messages-content)))

(defun ellm--org-get-property-drawer-alist ()
  "Retrieve the property drawer of the current heading as an alist."
  (save-excursion
    (let ((properties nil))
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

(defun ellm-toggle-test-mode ()
  "Set LLM parameters to lowest token cost for testing purposes."
  (interactive)
  (let ((test-mode (< ellm-default-max-tokens 20)))
    (if test-mode
        (and
         (setq ellm-default-max-tokens 1000
               ellm-default-model "gpt-4-turbo-preview"
               ellm--test-mode t)
         (message "ellm-test-mode disabled"))
      (and
        (setq ellm-default-max-tokens 10
              ellm-default-model "gpt-3.5-turbo"
              ellm--test-mode nil)
        (message "ellm-test-mode enabled")))))

(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ; n") #'ellm-chat)
            (define-key map (kbd "C-c ; t") #'ellm-toggle-test-mode)
            map)
  (if ellm-mode
      (message "ELLM mode enabled")
    (message "ELLM mode disabled")))

(provide 'ellm)
;;; ellm.el ends here
