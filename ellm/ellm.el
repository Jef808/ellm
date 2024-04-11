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
(require 'savehist)

(require 'markdown-mode)

(defgroup ellm nil
  "Make API calls to LLMs."
  :group 'tools
  :prefix "ellm-")

(defcustom ellm-get-openai-api-key
  #'ellm--get-openai-api-key-from-env
  "A function which retrieves your OpenAI API key."
  :type 'function
  :group 'ellm)

(defcustom ellm-get-anthropic-api-key
  #'ellm--get-anthropic-api-key-from-env
  "A function which retrieves your Anthropic API key."
  :type 'function
  :group 'ellm)

(defconst ellm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--anthropic-api-url "https://api.anthropic.com/v1/messages"
  "The URL to send requests to the OpenAI API.")

(defcustom ellm--temp-conversations-buffer-name "*LLM Conversations*"
  "The file to store conversation history."
  :type 'string
  :group 'ellm)

(defcustom ellm--conversations-file "~/.llm/conversations.org"
  "The file to store conversation history."
  :type 'string
  :group 'ellm)

(defcustom ellm-provider 'openai
  "The provider to use for API calls."
  :type '(choice
          (const :tag "OpenAI" openai)
          (const :tag "Anthropic" anthropic))
  :group 'ellm)

(defcustom ellm-model-size 'big
  "The model size to use for API calls."
  :type '(choice
          (const :tag "Big" big)
          (const :tag "Medium" medium)
          (const :tag "Small" small))
  :group 'ellm)

(defcustom ellm-model "gpt-4-turbo-preview"
  "The model to use when making a prompt."
  :type 'string
  :group 'ellm)

(defcustom ellm-temperature 0.2
  "The temperature to use when making a prompt."
  :type 'float
  :group 'ellm)

(defcustom ellm-max-tokens 1000
  "The temperature to use when making a prompt."
  :type 'integer
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

(defcustom ellm-model-providers-alist `(("gpt-4-turbo-preview" . openai)
                                        ("gpt-3.5-turbo" . openai)
                                        ("claude-3-opus-20240229" . anthropic)
                                        ("claude-3-sonnet-20240229" . anthropic)
                                        ("claude-3-haiku-20240307" . anthropic))
  "Alist mapping model names to their providers."
  :type 'alist
  :group 'ellm)

(defcustom ellm-save-conversations t
  "If non-nil, save the conversation history to `ellm--conversations-file'."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-time-format-string "%r"
  "The format string to use with `format-time-string' for displaying conversations."
  :type 'string
  :group 'ellm)

(defcustom ellm--debug-mode nil
  "If non nil, log each request and response to the `ellm--log-buffer-name' buffer."
  :type 'boolean
  :group 'ellm)

(defvar ellm--test-mode nil
  "If non-nil, set LLM parameters to lowest token cost for testing purposes.")

(defcustom ellm-system-message "You are a useful emacs-integrated code editing assistant.
Your goal is to execute the user's task or precisely answer their questions, using **all** \
of the CONTEXT they provide (if any).
You are very cautious when providing information or making a claim, you thus always \
accompany those with a thorough explanation or justification.
Moreover, you should ***always*** include a short summary title \
which describes the discussion for easy navigation through the user's conversation history. \n \
Separate it by a markdown horizontal rule (i.e. a line consisting of three or more dashes).
Your answer should thus be formatted as follows:\n\n

{{Your title here}}

--------------------------------------------

{{Your response here}}"
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

(defun ellm--setup-persistance ()
  "Register ellm configuration variables with the `savehist' package."
  (let ((symbols-to-add '(ellm-max-tokens
                          ellm-model-size
                          ellm-provider
                          ellm-temperature
                          ellm-save-conversations)))
    (dolist (symbol symbols-to-add)
      (cl-pushnew symbol savehist-additional-variables))))

(defun ellm--get-openai-api-key-from-env ()
  "Get the OpenAI API key from the environment."
  (getenv "OPENAI_API_KEY"))

(defun ellm--get-anthropic-api-key-from-env ()
  "Get the Anthropic API key from the environment."
  (getenv "ANTHROPIC_API_KEY"))

(defun ellm-set-provider (&optional provider)
  "Set the API `PROVIDER' to use."
  (interactive)
  (let ((p (if provider (intern provider)
             (intern (completing-read "Choose provider: " '(openai anthropic))))))
    (setq ellm-model (alist-get ellm-model-size
                                (if (eq p 'openai)
                                    ellm--openai-models-alist
                                  ellm--anthropic-models-alist))
          ellm-provider p)
    (message "...provider set to %s..." ellm-provider)))

(defun ellm-set-model-size (&optional model-size)
    "Set the `MODEL-SIZE' to use."
    (interactive)
    (setq ellm-model-size
          (if model-size model-size
            (intern (completing-read "Choose model size: " '(big medium small)))))
    (let ((models-alist (if (eq ellm-provider 'openai)
                            ellm--openai-models-alist
                          ellm--anthropic-models-alist)))
      (setq ellm-model (cdr (assoc ellm-model-size models-alist)))
      (message "...model set to %s..." ellm-model)))

(defun ellm--validation-max-tokens (max-tokens)
  "Validate the `MAX-TOKENS' value."
  (and (integerp max-tokens) (>= max-tokens 1) (<= max-tokens 4096)))

(defun ellm-set-max-tokens (&optional max-tokens)
  "Set the `MAX-TOKENS' to use for the LLM prompt."
  (interactive)
  (if (called-interactively-p 'any)
    (let (mt)
      (while (not (and (setq mt (read-number "Max tokens (between 1 and 4096): "))
                       (ellm--validation-max-tokens mt)))
        (message "Error: Invalid max-tokens value: %s" mt))
      (and (setq ellm-max-tokens mt)
           (message "...max-tokens set to %d..." ellm-max-tokens)))
    (if (ellm--validation-max-tokens max-tokens)
        (setq ellm-max-tokens max-tokens)
      (error "Invalid argument: `%s' should be integer between 1 and 4096" max-tokens))))

(defun ellm--validation-temperature (temperature)
  "Validate the `TEMPERATURE' value."
  (and (numberp temperature) (>= temperature 0) (<= temperature 2)))

(defun ellm-set-temperature (&optional temperature)
  "Set the `TEMPERATURE' to use for the LLM prompt."
  (interactive)
  (if (called-interactively-p 'any)
   (let (temp)
     (while (not (and (setq temp (read-number "Temperature (between 0.0 and 2.0): "))
                      (ellm--validation-temperature temp)))
       (message "Error: Invalid temperature value: %s" temp))
     (and (setq ellm-temperature temp)
           (message "...temperature set to %s..." ellm-temperature)))
   (if (ellm--validation-max-tokens temperature)
       (setq ellm-temperature temperature)
     (error "Invalid argument: `%s' should be number between 0 and 2" temperature))))

(defun ellm-set-config ()
  "Call the `SETTING-FUNCTION' according to the user's choice."
  (interactive)
  (let (setting-function)
    (while (setq setting-function (ellm--config-prompt))
      (funcall-interactively setting-function))))

(defun ellm--config-prompt ()
  "Prompt the user to choose a setting to configure."
  (let ((choices (list
                  (cons (ellm--toggle-test-mode-description) 'ellm-toggle-test-mode)
                  (cons (ellm--toggle-save-conversations-description) 'ellm-toggle-save-conversations)
                  (cons (ellm--toggle-debug-mode-description) 'ellm-toggle-debug-mode)
                  (cons (ellm--provider-description) 'ellm-set-provider)
                  (cons (ellm--model-size-description) 'ellm-set-model-size)
                  (cons (ellm--temperature-description) 'ellm-set-temperature)
                  (cons (ellm--max-tokens-description) 'ellm-set-max-tokens))))
    (alist-get
     (completing-read "Choose a setting to configure: " (mapcar 'car choices))
     choices nil nil 'equal)))

(defun ellm--provider-description ()
  "Return a string describing the current provider."
  (format "Provider                        %s"
          (propertize (symbol-name ellm-provider)
                      'face 'font-lock-string-face)))

(defun ellm--model-size-description ()
  "Return a string describing the current model size."
  (format "Model size                      %s"
          (propertize (symbol-name ellm-model-size)
                      'face 'font-lock-string-face)))

(defun ellm--temperature-description ()
  "Return a string describing the current temperature."
  (format "Temperature                     %s"
          (propertize (number-to-string ellm-temperature)
                      'face 'font-lock-number-face)))

(defun ellm--max-tokens-description ()
  "Return a string describing the current max tokens."
  (format "Max Tokens                      %s"
          (propertize (number-to-string ellm-max-tokens)
                      'face 'font-lock-number-face)))

(defun ellm--toggle-save-conversations-description ()
  "Return a string describing the current save conversations setting."
  (format "Save Conversations to file      %s"
          (propertize (if ellm-save-conversations "t" "nil")
                      'face (if ellm-save-conversations
                                'font-lock-builtin-face
                              'font-lock-comment-face))))

(defun ellm--toggle-test-mode-description ()
  "Return a string describing the current save conversations setting."
  (format "Test Mode                       %s"
          (propertize (if ellm--test-mode "t" "nil")
                      'face (if ellm--test-mode
                                'font-lock-builtin-face
                              'font-lock-comment-face))))

(defun ellm--toggle-debug-mode-description ()
  "Return a string describing the current debug mode setting."
  (format "Debug Mode                      %s"
          (propertize (if ellm--debug-mode "t" "nil")
                      'face (if ellm--debug-mode
                                'font-lock-builtin-face
                              'font-lock-comment-face))))

(defun ellm--log (data &optional label should-encode-to-json)
  "Log `DATA' with an optional `LABEL'.
Will call `json-encode' on `DATA' if
`SHOULD-ENCODE-TO-JSON' is set to a non-nil value."
  (let* ((json-data (if should-encode-to-json (json-encode data) data))
         (formatted-log (format "{\"TIMESTAMP\": \"%s\", \"%s\": %s}"
                                (format-time-string "%Y-%m-%d %H:%M:%S")
                                (or label "INFO")
                                json-data)))
    (with-current-buffer (get-buffer-create ellm--log-buffer-name)
      (goto-char (point-max))
      (unless (bolp) (newline))
      (let ((pos (point)))
        (insert formatted-log)
        (newline)
        (json-pretty-print pos (point-max))
        (goto-char pos)))))

(defun ellm--log-response-error (error)
  "Log the `ERROR' response."
  (ellm--log error "RESPONSE-TYPE-ERROR" t))

(defun ellm--log-json-error (error)
  "Log the `ERROR' when response fails to parse as json."
  (ellm--log error "JSON-ERROR" t))

(defun ellm--log-response-splitting-error (error)
  "Log the `ERROR' when response fails to split."
  (ellm--log (format "No title or body in response: %S" error)
             "RESPONSE-FORMAT-ERROR" t))

(defun ellm--log-http-error (status)
  "Log HTTP `STATUS' errors."
  (ellm--log (plist-get status :error) "HTTP-ERROR") t)

(defun ellm--log-http-redirect (status)
  "Log HTTP `STATUS' redirects."
  (ellm--log (plist-get status :redirect) "HTTP-REDIRECT") t)

(defun ellm--log-no-response-body ()
  "Log that were no response body."
  (ellm--log "No response body (DNS resolve or TCP error)" "REQUEST-ERROR" t))

(defun ellm--log-request-headers (headers)
  "Log the request `HEADERS'."
  (when ellm--debug-mode (ellm--log headers "REQUEST-HEADERS" t)))

(defun ellm--log-request-body (body)
  "Log the request `BODY'."
  (when ellm--debug-mode (ellm--log body "REQUEST-BODY")))

(defun ellm--log-response-body (response-body)
  "Log the `RESPONSE-BODY'."
  (when ellm--debug-mode (ellm--log response-body "RESPONSE-BODY")))

(defun ellm--log-response-content (response-content)
  "Log the `RESPONSE-CONTENT'."
  (when ellm--debug-mode (ellm--log response-content "RESPONSE-CONTENT" t)))

(defun ellm--log-prompt-data (prompt-data)
  "Log the `PROMPT-DATA'."
  (when ellm--debug-mode (ellm--log prompt-data "PROMPT-DATA" t)))

(defun ellm--log-markdown-messages (markdown-string)
  "Log `MARKDOWN-STRING'."
  (when ellm--debug-mode
    (ellm--log (ellm--markdown-pretty-print markdown-string)
               "MESSAGES-FOR-MARKDOWN" t)))

(defun ellm--log-org-messages (org-string)
  "Log `ORG-STRING'."
  (when ellm--debug-mode
    (ellm--log (ellm--org-pretty-print org-string)
               "MESSAGES-FOR-ORG-MODE" t)))

(defun ellm--org-pretty-print (org-content)
  "Pretty print `ORG-CONTENT'."
  (with-temp-buffer
    (insert org-content)
    (org-mode)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ellm--markdown-pretty-print (markdown-content)
  "Pretty print `MARKDOWN-CONTENT'."
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

(defun ellm--maybe-make-prompt-contextual (prompt)
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
  "Create a message with the given `ROLE' and `CONTENT'."
  `((role . ,role) (content . ,content)))

(defun ellm--add-system-message (content conversation-data)
  "Prepend a system message with `CONTENT' to the `CONVERSATION-DATA'."
  (push (ellm--make-message :system content)
        (alist-get 'messages conversation-data nil nil)))

(defun ellm--add-user-message (content conversation-data)
  "Append a user message with `CONTENT' to the `CONVERSATION-DATA'."
  (nconc (alist-get 'messages conversation-data)
         (list (ellm--make-message :user content))))

(defun ellm--add-assistant-message (content conversation-data)
  "Append an assistant message with `CONTENT' to the `CONVERSATION-DATA'."
  (nconc (alist-get 'messages conversation-data)
         (list (ellm--make-message :assistant content))))

(defun ellm--add-or-update-title (title conversation-data)
  "Add or update the `TITLE' in the `CONVERSATION-DATA'.
A generic `Untitled <TIMESTAMP>' title is used if `TITLE' is nil."
  (setf (alist-get 'title conversation-data)
        (if title title
          (format "Untitled  %s" (current-time-string)))))

(defun ellm--handle-assistant-response (response conversation-data)
  "Add the `RESPONSE' to the `CONVERSATION-DATA'."
  (let* ((split-response (ellm--split-response response))
         (title (elt split-response 0))
         (content (elt split-response 1)))
    (ellm--add-or-update-title title conversation-data)
    (ellm--add-assistant-message content conversation-data)))

(defun ellm--split-response (response-content)
  "Split the `RESPONSE-CONTENT' around a markdown horizontal rule.

When response comes in the form of

\"TITLE
---
CONTENT\"

this returns both components as the list

`(\"TITLE\" \"CONTENT\")'

When no horizontal rule is found, it sets the title as nil:

`(nil RESPONSE-CONTENT)'.

Note that both components are trimmed of whitespace."
  (let* ((res (string-split
               response-content
               "\n\\(\\*\\{3,\\}\\|\\-\\{3,\\}\\|_\\{3,\\}\\)\n" t "[ \f\t\n\r\v]+")))
    (when (length= res 1) (push nil res))
    res))

(defun ellm--initialize-conversation (prompt)
  "Initialize a new conversation starting with `PROMPT'.

Return the conversation-data alist."
  (let* ((effective-prompt (ellm--maybe-make-prompt-contextual prompt))
         (user-message (ellm--make-message :user effective-prompt)))
    `((messages . (,user-message))
      (temperature . ,ellm-temperature)
      (max_tokens . ,ellm-max-tokens)
      (model . ,ellm-model)
      (title . nil)
      (system . ,ellm-system-message))))

(defun ellm--get-model-provider (conversation-data)
  "Get the provider of the model in `CONVERSATION-DATA'."
  (alist-get (alist-get 'model conversation-data)
             ellm-model-providers-alist nil nil #'string=))

(defun ellm--prepare-request-headers (conversation-data)
  "Prepare the API call headers to send `CONVERSATION-DATA'."
  (let* ((provider (ellm--get-model-provider conversation-data))
         (headers
          (cond ((eq provider 'openai)
                 `(("Authorization" . ,(concat "Bearer " (funcall ellm-get-openai-api-key)))))
                ((eq provider 'anthropic)
                 `(("x-api-key" . ,(funcall ellm-get-anthropic-api-key))
                   ("anthropic-version" . "2023-06-01")))
                (t (error "ellm--prepare-request-headers: Unknown provider: %s" (symbol-name provider))))))
    (setf (alist-get "Content-Type" headers nil nil #'string=) "application/json")
    headers))

(defun ellm--prepare-request-body (conversation-data)
  "Prepare the messages list with a new user message based on `CONVERSATION-DATA'."
  (let ((data (copy-alist conversation-data))
        (provider (ellm--get-model-provider conversation-data)))
    (unless (eq provider 'anthropic)
      (let* ((messages (cl-copy-list (alist-get 'messages data)))
             (system-message (alist-get 'system data)))
        (setf (alist-get 'messages data) messages)
        (ellm--add-system-message system-message data)
        (setf (alist-get 'system data nil t) nil)))
    (setf (alist-get 'title data nil t) nil)
    (json-encode data)))

(defun ellm--get-url (conversation-data)
  "Get the URL to send the request to based on `CONVERSATION-DATA'."
  (let ((provider (ellm--get-model-provider conversation-data)))
    (cond ((eq provider 'openai) ellm--openai-api-url)
          ((eq provider 'anthropic) ellm--anthropic-api-url)
          (t (error "ellm--get-url: Unknown provider: %s" (symbol-name provider))))))

(defun ellm-chat (prompt &optional current-conversation-data)
  "Send the PROMPT through to the LLM.

If `CURRENT-CONVERSATION-DATA' is non-nil, continue that conversation."
  (interactive "sEnter your prompt: ")
  (let ((conversation-data (if current-conversation-data
                               current-conversation-data
                             (ellm--initialize-conversation prompt))))
    (let* ((url (ellm--get-url conversation-data))
           (request-headers (ellm--prepare-request-headers conversation-data))
           (request-body (ellm--prepare-request-body conversation-data))
           (url-request-method "POST")
           (url-request-extra-headers request-headers)
           (url-request-data request-body))
      (ellm--log-request-headers request-headers)
      (ellm--log-request-body request-body)
      (url-retrieve url #'ellm--handle-response (list conversation-data)))))

(defun ellm--handle-response (status conversation-data)
  "Handle the response to the prompt made using `CONVERSATION-DATA'.

Information about the response is contained in `STATUS' (see `url-retrieve')."
  (cond ((plist-get status :error)
         (ellm--log-http-error status))
        ((plist-get status :redirect)
         (ellm--log-http-redirect status))
        (t
         (goto-char url-http-end-of-headers)
         (if (<= (point-max) (point))
             (ellm--log-no-response-body)
           (let ((parsed-response (ellm--parse-json-response)))
             (when parsed-response
               (ellm--add-response-to-conversation parsed-response conversation-data)))))))

(defun ellm--parse-json-response ()
  "Parse the json response from the API call."
  (condition-case error
      (let ((response-body
             (string-trim (buffer-substring-no-properties (point) (point-max)))))
        (ellm--log-response-body response-body)
        (json-parse-string response-body))
    (json-parse-error
     (ellm--log-json-error error)
     nil)))

(defun ellm--add-response-to-conversation (parsed-response prompt-data)
  "Process the `PARSED-RESPONSE' from the API call made with config `PROMPT-DATA'."
  (let* ((provider (ellm--get-model-provider prompt-data))
         (response-content (cond ((eq provider 'openai)
                                  (ellm--extract-response-content-openai parsed-response))
                                 ((eq provider 'anthropic)
                                  (ellm--extract-response-content-anthropic parsed-response))
                                 (t (error "ellm--add-response-to-conversation: Unknown provider: %s"
                                           (symbol-name provider))))))
    (ellm--handle-assistant-response response-content prompt-data)
    (ellm--log-prompt-data prompt-data)
    (ellm--insert-conversation-into-org prompt-data)))

(defun ellm--extract-response-content-openai (response)
  "Extract the text from the json `RESPONSE'.
This function is meant to be used with the response from the OpenAI API."
  (condition-case error
      (let* ((choices (gethash "choices" response))
             (first-choice (aref choices 0))
             (msg (gethash "message" first-choice))
             (content (gethash "content" msg)))
        (ellm--log-response-content content)
        (replace-regexp-in-string "\\\\\"" "\"" content))
    (wrong-type-argument (ellm--log-response-error error))))

(defun ellm--extract-response-content-anthropic (response)
  "Extract the text from the json `RESPONSE'."
  (condition-case error
      (let* ((messages (gethash "content" response))
             (first-message (aref messages 0))
             (content (gethash "text" first-message)))
        (ellm--log-response-content content)
        (replace-regexp-in-string "\\\\\"" "\"" content))
    (wrong-type-argument (ellm--log-response-error error))))

(defun ellm--stringify-message (message)
  "Convert the `MESSAGE' to a string.

A message of the form

  `((role . :role) (content . \"content\"))'

is converted to the string

  \"# Role
     content\"."
  (let ((role
         (capitalize (substring (symbol-name (alist-get 'role message)) 1)))
        (content (alist-get 'content message)))
    (format "# %s\n\n%s" role content)))

(defun ellm--messages-to-markdown-string (messages)
  "Convert the MESSAGES to a markdown string."
  (mapconcat #'ellm--stringify-message messages "\n\n"))

(defun ellm--markdown-to-org-sync (markdown-string)
  "Convert `MARKDOWN-STRING' from Markdown to Org using Pandoc."
  (let ((pandoc-command "pandoc -f markdown -t org --shift-heading-level-by=1"))
    (with-temp-buffer
      (insert markdown-string)
      (shell-command-on-region (point-min) (point-max) pandoc-command (current-buffer) t ellm--log-buffer-name)
      (buffer-string))))

(defun ellm--insert-conversation-into-org (prompt-data)
  "Insert the `PROMPT-DATA' into the org file."
  (let* ((messages (alist-get 'messages prompt-data))
         (title (alist-get 'title prompt-data))
         (model (alist-get 'model prompt-data))
         (temperature (alist-get 'temperature prompt-data))
         (buffer (if ellm-save-conversations
                     (find-file-noselect ellm--conversations-file)
                   (get-buffer-create ellm--temp-conversations-buffer-name)))
         (org-formatted-messages (ellm--convert-messages-to-org messages)))
    (ellm--log-org-messages org-formatted-messages)
    (save-excursion
      (with-current-buffer buffer
        (read-only-mode -1)
        (ellm--insert-heading-and-metadata title model temperature)
        (insert org-formatted-messages)
        (insert "\n--------------\n")
        (when ellm-save-conversations
          (save-buffer))
        (read-only-mode 1)
        (ellm--display-org-buffer)))))

(defun ellm--convert-messages-to-org (messages)
  "Convert `MESSAGES' to an Org-formatted string using Pandoc."
  (let ((markdown (ellm--messages-to-markdown-string messages)))
    (ellm--log-markdown-messages markdown)
    (ellm--markdown-to-org-sync markdown)))

(defun ellm--insert-heading-and-metadata (title model temperature)
  "Insert an Org heading with TITLE, MODEL, and TEMPERATURE as properties."
  (goto-char (point-min))
  (org-insert-heading)
  (insert title)
  ;; (newline)
  (when ellm-save-conversations
    (org-id-get-create))
  (org-set-property "Timestamp" (format-time-string "%Y-%m-%d %H:%M:%S"))
  (org-set-property "Model" model)
  (org-set-property "Temperature" (number-to-string temperature)))

(defun ellm--display-org-buffer ()
  "Prepare the conversations buffer for viewing."
  (org-mode)
  (org-overview)
  (ellm--goto-first-top-level-heading)
  (org-fold-show-subtree)
  (display-buffer (current-buffer) t))

(defun ellm--goto-first-top-level-heading ()
  "Go to the first top-level heading in the current buffer."
  (goto-char (point-min))
  (org-goto-first-child))

(defun ellm--get-first-heading-id ()
  "Retrieve the ID of the first top-level heading of file at ORG-FILE-PATH."
  (with-current-buffer (find-file-noselect ellm--conversations-file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^\* ")
      (org-entry-get (point) "ID"))))

(defun ellm--retrieve-conversation-by-id (id)
  "Retrieve a conversation by its unique `ID' along with its properties."
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
  "Retrieve and process properties for a given `ID'."
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
  "Set LLM parameters to lowest token cost for testing purposes.

When togling off, restore the previously set values."
  (interactive)
  (if ellm--test-mode
    (progn
      (ellm-set-max-tokens (get 'ellm-max-tokens 'previous-value))
      (ellm-set-model-size (get 'ellm-model-size 'previous-value))
      (setq ellm-save-conversations (get 'ellm-save-conversations 'previous-value))
      (put 'ellm-max-tokens 'previous-value nil)
      (put 'ellm-model-size 'previous-value nil)
      (setq ellm--test-mode nil)
      (message "...ellm-test-mode disabled..."))
    (progn
      (put 'ellm-max-tokens 'previous-value (symbol-value 'ellm-max-tokens))
      (put 'ellm-model-size 'previous-value (symbol-value 'ellm-model-size))
      (put 'ellm-save-conversations 'previous-value (symbol-value 'ellm-save-conversations))
      (ellm-set-max-tokens 10)
      (ellm-set-model-size 'small)
      (setq ellm-save-conversations nil)
      (setq ellm--test-mode t)
      (message "...ellm-test-mode enabled..."))))

(defun ellm-toggle-debug-mode ()
  "Toggle debug mode."
  (interactive)
  (setq ellm--debug-mode (not ellm--debug-mode))
  (message "...debug mode %s..." (if ellm--debug-mode "enabled" "disabled")))

(defun ellm-show-conversations ()
  "Show the conversations in the `ellm--conversations-file'."
  (interactive)
  (find-file-other-window ellm--conversations-file)
  (with-current-buffer ellm--conversations-file
    (org-mode)
    (org-overview)))

(defun ellm-toggle-save-conversations ()
  "Toggle saving conversations to `ellm--conversations-file'."
  (interactive)
  (setq ellm-save-conversations (not ellm-save-conversations)))

(defun ellm-next-conversation ()
  "Move the cursor to the next top-level heading in an Org-mode buffer."
  (interactive)
  (when (string= (buffer-file-name) "home/jfa/.llm/conversations.org")
    (progn
      (org-next-visible-heading 1)
      (while (not (looking-at "^\\* "))
        (org-next-visible-heading 1))
      (recenter-top-bottom 1))))

(defun ellm--extract-definitions-in-buffer ()
  "Extract all variable and function definitions in the current buffer."
  (interactive)
  (let ((functions '())
        (variables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^(defvar|defcustom " nil t)
        (push `((,(symbol-at-point) . (documentation (symbol-at-point)))) variables))
      (goto-char (point-min))
      (while (re-search-forward "^(defun " nil t)
        (push `((,(symbol-at-point) . ,(documentation (symbol-at-point)))) functions)))
    `((variables . ,(nreverse variables)) (functions . ,(nreverse functions)))))

(defun ellm-fold-conversations-buffer ()
  "Fold all conversations in the `ellm--conversations-file'."
  (interactive)
  (with-current-buffer (get-file-buffer ellm--conversations-file)
    (goto-char (point-min))
    (org-overview)
    (ellm--goto-first-top-level-heading)))

;;;###autoload
(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ; n") #'ellm-chat)
            (define-key map (kbd "C-c ; t") #'ellm-toggle-test-mode)
            (define-key map (kbd "C-c ; c") #'ellm-set-config)
            (define-key map (kbd "C-c ; ;") #'ellm-show-conversations)
            (define-key map (kbd "C-c ; s") #'ellm-toggle-save-conversations)
            (define-key map (kbd "C-c ; o") #'ellm-fold-conversations-buffer)
            map))

;;;###autoload
(define-globalized-minor-mode global-ellm-mode ellm-mode
  (lambda ()
    (ellm--setup-persistance)
    (ellm-mode 1)))

(provide 'ellm)
;;; ellm.el ends here
