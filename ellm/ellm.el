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

(require 'f)
(require 'json)
(require 'markdown-mode)
(require 'org-capture)
(require 'org-element)
(require 'org-fold)
(require 'org-id)
(require 'ox-html)
(require 'savehist)
(require 'url)
(require 'know-your-http-well)

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

(defcustom ellm-get-groq-api-key
  #'ellm--get-groq-api-key-from-env
  "A function which retrieves your Groq API key."
  :type 'function
  :group 'ellm)

(defconst ellm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--anthropic-api-url "https://api.anthropic.com/v1/messages"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--groq-api-url "https://api.groq.com/openai/v1/chat/completions"
  "The URL to send requests to the Groq API.")

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
          (const :tag "Anthropic" anthropic)
          (const :tag "Groq" groq))
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


(defcustom ellm--groq-models-alist `((big . "llama3-70b-8192")
                                     (medium . "llama3-8b-8192")
                                     (small . "mixtral-8x7b-32768"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm-model-alist `(("gpt-4-turbo-preview" . (:provider openai :size big))
                              ("gpt-3.5-turbo" . (:provider openai :size small))
                              ("claude-3-opus-20240229" . (:provider anthropic :size big))
                              ("claude-3-sonnet-20240229" . (:provider anthropic :size medium))
                              ("claude-3-haiku-20240307" . (:provider anthropic :size small))
                              ("llama3-70b-8192" . (:provider groq :size big))
                              ("llama3-8b-8292" . (:provider groq :size medium))
                              ("mixtral-8x7b-32768" . (:provider groq :size small)))
  "Alist mapping model names to their providers."
  :type 'alist
  :group 'ellm)

(defcustom ellm-save-conversations t
  "If non-nil, save the conversation history to `ellm--conversations-file'."
  :type 'boolean
  :group 'ellm)

(defcustom ellm--time-format-string "[%Y-%m-%d %a %H:%M]"
  "The format string to use with `format-time-string' for displaying conversations."
  :type 'string
  :group 'ellm)

(defcustom ellm--debug-mode nil
  "If non nil, log each request and response to the `ellm--log-buffer-name' buffer."
  :type 'boolean
  :group 'ellm)

(defvar ellm--test-mode nil
  "If non-nil, set LLM parameters to lowest token cost for testing purposes.")

(defcustom ellm-system-message "You are a useful emacs-integrated general assistant.
Your goal is to execute the user's task or precisely answer their questions, using all \
the CONTEXT the user provides (if any).
You are very cautious when providing information or making a claim, you thus always \
accompany your answer with a thorough explanation or justification when something is not obvious.
A user would always prefer you answering with a request for clarifications and/or questions when \
you are unsure, they will then provide the relevant context for you to can accomplish your task."
  "The system message to set the stage for new conversations."
  :type 'string
  :group 'ellm)

(defvar ellm--system-message-suffix "\nFinally, you should include a short summary title which describes the discussion, so that the user
can later navigate through their history. Separate the summary title by a markdown horizontal \
rule (i.e. a line consisting of three or more dashes). Your answer should thus be formatted as follows:

Your title here

--------------------------------------------

Your response here"
  "The system message suffix to append to the system message.")

(defcustom ellm-prompt-context-fmt-string "Consider the following CONTEXT :\n\n```%s\n%s\n```\n\n"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--make-context-message' for usage details."
  :type 'string
  :group 'ellm)

(defvar ellm-auto-export nil
  "If non-nil, the chat is being conducted in an external chat buffer.")

(defvar ellm--last-conversation-exported-id nil
  "The ID of the last conversation exported to an external chat buffer.")

(defconst ellm--log-buffer-name "*ELLM-Logs*"
  "Log buffer for LLM messages.")

(defvar ellm-provider-configurations
  `((openai . ((base-url . ,ellm--openai-api-url)
               (get-api-key . ,(lambda () (funcall ellm-get-openai-api-key)))
               (prepare-request-headers . ellm--prepare-request-headers-default)
               (prepare-request-body . ellm--prepare-request-body-default)
               (parse-response . ellm--parse-response-openai)
               (models-alist . ,ellm--openai-models-alist)))
    (anthropic . ((base-url . ,ellm--anthropic-api-url)
                  (get-api-key . ,(lambda () (funcall ellm-get-anthropic-api-key)))
                  (prepare-request-headers . ellm--prepare-request-headers-anthropic)
                  (prepare-request-body . ellm--prepare-request-body-anthropic)
                  (parse-response . ellm--parse-response-anthropic)
                  (models-alist . ,ellm--anthropic-models-alist)))
    (groq . ((base-url . ,ellm--groq-api-url)
             (get-api-key . ,(lambda () (funcall ellm-get-groq-api-key)))
             (prepare-request-headers . ellm--prepare-request-headers-default)
             (prepare-request-body . ellm--prepare-request-body-default)
             (parse-response . ellm--parse-response-openai)
             (models-alist . ,ellm--groq-models-alist))))
  "Alist mapping providers to their API configurations.")

(defun ellm--get-provider-configuration (provider)
  "Get the configuration for the `PROVIDER'."
  (alist-get provider ellm-provider-configurations))

(defun ellm--get-openai-api-key-from-env ()
  "Get the OpenAI API key from the environment."
  (getenv "OPENAI_API_KEY"))

(defun ellm--get-anthropic-api-key-from-env ()
  "Get the Anthropic API key from the environment."
  (getenv "ANTHROPIC_API_KEY"))

(defun ellm--get-groq-api-key-from-env ()
  "Get the Groq API key from the environment."
  (getenv "GROQ_API_KEY"))

(defun ellm-set-provider (&optional provider)
  "Set the API `PROVIDER' to use."
  (interactive)
  (let* ((p (or provider
                (intern (completing-read "Choose provider: " '(anthropic openai groq)))))
         (config (ellm--get-provider-configuration p))
         (models-alist (alist-get 'models-alist config)))
    (setq ellm-model (alist-get ellm-model-size models-alist)
          ellm-provider p))
  (message "...provider set to %s, model set to %s..." ellm-provider ellm-model))

(defun ellm-set-model-size (&optional model-size)
  "Set the `MODEL-SIZE' to use."
  (interactive)
  (let* ((ms (or model-size
                 (intern (completing-read "Choose model size: " '(big medium small)))))
         (config (ellm--get-provider-configuration ellm-provider))
         (models-alist (alist-get 'models-alist config)))
    (setq ellm-model (alist-get ms models-alist)
          ellm-model-size ms))
  (message "...model-size set to %s, model set to %s..." ellm-model-size ellm-model))

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
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map))
        (setting-function))
    (define-key minibuffer-local-map (kbd "q") 'keyboard-quit)
    (let ((minibuffer-allow-text-properties t)
          (minibuffer-local-map minibuffer-local-map))
      (while (setq setting-function (ellm--config-prompt))
        (funcall-interactively setting-function)))))

(defun ellm--config-prompt ()
  "Prompt the user to choose a setting to configure."
  (let ((choices (list
                  (cons (ellm--toggle-test-mode-description) 'ellm-toggle-test-mode)
                  (cons (ellm--toggle-save-conversations-description) 'ellm-toggle-save-conversations)
                  (cons (ellm--toggle-debug-mode-description) 'ellm-toggle-debug-mode)
                  (cons (ellm--provider-description) 'ellm-set-provider)
                  (cons (ellm--model-size-description) 'ellm-set-model-size)
                  (cons (ellm--temperature-description) 'ellm-set-temperature)
                  (cons (ellm--max-tokens-description) 'ellm-set-max-tokens)))
        (minibuffer-local-map (copy-keymap minibuffer-local-map)))
    (define-key minibuffer-local-map (kbd "q") 'abort-recursive-edit)
    (let ((minibuffer-allow-text-properties t)
          (minibuffer-local-map minibuffer-local-map))
      (alist-get
       (completing-read "Choose a setting to configure: " (mapcar 'car choices))
       choices nil nil 'equal))))

(defun ellm--provider-description ()
  "Return a string describing the current provider."
  (format "Provider                        %s"
          (propertize (symbol-name ellm-provider)
                      'face 'font-lock-type-face)))

(defun ellm--model-size-description ()
  "Return a string describing the current model size."
  (format "Model size                      %s"
          (propertize (symbol-name ellm-model-size)
                      'face 'font-lock-type-face)))

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

(defun ellm-toggle-auto-export ()
  "Toggle automatic exports of responses."
  (interactive)
  (setq ellm-auto-export (not ellm-auto-export))
  (message "...debug mode %s..." (if ellm-auto-export "enabled" "disabled")))

(defun ellm-toggle-save-conversations ()
  "Toggle saving conversations to `ellm--conversations-file'."
  (interactive)
  (setq ellm-save-conversations (not ellm-save-conversations)))

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

(defun ellm--get-model-properties (conversation)
  "Get the properties of the model in `CONVERSATION'."
  (let ((model (alist-get 'model conversation)))
    (alist-get model ellm-model-alist nil nil 'equal)))

(defun ellm--get-model-provider (conversation)
  "Get the provider of the model in `CONVERSATION'."
  (let ((properties (ellm--get-model-properties conversation)))
    (plist-get properties :provider)))

(defun ellm--get-model-size (conversation)
  "Get the size of the model in `CONVERSATION'."
  (plist-get (ellm--get-model-properties conversation) :size))

(defun ellm--set-model (model)
  "Set the `MODEL' to use for the LLM prompt."
  (let ((model-properties (alist-get model ellm-model-alist nil nil 'equal)))
    (setq ellm-model model
          ellm-provider (plist-get model-properties :provider)
          ellm-model-size (plist-get model-properties :size))))

(defvar ellm--major-mode-to-org-lang-alist
  '((python-mode . "python")
    (emacs-lisp-mode . "emacs-lisp")
    (org-mode . "org")
    (lua-mode . "lua"))
  "Alist mapping major modes to Org mode source block languages.")

(defun ellm--add-context-from-region (prompt)
  "Use the active region if any to enhance the `PROMPT' with context.
If a region is marked, use the `ellm-context-fmt-string' template
to add context from that region. In that case, the current buffer's
major mode is used according to `ellm--major-mode-to-org-lang-alist'
for determining the language with which to format the context."
  (let ((prefix
         (when (use-region-p)
           (format ellm-prompt-context-fmt-string
                   (or (cdr (assoc major-mode ellm--major-mode-to-org-lang-alist)) "text")
                   (buffer-substring-no-properties (region-beginning) (region-end)))))
        (deactivate-mark))
    (concat prefix prompt)))

(defun ellm--context-prefix-from-region ()
  "Return the context prefix from the active region for user prompt."
  (when (use-region-p)
    (format ellm-prompt-context-fmt-string
            (or (cdr (assoc major-mode ellm--major-mode-to-org-lang-alist)) "text")
            (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun ellm--make-message (role content)
  "Create a message with the given `ROLE' and `CONTENT'."
  `((role . ,role) (content . ,(encode-coding-string content 'utf-8))))

(defun ellm--add-system-message (content conversation)
  "Prepend a system message with `CONTENT' to the `CONVERSATION'."
  (push (ellm--make-message :system content)
        (alist-get 'messages conversation nil nil)))

(defun ellm--add-user-message (content conversation)
  "Append a user message with `CONTENT' to the `CONVERSATION'."
  (and (nconc (alist-get 'messages conversation)
                (list (ellm--make-message :user content)))
       conversation))

(defun ellm--add-assistant-message (content conversation)
  "Append an assistant message with `CONTENT' to the `CONVERSATION'."
  (nconc (alist-get 'messages conversation)
         (list (ellm--make-message :assistant content))))

(defun ellm--initialize-conversation (prompt)
  "Initialize a new conversation starting with `PROMPT'.

Return the conversation-data alist."
  (let* ((effective-prompt (ellm--add-context-from-region prompt))
         (user-message (ellm--make-message :user effective-prompt)))
    `((messages . (,user-message))
      (temperature . ,ellm-temperature)
      (max_tokens . ,ellm-max-tokens)
      (model . ,ellm-model)
      (title . nil)
      (system . ,(concat ellm-system-message ellm--system-message-suffix)))))

(defun ellm--prepare-request-headers (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations))
         (api-key-fn (lambda () (funcall (alist-get 'get-api-key config))))
         (prepare-fn (alist-get 'prepare-request-headers config)))
    (funcall prepare-fn api-key-fn)))

(defun ellm--prepare-request-headers-default (get-api-key)
  "Prepare the headers for API requests as done for openai models.

The `GET-API-KEY' function is used to retrieve the api key for the provider."
  `(("Authorization" . ,(concat "Bearer " (funcall get-api-key)))
    ("Content-Type" . "application/json; charset=utf-8")))

(defun ellm--prepare-request-headers-anthropic (get-api-key)
  "Prepare the headers for API requests made to anthropic models.

The `GET-API-KEY' function is used to retrieve the api key for anthropic."
  `(("x-api-key" . ,(funcall get-api-key))
    ("anthropic-version" . "2023-06-01")
    ("Content-Type" . "application/json; charset=utf-8")))

(defun ellm--prepare-request-body-default (conversation)
  "Prepare the messages list with a new user message based on `CONVERSATION'."
  (let ((data (copy-alist conversation)))
    (let* ((messages (cl-copy-list (alist-get 'messages data)))
           (system-message (alist-get 'system data)))
      (setf (alist-get 'messages data) messages)
      (ellm--add-system-message system-message data)
      (setf (alist-get 'system data nil t) nil))
    (setf (alist-get 'title data nil t) nil)
    (setf (alist-get 'id data nil t) nil)
    (json-encode data)))

(defun ellm--prepare-request-body-anthropic (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let ((data (copy-alist conversation)))
    (setf (alist-get 'title data nil t) nil)
    (setf (alist-get 'id data nil t) nil)
    (json-encode data)))

(defun ellm--prepare-request-body (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations))
         (prepare-fn (alist-get 'prepare-request-body config)))
    (funcall prepare-fn conversation)))

(defun ellm--get-url (conversation)
  "Get the URL to send the request to based on `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations)))
    (or (alist-get 'base-url config)
        (error "ellm--get-url: Unknown provider or missing base url: %s"
               (symbol-name provider)))))

(defun ellm-chat (&optional current-conversation next-prompt)
  "Send a request to the current provider's chat completion endpoint.

Unless `NEXT-PROMPT' is non-nil, the next prompt is read interactively
from the minibuffer. When `CURRENT-CONVERSATION' is a valid conversation object,
that conversation is continued with the next prompt and associated response."
  (interactive)
  (let* ((prompt-message (if current-conversation
                             "Enter your next prompt: "
                           "Enter your prompt: "))
         (prompt (or next-prompt (read-string prompt-message)))
         (conversation (or
                        (and current-conversation
                             (ellm--add-user-message prompt current-conversation))
                           (ellm--initialize-conversation prompt)))
         (url (ellm--get-url conversation))
         (request-headers (ellm--prepare-request-headers conversation))
         (request-body (ellm--prepare-request-body conversation))
         (url-request-method "POST")
         (url-request-extra-headers request-headers)
         (url-request-data request-body))
      (ellm--log-request-headers request-headers)
      (ellm--log-request-body request-body)
      (url-retrieve url #'ellm--handle-response (list conversation)))
  nil)

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

(defun ellm--add-response-to-conversation (parsed-response conversation)
  "Process the `PARSED-RESPONSE' from the API call made with config `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (ellm--get-provider-configuration provider))
         (parse-response (alist-get 'parse-response config))
         (response-content (funcall parse-response parsed-response)))
    (ellm--handle-assistant-response response-content conversation)
    (ellm--log-conversation conversation)
    (ellm--insert-conversation-into-org conversation)))

(defun ellm--handle-assistant-response (response conversation)
  "Add the `RESPONSE' to the `CONVERSATION'."
  (let* ((split-response (ellm--split-response response))
         (title (elt split-response 0))
         (content (elt split-response 1)))
    (ellm--add-or-update-title title conversation)
    (ellm--add-assistant-message content conversation)))

(defun ellm--insert-conversation-into-org (conversation)
  "Insert the `CONVERSATION' into the org file."
  (let* ((messages (alist-get 'messages conversation))
         (title (alist-get 'title conversation))
         (model (alist-get 'model conversation))
         (temperature (alist-get 'temperature conversation))
         (id (alist-get 'id conversation))
         (buffer (if ellm-save-conversations
                     (find-file-noselect ellm--conversations-file)
                   (get-buffer-create ellm--temp-conversations-buffer-name)))
         (org-formatted-messages (ellm--convert-messages-to-org messages)))
    (ellm--log-org-messages org-formatted-messages)
    (with-current-buffer buffer
      (org-mode)
      (read-only-mode -1)
      (save-excursion
        (if-let ((pos (and ellm-save-conversations (org-id-find id 'marker))))
            (progn (goto-char pos)
                   (org-cut-subtree))
          (setq id (or id (org-id-new))))
        (goto-char (point-min))
        (ellm--insert-heading-and-metadata title id model temperature)
        (insert org-formatted-messages)
        (when ellm-save-conversations
          (save-buffer))
        (read-only-mode 1)
        (org-up-heading-safe)
        (ellm--display-org-buffer)
        (when ellm-auto-export
          (ellm-export-conversation))))))

(defun ellm--insert-heading-and-metadata (title id model temperature)
  "Insert an Org heading with properties TITLE, ID, MODEL, and TEMPERATURE."
  (org-insert-heading)
  (insert title "  ")
  (org-insert-time-stamp nil t t)
  (newline)
  (org-set-property "ID" id)
  (org-set-property "MODEL" model)
  (org-set-property "TEMPERATURE" (number-to-string temperature)))

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

(defun ellm--parse-response-openai (response)
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

(defun ellm--parse-response-anthropic (response)
  "Extract the text from the json `RESPONSE'."
  (condition-case error
      (let* ((messages (gethash "content" response))
             (first-message (aref messages 0))
             (content (gethash "text" first-message)))
        (ellm--log-response-content content)
        (replace-regexp-in-string "\\\\\"" "\"" content))
    (wrong-type-argument (ellm--log-response-error error))))

(defun ellm--add-or-update-title (title conversation)
  "Add or update the `TITLE' in the `CONVERSATION'.

A generic `Untitled [TIMESTAMP]' title is used if `TITLE' is nil."
  (let* ((previous-title (alist-get 'title conversation))
         (new-title (or title previous-title)))
    (setf (alist-get 'title conversation)
        (or new-title "Untitled"))))

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

(defun ellm--convert-messages-to-org (messages)
  "Convert `MESSAGES' to an Org-formatted string using Pandoc."
  (let ((markdown-string (ellm--messages-to-markdown-string messages)))
    (ellm--log-markdown-messages markdown-string)
    (let ((org-string (ellm--markdown-to-org-sync markdown-string)))
      (ellm--log-org-messages org-string)
      org-string)))

(defun ellm--messages-to-markdown-string (messages)
  "Convert the MESSAGES to a markdown string."
  (mapconcat #'ellm--stringify-message messages "\n\n"))

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

(defun ellm--markdown-to-org-sync (markdown-string)
  "Convert `MARKDOWN-STRING' from Markdown to Org using Pandoc."
  (let ((pandoc-command "pandoc -f markdown -t org --shift-heading-level-by=1"))
    (with-temp-buffer
      (org-mode)
      (insert markdown-string)
      (shell-command-on-region (point-min) (point-max) pandoc-command (current-buffer) t ellm--log-buffer-name)
      (buffer-string))))

(defun ellm--resume-conversation (id &optional prompt)
  "Resume a previous conversation.

When `ID' is nil, resume the latest conversation
appearing in `ellm--conversations-file'.
Optionally, the `PROMPT' for the next user message can be passed as
an argument.
When `ellm-save-conversations' is non-nil, the conversation
at point will be removed from the org document and the updated conversation
will be inserted at the top of the document."
  (let ((conversation-pos
         (or (org-id-find id 'marker)
             (error "Conversation with id %s not found" id))))
      (save-excursion
        (goto-char conversation-pos)
        (let ((conversation (ellm--parse-conversation)))
          (setf (alist-get 'system conversation) (concat ellm-system-message ellm--system-message-suffix)
                (alist-get 'max_tokens conversation) ellm-max-tokens
                (alist-get 'temperature conversation) ellm-temperature)
          (ellm--set-model (alist-get 'model conversation))
          (ellm-chat conversation prompt)))))

(defun ellm--parse-conversation ()
  "Parse the current org subtree into a conversation object.

Note that any trailing timestamp in the conversation title is removed."
  (let* (result
         (subtree (ellm--conversation-at-point))
         (title (org-element-property :raw-value subtree))
         (msg (message "***TITLE: %s" title))
         (effective-title (when (string-match org-element--timestamp-regexp title)
                            (s-trim (substring title 0 (match-beginning 0)))))
         (metadata (ellm--metadata-from-subtree subtree))
         (messages (ellm--messages-from-subtree subtree)))
      (setq result metadata)
      (push (cons 'title effective-title) result)
      (push (cons 'messages messages) result)
      (push (cons 'system (concat ellm-system-message ellm--system-message-suffix)) result)
      result))

(defun ellm--conversation-at-point ()
  "Return the conversation at point as an org element subtree."
  (unless (or (equal (buffer-name (current-buffer)) ellm--temp-conversations-buffer-name)
              (f-equal-p (buffer-file-name (current-buffer)) ellm--conversations-file))
    (error "Not in the conversations file"))
  (when (org-before-first-heading-p)
    (error "Point is not within a conversation"))
  (save-excursion
    (org-up-heading-safe)
    (unless (and (looking-at org-heading-regexp)
                 (= (org-element-property :level (org-element-at-point)) 1))
      (error "Failed to find top-level heading"))
    (org-narrow-to-subtree)
    (let ((conversation
           (unwind-protect (car (org-element-contents (org-element-parse-buffer))))))
      (widen)
      conversation)))

(defun ellm--metadata-from-subtree (subtree)
  "Extract metadata from an org `SUBTREE' conversation."
  (let ((id (org-element-property :ID subtree))
        (model (org-element-property :MODEL subtree))
        (temperature (string-to-number (org-element-property :TEMPERATURE subtree))))
    `((id . ,id) (model . ,model) (temperature . ,temperature))))

(defun ellm--messages-from-subtree (subtree)
  "Extract messages from an org `SUBTREE' conversation."
  (let (result)
    (org-element-map subtree 'headline
      (lambda (hl)
        (let* ((title (org-element-property :raw-value hl))
               (content (org-element-contents (car (org-element-contents hl)))))
          (when (and (member title '("User" "Assistant")) content)
            (let ((role (intern (concat ":" (downcase title))))
                  (content-string (ellm--org-plain-text (cdr content))))
              (push `((role . ,role) (content . ,content-string)) result))))))
    (nreverse result)))

(defun ellm--org-plain-text (org-element)
  "Get the plain text content of the given `ORG-ELEMENT'."
  (with-temp-buffer
    (insert (org-element-interpret-data org-element))
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun ellm-chat-at-point (&optional prompt)
  "Resume the conversation at point.

The current buffer must be visiting `ellm-conversations-file' and
the point be within some conversation subtree.
In that case, that conversation is resumed with the next user message.
Optionally, the content of that message can be passed as the `PROMPT' argument."
  (interactive)
  (unless (or (and (f-equal-p (buffer-file-name (current-buffer)) ellm--conversations-file)
                   (not (org-before-first-heading-p)))
              (equal (buffer-name) ellm--temp-conversations-buffer-name))
    (user-error "Point is not within a conversation"))
  (let ((id (org-entry-get (point) "ID" t)))
    (ellm--resume-conversation id prompt)))

(defun ellm-chat-external (prompt &optional id)
  "Entry point for making a `PROMPT' via `emacsclient'.

The content of the next (or first) user message is passed
as the `PROMPT' argument. Optionally, the `ID' of a previous
conversation can be specified to continue that conversation."
  (setq ellm-auto-export t)
  (if id (ellm--resume-conversation id prompt)
    (ellm-chat nil prompt)))

(defun ellm--display-org-buffer ()
  "Prepare the conversations buffer for viewing."
  (org-overview)
  (ellm--goto-first-top-level-heading)
  (org-fold-show-subtree)
  (display-buffer (current-buffer)))

(defun ellm-show-conversations-buffer ()
  "Show the conversations in the `ellm--conversations-file'."
  (interactive)
  (let ((buffer (find-file-other-window ellm--conversations-file)))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (read-only-mode 1)
      (org-overview))))

(defun ellm-export-conversation ()
  "Mark the current conversation in the `ellm--conversations-file'."
  (interactive)
  (save-excursion
    (org-up-heading-safe)
    (while (not (= (org-element-property :level (org-element-at-point)) 1))
      (org-up-heading-safe))
    (let ((html-file (org-html-export-to-html nil 'subtree)))
      (browse-url html-file))))

(defun ellm--get-first-conversation-id ()
  "Get the ID of the first conversation in the `ellm--conversations-file'."
  (with-current-buffer (find-file-noselect ellm--conversations-file)
    (goto-char (point-min))
    (org-goto-first-child)
    (org-entry-get (point) "ID")))

(defun ellm--goto-first-top-level-heading ()
  "Go to the first top-level heading in the current buffer."
  (goto-char (point-min))
  (org-goto-first-child))

(defun ellm--setup-persistance ()
  "Register ellm configuration variables with the `savehist' package."
  (let ((symbols-to-add '(ellm-max-tokens
                          ellm-model-size
                          ellm-provider
                          ellm-temperature
                          ellm-save-conversations
                          ellm--debug-mode)))
    (dolist (symbol symbols-to-add)
      (cl-pushnew symbol savehist-additional-variables))))

(defun ellm--log (data &optional label should-encode-to-json)
  "Log `DATA' with an optional `LABEL'.
Will call `json-encode' on `DATA' if
`SHOULD-ENCODE-TO-JSON' is set to a non-nil value."
  (let* ((json-data (if should-encode-to-json (json-encode data) data))
         (formatted-log (format "{\"TIMESTAMP\": \"%s\", \"%s\": %s}"
                                (format-time-string ellm--time-format-string)
                                (or label "INFO")
                                json-data)))
    (with-current-buffer (get-buffer-create ellm--log-buffer-name)
      (goto-char (point-max))
      (unless (bolp) (newline))
      (save-excursion
        (insert formatted-log)
        (newline))
      (json-pretty-print (point) (point-max)))))

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
  (let ((err (plist-get status :error)))
    (ellm--log (http-status-code (number-to-string (car (last err)))) "HTTP-ERROR" t)))

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

(defun ellm--log-conversation (conversation)
  "Log the `CONVERSATION'."
  (when ellm--debug-mode (ellm--log conversation "CONVERSATION" t)))

(defun ellm--log-markdown-messages (markdown-string)
  "Log `MARKDOWN-STRING'."
  (when ellm--debug-mode
    (ellm--log markdown-string
               "MESSAGES-FOR-MARKDOWN" t)))

(defun ellm--log-org-messages (org-string)
  "Log `ORG-STRING'."
  (when ellm--debug-mode
    (ellm--log org-string
               "MESSAGES-FOR-ORG-MODE" t)))

(defun ellm--log-escaped-org-messages (org-string)
  "Log `ORG-STRING'."
  (when ellm--debug-mode
    (ellm--log org-string
               "ESCAPED-MESSAGES-FOR-ORG-MODE" t)))

(defun ellm-fold-conversations-buffer ()
  "Fold all conversations in the `ellm--conversations-file'."
  (interactive)
  (with-current-buffer (get-file-buffer ellm--conversations-file)
    (goto-char (point-min))
    (org-overview)
    (ellm--goto-first-top-level-heading)))

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

(require 'imenu)

(defun ellm--extract-definitions-using-imenu ()
  "Extract all variable and function definitions in the current buffer using imenu."
  (interactive)
  (let ((imenu-auto-rescan t)  ; Ensure imenu index is up-to-date
        (functions '())
        (variables '()))
    ;; Generate or update the imenu index
    (imenu--make-index-alist t)
    ;; Loop through the imenu index
    (dolist (item imenu--index-alist)
      (cond
       ;; Check if the item is a function definition
       ((string-match-p "\\`Functions" (car item))
        (dolist (func (cdr item))
          (let ((func-name (intern (car func))))
            (push `((,(car func) . ,(condition-case nil
                                         (documentation func-name)
                                       (error "No documentation available"))))
                   functions))))
       ;; Check if the item is a variable definition
       ((string-match-p "\\`Variables" (car item))
        (dolist (var (cdr item))
          (let ((var-name (intern (car var))))
            (if (and (boundp var-name) (functionp (symbol-value var-name)))
                (push `((,(car var) . "Customizable variable of type function.")) variables)
              (push `((,(car var) . ,(condition-case nil
                                          (documentation-property var-name 'variable-documentation)
                                        (error "No documentation available"))))
                     variables))))))
    ;; Combine and return the results
    `((variables . ,(nreverse variables)) (functions . ,(nreverse functions))))))

(defun ellm--setup-org-capture ()
  "Add the ellm capture template to `org-capture-templates'."
  (let* ((org-capture-template
          '(";" "LLM Prompt" entry (file "~/.llm/conversations.org")
           "* TITLE
:PROPERTIES:
:ID:       %(org-id-new)
:TIMESTAMP: %U
:END:
#+begin_drawer
MODEL: %^{Model|%(symbol-value 'ellm-model)|%(mapconcat 'cdr (append ellm--openai-models-alist ellm--anthropic-models-alist) \"|\")}
TEMPERATURE: %^{Temperature|%(number-to-string (symbol-value 'ellm-temperature))}
MAX-TOKENS: %^{Max Tokens|%(number-to-string (symbol-value 'ellm-max-tokens))}
#+end_drawer

%?
"
           :prepend t))
        (existing-index (cl-position ?\; org-capture-templates :test #'equal)))
    (if existing-index
        (setf (nth existing-index org-capture-templates) org-capture-template)
      (push org-capture-template org-capture-templates))
    (advice-add 'org-capture :around #'ellm--org-capture-to-read-only-buffer))
  nil)

(defun ellm--org-capture-to-read-only-buffer (orig-fun &rest args)
  "Used to advise `ORIG-FUN', disabling `read-only-mode' temporarily.

`ARGS' are passed to `ORIG-FUN'."
  (let ((target-buffer (find-file-noselect ellm--conversations-file)))
      (with-current-buffer target-buffer
        ;; Store original read-only state
        (let ((read-only-original buffer-read-only))
          (unwind-protect
              (progn
                ;; Temporarily disable read-only
                (read-only-mode -1)
                ;; Call original org-capture function
                (apply orig-fun args))
            (read-only-mode (if read-only-original 1 -1)))))))

(defun ellm--template-to-json (template)
  "Convert an `org-capture' `TEMPLATE' to a JSON object."
  (let* ((lines (split-string template "\n"))
         (drawer-content (cl-loop for line in lines
                                  when (string-match
                                        "^\\MODEL\\|TEMPERATURE\\|MAX-TOKENS\\): \\(.+\\)$" line)
                                  collect (cons (match-string 1 line) (match-string 2 line))))
         (prompt (cl-loop for line in lines
                          until (string-match "^%\\(.*\\)$" line)
                          finally return (match-string 1 line)))
         (model (cdr (assoc "MODEL" drawer-content)))
         (temperature (string-to-number (cdr (assoc "TEMPERATURE" drawer-content))))
         (max-tokens (string-to-number (cdr (assoc "MAX-TOKENS" drawer-content))))
         (user-message "--TODO_PLACEHOLDER")
         (json-object `(("model" . ,model)
                        ("temperature" . ,temperature)
                        ("max_tokens" . ,max-tokens)
                        ("prompt" . ,(concat ellm-system-message "\n" user-message)))))
    (json-encode json-object)))

;;;###autoload
(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ; N") #'ellm-chat-at-point)
            (define-key map (kbd "C-c ; n") #'ellm-chat)
            (define-key map (kbd "C-c ; e") #'ellm-export-conversation)
            (define-key map (kbd "C-c ; t") #'ellm-toggle-test-mode)
            (define-key map (kbd "C-c ; c") #'ellm-set-config)
            (define-key map (kbd "C-c ; ;") #'ellm-show-conversations-buffer)
            (define-key map (kbd "C-c ; s") #'ellm-toggle-save-conversations)
            (define-key map (kbd "C-c ; o") #'ellm-fold-conversations-buffer)
            map))

;;;###autoload
(define-globalized-minor-mode global-ellm-mode ellm-mode
  (lambda ()
    (ellm--setup-persistance)
    (ellm--setup-org-capture)
    (ellm-mode 1)))

(provide 'ellm)
;;; ellm.el ends here
