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
(require 'project)
(require 'request)
(require 'markdown-mode)
(require 'org-element)
(require 'org-fold)
(require 'org-id)
(require 'ox-html)
(require 'savehist)
(require 'url)

(defgroup ellm nil
  "Make API calls to LLMs."
  :group 'applications
  :prefix "ellm-")

(defvar ellm--prompt-history nil
  "Store for prompts history.")

(defcustom ellm-server-host "localhost"
  "The host to use for the webserver."
  :type 'string
  :group 'ellm)

(defcustom ellm-server-port 5040
  "The port to use for the webserver."
  :type 'integer
  :group 'ellm)

(defcustom ellm-get-api-key 'ellm-get-api-key-from-env
  "The function which retrieves your API key for the current provider."
  :type 'function
  :group 'ellm)

(defcustom ellm-password-store-path-by-provider nil
  "A function to the password store path to get api keys.
The function should take one argument, the provider as a
symbol, and return the path as a string"
  :type 'function
  :group 'ellm)

(defconst ellm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--anthropic-api-url "https://api.anthropic.com/v1/messages"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--groq-api-url "https://api.groq.com/openai/v1/chat/completions"
  "The URL to send requests to the Groq API.")

(defconst ellm--mistral-api-url "https://codestral.mistral.ai/v1/chat/completions"
  "The URL to send requests to the Mistral API.")

(defcustom ellm--conversations-dir
  (directory-file-name (expand-file-name ".ellm/" "~/"))
  "The directory to store conversation history."
  :type 'string
  :group 'ellm)

(defcustom ellm--conversations-filename-prefix "conversations-"
  "The file to store conversation history."
  :type 'string
  :group 'ellm)

(defcustom ellm-provider 'openai
  "The provider to use for API calls."
  :type '(choice
          (const :tag "OpenAI" openai)
          (const :tag "Anthropic" anthropic)
          (const :tag "Groq" groq)
          (const :tag "Mistral" mistral))
  :group 'ellm)

(defcustom ellm-model-size 'big
  "The model size to use for API calls."
  :type '(choice
          (const :tag "Big" big)
          (const :tag "Medium" medium)
          (const :tag "Small" small))
  :group 'ellm)

(defcustom ellm-model "gpt-4o"
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

(defcustom ellm--openai-models-alist `((big . "gpt-4o")
                                       (medium . "gpt-3.5-turbo")
                                       (small . "gpt-3.5-turbo"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm--anthropic-models-alist `((big . "claude-3-5-sonnet-20240620")
                                          (medium . "claude-3-5-sonnet-20240620")
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

(defcustom ellm--mistral-models-alist `((big . "codestral-latest")
                                        (medium . "mistral-large-latest")
                                        (small . "mistral-small-latest"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defvar ellm--models-alist
  `((openai . `((big . "gpt-4o")
                (medium . "gpt-3.5-turbo")
                (small . "gpt-3.5-turbo")))
    (anthropic . `((big . "claude-3-5-sonnet-20240620")
                   (medium . "claude-3-5-sonnet-20240620")
                   (small . "claude-3-haiku-20240307")))
    (groq . `((big . "llama3-70b-8192")
              (medium . "llama3-8b-8292")
              (small . "mixtral-8x7b-32768")))
    (mistral . `((big . "codestral-latest")
                 (medium . "mistral-large-latest")
                 (small . "mistral-small-latest"))))
  "Alist mapping providers to their models.")

(defcustom ellm-model-alist `(("gpt-4o" . (:provider openai :size big))
                              ("gpt-3.5-turbo" . (:provider openai :size small))
                              ("claude-3-5-sonnet-20240620" . (:provider anthropic :size big))
                              ("claude-3-5-sonnet-20240620" . (:provider anthropic :size medium))
                              ("claude-3-haiku-20240307" . (:provider anthropic :size small))
                              ("llama3-70b-8192" . (:provider groq :size big))
                              ("llama3-8b-8292" . (:provider groq :size medium))
                              ("mixtral-8x7b-32768" . (:provider groq :size small))
                              ("codestral-latest" . (:provider mistral :size big))
                              ("mistral-large-latest" . (:provider mistral :size medium))
                              ("mistral-small-latest" . (:provider mistral :size small)))
  "Alist mapping model names to their providers."
  :type 'alist
  :group 'ellm)

(defun ellm-providers-supported ()
  "List of supported providers."
  (seq-uniq (mapcar
             (lambda (model)
               (plist-get (cdr model) :provider)) ellm-model-alist)))

(defcustom ellm-save-conversations t
  "If non-nil, save the conversation history to `ellm--conversations-file'."
  :type 'boolean
  :group 'ellm)

(defcustom ellm--debug-mode nil
  "If non nil, log each request and response to the `ellm--log-buffer-name' buffer."
  :type 'boolean
  :group 'ellm)

(defvar ellm--test-mode nil
  "If non-nil, set LLM parameters to lowest token cost for testing purposes.")

(defvar ellm-org--faces-alist
  `(("5" '(:foreground "green1"))
    ("4" '(:foreground "chartreuse"))
    ("3" '(:foreground "yellow"))
    ("2" '(:foreground "orange"))
    ("1" '(:foreground "OrangeRed")))
  "Faces indicating the user ratings of conversations.")

(defvar ellm--system-message-suffix "\nFinally, you accompany any response with a short title \
which describes the discussion.
You separate the title and the response by a markdown horizontal \
rule (i.e. a line consisting of three or more dashes).
Your answers are thus formatted as follows:\n
Your title here\n
--------------------------------------------\n
Your response here.
Format your response in markdown."
  "The system message suffix to append to the system message.")

(defcustom ellm-system-messages
  `((default :type string
             :value "You are a useful general assistant integrated with Emacs.
Your goal is to execute the user's task or precisely answer their questions, \
using all the CONTEXT the user provides (if any).
Ensure your responses are relevant and adequate based on the nature of the query.
You are very cautious and give thorough justifications for each claim made.
When not confident about certain details, you say so.
If needed, request further information or clarifications from the user.
Avoid unnecessary politeness and organizational sections.
Instead, focus on providing clear, relevant examples and the technical aspects \
of the subject at hand.")
    (question-and-answer :type string
                         :value "You are an expert technical assistant integrated with Emacs.
Your primary task is to precisely and accurately answer the user's technical questions.
When answering, ensure that your responses are directly relevant to the question asked.
If you are unsure or don't have enough information to provide a confident answer, \
simply say \"I don't know\" or \"I'm not sure.\"
Avoid unnecessary politeness details and focus on accuracy and relevance.")
    (code-refactoring :type function
                      :args (:language)
                      :value (lambda (language)
                               (format "You are an expert %s programmer integrated with Emacs.
Your primary task is to assist with improving and optimizing existing code.
Identify inefficient or outdated code patterns and suggest better alternatives.
Ensure that the refactored code remains functionally equivalent to the original.
If there are multiple valid ways to refactor, present the options and recommend the most efficient one.
Provide clear, concise code samples highlighting the changes."
                                       language)))
    (code-review :type function
                 :args (:language)
                 :value (lambda (language)
                          (format "You are an expert %s programmer integrated with Emacs.
Your primary task is to assist with reviewing code, identifying potential issues, and suggesting improvements.
Focus on code logic, best practices, performance, and readability.
Provide constructive feedback, indicating both strengths and areas for improvement.
Ensure your comments are clear, specific, and actionable.
Feel free to request additional context or clarifications if necessary.
Avoid unnecessary politeness and organizational sections like a closing summary."
                                  language)))
    (code-generation :type function
                     :args (:language)
                     :value (lambda (language)
                              (format "You are an expert %s programmer integrated with Emacs.
Your primary task is to assist with code generation, ensuring accuracy and efficiency.
When generating code, use the provided CONTEXT to tailor your responses to the user's needs.
Provide well-commented, clean, and efficient code samples.
If the problem can be solved in multiple ways, briefly state the options and recommend the most efficient one.
3When you encounter incomplete or ambiguous instructions, seek clarifications from the user.
Maintain a focus on technical precision and completeness without redundant explanations or politeness."
                                      language))))
  "Alist mapping system message names to their content.
The content of those messages are the system messages that will be used as
instructions for the language models in new conversations."
  :safe #'always
  :type '(alist :key-type symbol :value-type plist)
  :group 'ellm)

(defvar ellm-current-system-message 'default
  "The index of the current system message in `ellm-system-messages'.")

(defvar ellm-prompt-context-fmt-string
  "## CONTEXT:\n%s\n\n## PROMPT:\n%s\n"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--add-context-from-region' for usage details.")

(defvar ellm-prompt-context-fmt-string-anthropic
  "<context>\n%s\n</context>\n\n<prompt>\n%s\n</prompt>"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--add-context-from-region' for usage details.")

(defconst ellm-org--buffer-props
  '(:STARTUP overview
    :SETUPFILE "~/repos/org-html-themes/org/theme-readtheorg-local.setup"
    :OPTIONS toc:nil)
  "Properties to set for conversations buffers.")

(defvar ellm-auto-export nil
  "If non-nil, the chat is automatically exported when the response is received.")

(defvar ellm--is-external nil
  "If non-nil, the chat is being conducted in an external chat buffer.")

(defvar ellm--last-conversation-exported-id nil
  "The ID of the last conversation exported to an external chat buffer.")

(defconst ellm--log-buffer-name "*ELLM-Logs*"
  "Log buffer for LLM messages.")

(defconst ellm--temp-conversations-buffer-name "*LLM Conversations*"
  "The file to store conversation history.")

(defvar ellm-provider-configurations
  `((openai . ((prepare-request-headers . ellm--prepare-request-headers-default)
               (prepare-request-body . ellm--prepare-request-body-default)
               (parse-response . ellm--parse-response-openai)
               (models-alist . ,ellm--openai-models-alist)))
    (anthropic . ((prepare-request-headers . ellm--prepare-request-headers-anthropic)
                  (prepare-request-body . ellm--prepare-request-body-anthropic)
                  (parse-response . ellm--parse-response-anthropic)
                  (models-alist . ,ellm--anthropic-models-alist)))
    (groq . ((prepare-request-headers . ellm--prepare-request-headers-default)
             (prepare-request-body . ellm--prepare-request-body-default)
             (parse-response . ellm--parse-response-openai)
             (models-alist . ,ellm--groq-models-alist)))
    (mistral . ((prepare-request-headers . ellm--prepare-request-headers-default)
                (prepare-request-body . ellm--prepare-request-body-default)
                (parse-response . ellm--parse-response-openai)
                (models-alist . ,ellm--mistral-models-alist))))
  "Alist mapping providers to their API configurations.")

(defun ellm-get-api-key-from-env ()
  "Get the API key from the environment for the `PROVIDER'."
  (getenv (format "%s_API_KEY" (upcase (symbol-name ellm-provider)))))

(declare-function password-store-get "password-store.el" (entry))

(defun ellm-get-api-key-from-password-store ()
  "Get the API key from the password store.
The password store entry is gotten by evaluating
the `ellm-password-store-path-by-provider' function with the value
of `ellm-provider'."
  (password-store-get (funcall ellm-password-store-path-by-provider ellm-provider)))

; TODO Use this to configure the providers
(defmacro ellm-define-provider (name &rest config)
  "Define a new provider with given `NAME' and `CONFIG'.
The `CONFIG' should be a plist with the following keys:
- `api-key': a function to get the API key.
- `prepare-request-headers': a function to prepare the request headers.
- `prepare-request-body': a function to prepare the request body.
- `parse-response': a function to parse the response.
- `models-alist': an alist mapping model sizes to model names."
  `(defcustom ,(intern (format "ellm-%s-config" name))
     ',config
     ,(format "Configuration for %s API." name)
  :type '(plist :key-type symbol :value-type sexp)
  :group 'ellm))

(defun ellm-get-provider-config (provider)
  "Get the configuration for the `PROVIDER'."
  (symbol-value (intern (format "ellm-%s-config" (symbol-name provider)))))

(defun ellm--get-system-messages ()
  "Get the system messages."
  (let ((system-messages (copy-alist ellm-system-messages)))
    (if (boundp 'ellm-project-system-messages)
      (append ellm-project-system-messages system-messages)
      system-messages)))

(defun ellm--get-system-message (&rest args)
  "Get the system message based on `ellm-current-system-message'.
The `ARGS' should be keyword arguments corresponding to the signature of the
system message function, if there are any."
  (unless ellm--test-mode
    (let* ((message-entry (alist-get ellm-current-system-message (ellm--get-system-messages)))
           (message-type (plist-get message-entry :type))
           (effective-system-message
            (cond
             ((eq message-type 'function)
              (let ((func (plist-get message-entry :value))
                    (arg-keys (plist-get message-entry :args)))
                (apply func (mapcar (lambda (key) (plist-get args key)) arg-keys))))
             ((eq message-type 'string)
              (plist-get message-entry :value)))))
      (concat effective-system-message ellm--system-message-suffix))))

(defun ellm--get-provider-configuration (provider)
  "Get the configuration for the `PROVIDER'."
  (alist-get provider ellm-provider-configurations))

(defun ellm-set-system-message (&optional system-message)
  "Set the `SYSTEM-MESSAGE' to use for the next prompt."
  (interactive)
  (let ((sm (or system-message
                (intern (completing-read
                         "Choose system message: "
                         (mapcar #'(lambda (cons-message)
                                     (format
                                      (propertize (symbol-name (car cons-message))
                                                  'face 'font-lock-string-face)))
                                 (ellm--get-system-messages)))))))
    (setq ellm-current-system-message sm)
    (message "...system message set to %s..." sm)))

(defun ellm-set-provider (&optional provider)
  "Set the API `PROVIDER' to use."
  (interactive)
  (let* ((p (or provider
                (intern (completing-read
                         "Choose provider: "
                         (mapcar #'(lambda (item)
                                     (format
                                      (propertize (symbol-name item)
                                                  'face 'font-lock-type-face)))
                                 (ellm-providers-supported))))))
         (config (ellm--get-provider-configuration p))
         (models-alist (alist-get 'models-alist config)))
    (setq ellm-model (alist-get ellm-model-size models-alist)
          ellm-provider p))
  (message "...provider set to %s, model set to %s..." ellm-provider ellm-model))

(defun ellm-set-model-size (&optional model-size)
  "Set the `MODEL-SIZE' to use."
  (interactive)
  (let* ((ms (or model-size
                 (intern (completing-read
                          "Choose model size: "
                          (mapcar #'(lambda (item)
                                      (format
                                       (propertize (symbol-name item) 'face 'font-lock-type-face)))
                                  '(big medium small))))))
         (config (ellm--get-provider-configuration ellm-provider))
         (models-alist (alist-get 'models-alist config)))
    (setq ellm-model (alist-get ms models-alist)
          ellm-model-size ms))
  (message "...model-size set to %s, model set to %s..." ellm-model-size ellm-model))

(defun ellm--validation-max-tokens (max-tokens)
  "Validate the `MAX-TOKENS' value."
  (when (and (integerp max-tokens) (> max-tokens 0) (<= max-tokens 4096))
    max-tokens))

(defun ellm-set-max-tokens (&optional max-tokens)
  "Set the `MAX-TOKENS' to use for the LLM prompt."
  (interactive)
  (let ((mt max-tokens))
    (if (called-interactively-p 'interactive)
        (while (null (setq mt (ellm--validation-max-tokens max-tokens)))
          (setq max-tokens (read-number "Max tokens (between 1 and 4096): ")))
      (unless (setq mt (ellm--validation-max-tokens max-tokens))
        (error "Invalid argument: `%s' should be integer between 1 and 4096" max-tokens)))
  (setq ellm-max-tokens mt)
  (message "...max-tokens set to %s..." ellm-max-tokens)))


(defun ellm--validation-temperature (temperature)
  "Validate the `TEMPERATURE' value."
  (when (and (numberp temperature) (>= temperature 0.0) (<= temperature 2.0))
    temperature))

(defun ellm-set-temperature (&optional temperature)
  "Set the `TEMPERATURE' to use for the LLM prompt."
  (interactive)
  (let ((temp temperature))
    (if (called-interactively-p 'interactive)
        (while (null (setq temp (ellm--validation-temperature temperature)))
          (setq temperature (read-number "Temperature (between 0.0 and 2.0): ")))
      (unless (setq temp (ellm--validation-temperature temperature))
        (error "Invalid argument: `%s' should be number between 0.0 and 2.0" temperature)))
    (setq ellm-temperature temp)
    (message "...temperature set to %s..." ellm-temperature)))

(defun ellm-set-config ()
  "Call the `SETTING-FUNCTION' according to the user's choice."
  (interactive)
  (while-let ((config-function (ellm--config-prompt)))
    (funcall-interactively config-function)))

(defun ellm--config-prompt ()
  "Prompt the user to choose a setting to configure."
  (let ((choices (list
                  (cons (ellm--toggle-test-mode-description) #'ellm-toggle-test-mode)
                  (cons (ellm--toggle-save-conversations-description) #'ellm-toggle-save-conversations)
                  (cons (ellm--toggle-debug-mode-description) #'ellm-toggle-debug-mode)
                  (cons (ellm--provider-description) #'ellm-set-provider)
                  (cons (ellm--model-size-description) #'ellm-set-model-size)
                  (cons (ellm--temperature-description) #'ellm-set-temperature)
                  (cons (ellm--max-tokens-description) #'ellm-set-max-tokens)
                  (cons (ellm--system-message-description) #'ellm-set-system-message))))
    (alist-get
     (completing-read "Choose a setting to configure: " (mapcar 'car choices))
     choices nil nil 'equal)))

(defun ellm--system-message-description ()
  "Return a string describing the current system message."
  (format "System Message                  %s"
          (propertize (symbol-name ellm-current-system-message)
                      'face 'font-lock-string-face)))

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
      (when-let ((temp-buffer (get-buffer ellm--temp-conversations-buffer-name))
                 (inhibit-read-only t))
        (kill-buffer temp-buffer))
      (message "...ellm-test-mode disabled..."))
    (progn
      (put 'ellm-max-tokens 'previous-value (symbol-value 'ellm-max-tokens))
      (put 'ellm-model-size 'previous-value (symbol-value 'ellm-model-size))
      (put 'ellm-save-conversations 'previous-value (symbol-value 'ellm-save-conversations))
      (ellm-set-max-tokens 10)
      (ellm-set-model-size 'small)
      (setq ellm-save-conversations nil)
      (setq ellm--test-mode t)
      (with-current-buffer
          (get-buffer-create ellm--temp-conversations-buffer-name)
        (org-mode)
        (goto-char (point-min))
        (let ((plist ellm-org--buffer-props))
          (while plist
            (let ((prop (substring (symbol-name (car plist)) 1))
                  (value (cadr plist)))
              (insert (format "#+%s: %s\n" prop value)))
            (setq plist (cddr plist))))
      (message "...ellm-test-mode enabled...")))))

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
  '((python-ts-mode . "python")
    (python-mode . "python")
    (emacs-lisp-mode . "emacs-lisp")
    (org-mode . "org")
    (lua-mode . "lua")
    (c-or-c++-mode . "cpp")
    (c-or-c++-ts-mode . "cpp")
    (rjsx-mode . "typescript")
    (sh-mode . "shell"))
  "Alist mapping major modes to Org mode source block languages.")

(defun ellm--add-context-from-region (prompt)
  "Use the active region if any to attach context to the `PROMPT' string.
Use the `ellm-prompt-context-fmt-string' template to add context from that
region in the form of a markdown code block.
To determine the language label of the code block,
a lookup to `ellm--major-mode-to-org-lang-alist' with the buffer's major mode
is used, except for the special case of `org-mode' where we use the
function `ellm--add-context-from-region-org' (which see)."
  (if (use-region-p)
      (let* ((r-beg (region-beginning))
             (r-end (region-end))
             (mode major-mode)
             (lang (alist-get mode ellm--major-mode-to-org-lang-alist
                              "text" nil 'string=))
             (string-template
              (if (eq ellm-provider 'anthropic)
                  ellm-prompt-context-fmt-string-anthropic
                ellm-prompt-context-fmt-string)))
        (deactivate-mark)
        (save-excursion
          (goto-char r-beg)
          (setq r-beg (pos-bol))
          (goto-char r-end)
          (setq r-end (pos-eol)))
        (when (eq mode 'org-mode)
          (let ((adjusted-values
                 (ellm--add-context-from-region-org r-beg r-end lang)))
            (setq r-beg (nth 0 adjusted-values)
                  r-end (nth 1 adjusted-values)
                  lang (nth 2 adjusted-values))))
        (format string-template
                lang (buffer-substring-no-properties r-beg r-end) prompt))
    prompt))

(defun ellm--add-context-from-region-org (r-beg r-end lang)
  "Special context function for `ellm-org' buffers.
Considering the region between `R-BEG' and `R-END', if it is
within a code block, use the code block's language instead
of `LANG'. Adjust the region to the body of the code block if needed.
This function returns a list of the form (R-BEG R-END LANG) with
the possibly adjusted values."
  (save-excursion
    (goto-char r-beg)
    (let* ((element (org-element-at-point-no-context))
           (element-begin (org-element-property :begin element))
           (element-end (org-element-property :end element)))
      (when (and (eq (org-element-type element) 'src-block)
                 (<= element-begin r-beg)
                 (>= element-end r-end))
        (progn
          (setq lang (org-element-property :language element))
          (let ((region-begin-at-block-begin-p
                 (= (line-number-at-pos r-beg) (line-number-at-pos element-begin))))
            (when region-begin-at-block-begin-p
              (goto-char element-begin)
              (setq r-beg (+ (line-end-position) 1)))
            (goto-char (- element-end (+ (org-element-property :post-blank element) 1)))
            (goto-char (line-beginning-position))
            (when (<= (point) r-end)
              (setq r-end (- (point) 1))))))))
  (list r-beg r-end lang))

(defmacro ellm--append-to! (place element)
  "Append ELEMENT to the end of the list stored in PLACE.
Similar to `push', but for the end of the list."
  `(setf ,place
         (if ,place
             (nconc ,place (list ,element))
           (list ,element))))

(defun ellm--make-message (role content &optional image-path)
  "Create a message with the given `ROLE' and `CONTENT'.
If `IMAGE-PATH' is provided, create an image message instead."
  (if image-path
      (ellm--make-image-message role image-path)
    `((role . ,role) (content . ,(encode-coding-string content 'utf-8)))))

(defun ellm--make-image-message (role image-path)
  "Create an image message with the given `ROLE' and `IMAGE-PATH'."
  (let* ((file-extension (file-name-extension image-path))
         (media-type (cond ((member file-extension '("jpg" "jpeg")) "image/jpeg")
                           ((string= file-extension "png") "image/png")
                           (t (error "Unsupported image format: %s" file-extension))))
         (base64-data (with-temp-buffer
                        (insert-file-contents-literally image-path)
                        (base64-encode-string (buffer-string) t))))
    `((role . ,role)
      (content . ((type . "image")
                  (source . ((type . "base64")
                             (media_type . ,media-type)
                             (data . ,base64-data))))))))

(defun ellm--add-system-message (content conversation)
  "Add a system message with `CONTENT' to the `CONVERSATION'.
The system message is prepended to the `messages' list."
  (push (ellm--make-message :system content)
        (alist-get 'messages conversation)))

(defun ellm--add-user-message (conversation content)
  "Append a user message with `CONTENT' to the `CONVERSATION'.
The user message is appended to the `messages' list."
  (ellm--append-to! (alist-get 'messages conversation)
                    (ellm--make-message :user content)))

(defun ellm--add-assistant-message (conversation content)
  "Append an assistant message with `CONTENT' to the `CONVERSATION'.
The assistant message is appended to the `messages' list."
  (ellm--append-to! (alist-get 'messages conversation)
                    (ellm--make-message :assistant content)))

(defun ellm--initialize-conversation (prompt)
  "Initialize a new conversation starting with `PROMPT'.
Return the conversation-data alist."
  (let* ((system-message-args
          (when (derived-mode-p 'prog-mode)
            (list :language
                  (or (alist-get major-mode ellm--major-mode-to-org-lang-alist)
                      (thread-last (symbol-name major-mode)
                                   (string-remove-suffix "-mode")
                                   (string-remove-suffix "-ts"))
                      "computer"))))
         (conversation
          (list
           (cons 'messages (list))
           (cons 'temperature ellm-temperature)
           (cons 'max_tokens ellm-max-tokens)
           (cons 'model ellm-model)
           (cons 'title nil)
           (cons 'system (apply #'ellm--get-system-message system-message-args)))))
         (ellm--add-user-message conversation prompt)
         conversation))

(defun ellm--prepare-request-headers (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations))
         (prepare-fn (alist-get 'prepare-request-headers config)))
    (funcall prepare-fn)))

(defun ellm--prepare-request-headers-default ()
  "Prepare the headers for API requests as done for openai models.

The `GET-API-KEY' function is used to retrieve the api key for the provider."
  `(("Authorization" . ,(concat "Bearer " (funcall ellm-get-api-key)))
    ("Content-Type" . "application/json; charset=utf-8")))

(defun ellm--prepare-request-headers-anthropic ()
  "Prepare the headers for API requests made to anthropic models.

The `GET-API-KEY' function is used to retrieve the api key for anthropic."
  `(("x-api-key" . ,(funcall ellm-get-api-key))
    ("anthropic-version" . "2023-06-01")
    ("Content-Type" . "application/json; charset=utf-8")))

(defun ellm--serialize-conversation (conversation)
  "Serialize the `CONVERSATION' to a JSON string."
  (let ((messages
         (vconcat
          (seq-map
           (lambda (msg)
             `((role . ,(substring (symbol-name (alist-get 'role msg)) 1))
               (content . ,(alist-get 'content msg))))
           (alist-get 'messages conversation)))))
    (json-serialize (cons `(messages . ,messages) conversation))))

(defun ellm--prepare-request-body-default (conversation)
  "Prepare the messages list with a new user message based on `CONVERSATION'.
The SYSTEM entry, if non-nil, is removed and made into a system message intead.
Also remove the TITLE and ID entries."
  (let ((conversation-copy (copy-alist conversation)))
    (when-let ((system-directive (alist-get 'system conversation))
               (messages-copy (cl-copy-list (alist-get 'messages conversation))))
      (setf (alist-get 'messages conversation-copy) messages-copy)
      (ellm--add-system-message system-directive conversation-copy))
    (setf (alist-get 'system conversation-copy nil 'remove) nil
          (alist-get 'title conversation-copy nil 'remove) nil
          (alist-get 'id conversation-copy nil 'remove) nil)
    (ellm--serialize-conversation conversation-copy)))

(defun ellm--prepare-request-body-anthropic (conversation)
  "Prepare the API call body to send `CONVERSATION'.
Remove the SYSTEM entry in case it is nil.
Also remove the TITLE and ID entries."
  (let ((conversation-copy (copy-alist conversation))
        (system-directive (alist-get 'system conversation)))
    (setf (alist-get 'system conversation-copy nil 'remove) system-directive
          (alist-get 'title conversation-copy nil 'remove) nil
          (alist-get 'id conversation-copy nil 'remove) nil)
    (ellm--serialize-conversation conversation-copy)))

(defun ellm--prepare-request-body (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations)))
    (funcall (alist-get 'prepare-request-body config) conversation)))

(defun ellm--get-url (conversation)
  "Get the URL to send the request to based on `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (provider-name (symbol-name provider))
         (api-url-var (intern (format "ellm--%s-api-url" provider-name))))
    (if (boundp api-url-var)
        (symbol-value api-url-var)
      (error "ellm--get-url: Unknown provider or missing base url: %s"
             provider-name))))

(defun ellm-conversations-buffer-from-project ()
  "Get the conversations buffer associated to the current project.
The project is determined by calling `project-current' (which see).
In case the function was called interactively, `project-current' is
called with a non-nil MAYBE-PROMPT argument.
If `FORCE-PROMPT' is non-nil, instead prompt the user for which
project to target."
  (interactive)
  (let* ((maybe-prompt (called-interactively-p 'interactive))
         (filename-suffix
          (if-let ((current (project-current maybe-prompt)))
              (project-name current)
            "general"))
         (filepath (f-join ellm--conversations-dir
                           (concat ellm--conversations-filename-prefix
                                   filename-suffix
                                   ".org"))))
    (find-file-noselect filepath)))

(defun ellm-chat (&optional current-conversation conversations-buffer prompt-wrapper user-prompt)
  "Send a chat request to the current provider's completion endpoint.
This function is the main entry point for interacting with the `ellm' package.
Optional arguments:
- CURRENT-CONVERSATION: Conversation object to continue. Start a new one if nil.
- CONVERSATIONS-BUFFER: File path to save the conversation. If nil, use default.
- PROMPT-WRAPPER: Function to wrap the prompt.
- USER-PROMPT: Prompt string. If nil, read interactively.
The function sends the request, handles the response, and updates the
conversation.
This function always returns nil."
  (interactive)
  (let* ((prompt-message (if current-conversation
                             "Enter your next prompt: "
                           "Enter your prompt: "))
         (prompt (or user-prompt (read-string prompt-message nil 'ellm--prompt-history)))
         (wrapper (or prompt-wrapper 'identity))
         (wrapped-prompt (funcall wrapper prompt))
         (buffer (or conversations-buffer
                     (if ellm-save-conversations
                         (ellm-conversations-buffer-from-project)
                       (get-buffer-create ellm--temp-conversations-buffer-name))))
         (conversation (if current-conversation
                           (progn
                             (ellm--add-user-message current-conversation wrapped-prompt)
                             current-conversation)
                         (ellm--initialize-conversation wrapped-prompt)))
         (url (ellm--get-url conversation))
         (request-headers (ellm--prepare-request-headers conversation))
         (request-body (ellm--prepare-request-body conversation))
         (url-request-method "POST")
         (url-request-extra-headers request-headers)
         (url-request-data request-body))
      (ellm--log `((url . ,url)
                   (headers . ,request-headers)
                   (body . ,request-body)) "REQUEST")
      (url-retrieve url #'ellm--handle-response (list buffer conversation))
      (message "...Waiting for response from %s..."
               (symbol-name (ellm--get-model-provider conversation)))
  nil))

(defun ellm--handle-response (status conversations-buffer conversation)
  "Handle the response to the prompt made using `CONVERSATION'.
Information about the response is contained in `STATUS' (see `url-retrieve').
The response is added to the `CONVERSATION' and the conversation is added or
updated in the `CONVERSATIONS-BUFFER'."
  (message "...Received response from %s..."
           (symbol-name (ellm--get-model-provider conversation)))
  (cond ((plist-get status :error)
         (message "http error: %s" (elt (plist-get status :error) 2))
         (ellm--log status "HTTP-ERROR"))
        ((plist-get status :redirect)
         (ellm--log status "HTTP-REDIRECT"))
        (t
         (goto-char url-http-end-of-headers)
         (if (<= (point-max) (point))
             (ellm--log "No response body (DNS resolve or TCP error)" "REQUEST-ERROR")
           (let ((response (ellm--parse-json-response)))
             (when response
               (ellm--add-response-to-conversation conversations-buffer conversation response)
               (message "...Conversation updated..."))
             response)))))

(defun ellm--add-response-to-conversation (conversations-buffer conversation response)
  "Process the `RESPONSE' from the API call made with config `CONVERSATION'.
The `RESPONSE' should be an emacs-lisp hash table.
The `CONVERSATION' is then added to the `CONVERSATIONS-BUFFER'."
  (unless (hash-table-p response)
    (error "ellm--add-response-to-conversation: Expected cons, got %s" (type-of response)))
  (let* ((provider (ellm--get-model-provider conversation))
         (config (ellm--get-provider-configuration provider))
         (parse-response (alist-get 'parse-response config))
         (response-content (funcall parse-response response)))
    (ellm--handle-assistant-response response-content conversation)
    (ellm--log conversation "CONVERSATION")
    (ellm--insert-conversation-into-org conversations-buffer conversation)))

(defun ellm--handle-assistant-response (response conversation)
  "Add the `RESPONSE' to the `CONVERSATION'.
The `RESPONSE' is expected to be a string."
  (unless (stringp response)
    (error "ellm--handle-assistant-response: Expected string, got %s" (type-of response)))
  (let* ((split-response (ellm--split-response response))
         (title (elt split-response 0))
         (content (elt split-response 1)))
    (ellm--add-or-update-title title conversation)
    (ellm--add-assistant-message conversation content)))

(defun ellm--insert-conversation-into-org (conversations-buffer conversation)
  "Insert the `CONVERSATION' into the org file at `CONVERSATIONS-BUFFER'."
  (let* ((messages (alist-get 'messages conversation))
         (number-previous-messages (- (length messages) 2))
         (previous-messages (take number-previous-messages messages))
         (new-messages (nthcdr number-previous-messages messages))
         (title (alist-get 'title conversation))
         (model (alist-get 'model conversation))
         (temperature (alist-get 'temperature conversation))
         (id (alist-get 'id conversation))
         (stringified-previous-messages (ellm--messages-to-string previous-messages "**"))
         (org-formatted-new-messages (ellm--convert-messages-to-org new-messages))
         (messages-to-insert (concat stringified-previous-messages
                                     (unless (string-empty-p stringified-previous-messages) "\n\n")
                                     org-formatted-new-messages)))
    (ellm--log messages-to-insert "MESSAGES-TO-INSERT")
    (with-current-buffer conversations-buffer
      (let ((inhibit-read-only t))
        (if-let ((pos (and ellm-save-conversations (org-id-find id 'marker))))
            (progn (goto-char pos)
                   (org-cut-subtree))
          (setq id (or id (org-id-new))))
        (goto-char (point-min))
        (ellm--insert-heading-and-metadata title id model temperature)
        (insert messages-to-insert)
        (when ellm-save-conversations
          (save-buffer)))
      (ellm--display-conversations-buffer conversations-buffer 'highlight-last-message)
      (when ellm-auto-export
        (ellm-export-conversation)))))

(defun ellm--insert-heading-and-metadata (title id model temperature)
  "Insert an Org heading with properties `TITLE', `ID', `MODEL', and `TEMPERATURE'."
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
        (ellm--log response-body "RESPONSE")
        (json-parse-string response-body))
    (json-parse-error
     (ellm--log error "JSON-PARSE-ERROR")
     nil)))

(defun ellm--parse-response-openai (response)
  "Extract the text from the json `RESPONSE'.
This function is meant to be used with the response from the OpenAI API."
  (condition-case error
      (let* ((choices (gethash "choices" response))
             (first-choice (aref choices 0))
             (msg (gethash "message" first-choice))
             (content (gethash "content" msg)))
        (ellm--log content "RESPONSE-CONTENT")
        (replace-regexp-in-string "\\\\\"" "\"" content))
    (wrong-type-argument (ellm--log error "RESPONSE-ERROR"))))

(defun ellm--parse-response-anthropic (response)
  "Extract the text from the json `RESPONSE'."
  (condition-case error
      (let* ((messages (gethash "content" response))
             (first-message (aref messages 0))
             (content (gethash "text" first-message)))
        (ellm--log content "RESPONSE-CONTENT")
        (replace-regexp-in-string "\\\\\"" "\"" content))
    (wrong-type-argument (ellm--log error "RESPONSE-ERROR"))))

(defun ellm--add-or-update-title (title conversation)
  "Add or update the `TITLE' in the `CONVERSATION'.
A generic `Untitled  [TIMESTAMP]' title is used if `TITLE' is nil."
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
  (let ((markdown-string (ellm--messages-to-string messages)))
    (ellm--log markdown-string "MESSAGES-FOR-MARKDOWN")
    (let ((org-string (ellm--markdown-to-org-sync markdown-string)))
      (ellm--log org-string "MESSAGES-FOR-ORG")
      org-string)))

(defun ellm--messages-to-string (messages &optional headline-char)
  "Convert the MESSAGES to a markdown string.
Use `HEADLINE-CHAR' as the character for headlines."
  (mapconcat (lambda (message) (ellm--stringify-message message headline-char)) messages "\n\n"))

(defun ellm--stringify-message (message &optional headline-char)
  "Convert the `MESSAGE' to a string.
A message of the form `((role . :role) (content . \"content\"))'
is converted to the string \"[HC] Role\ncontent\" where [HC]
is `HEADLINE-CHAR' or \"#\" by default."
  (let* ((role (capitalize (substring (symbol-name (alist-get 'role message)) 1)))
         (content (alist-get 'content message))
         (hc (or headline-char "#")))
    (format "%s %s\n\n%s" hc role content)))

(defconst ellm--lua-filter-path
  (expand-file-name "format_lua.lua" (locate-library "ellm"))
  "Path to the Lua filter used to format the org output from Pandoc.")

(defun ellm--markdown-to-org-sync (markdown-string)
  "Convert `MARKDOWN-STRING' from Markdown to Org using Pandoc."
  (let* ((pandoc-command
          (concat "pandoc"
                  " -f markdown+tex_math_single_backslash"
                  " -t org"
                  " --shift-heading-level-by=1"
                  " --lua-filter="
                  ellm--lua-filter-path))
         (org-string
          (with-temp-buffer
            (insert markdown-string)
            (shell-command-on-region (point-min) (point-max)
                                     pandoc-command
                                     (current-buffer) 'no-mark
                                     ellm--log-buffer-name)
            (buffer-substring-no-properties (point-min) (point-max)))))
         org-string))

(defun ellm--resume-conversation (&optional id prompt)
  "Resume a previous conversation with given `ID'.
Optionally, the `PROMPT' for the next user message can be passed as
an argument.
When `ID' is nil, it is assumed that the current point is within a conversation
in the `ellm--temp-conversations-buffer-name' buffer.
When `ellm-save-conversations' is non-nil, the conversation
at point will be removed from the org document and the updated conversation
will be inserted at the top of the document."
  (let* ((pom (if id (org-id-find id 'marker)
                (save-excursion (ellm--goto-conversation-top) (point-marker))))
         (conversation (ellm--parse-conversation pom))
         (buffer (marker-buffer pom)))
    (ellm--set-model (alist-get 'model conversation))
    (ellm-set-temperature (alist-get 'temperature conversation))
    (setf (alist-get 'system conversation) (ellm--get-system-message)
          (alist-get 'max_tokens conversation) ellm-max-tokens)
    (ellm-chat conversation buffer prompt)))

(defun ellm--conversation-subtree ()
  "Return the current conversation subtree.
The returned object is an org parse tree."
  (save-restriction
    (progn
      (org-narrow-to-subtree)
      (car (org-element-contents (org-element-parse-buffer))))))

(defun ellm--parse-conversation (&optional posn)
  "Parse the org subtree starting at `POSN' into a conversation object."
  (let* (conversation
         (properties (org-entry-properties posn))
         (metadata (ellm-org--get-conversation-metadata properties))
         (title (alist-get "ITEM" properties nil nil 'equal))
         (effective-title (when (string-match org-element--timestamp-regexp title)
                            (s-trim (substring title 0 (match-beginning 0)))))
         (messages (ellm-org--get-conversation-messages posn)))
      (setq conversation metadata)
      (push (cons 'title effective-title) conversation)
      (push (cons 'messages messages) conversation)
      (push (cons 'system (ellm--get-system-message)) conversation)
      conversation))

(defun ellm-org--get-conversation-metadata (properties)
  "Extract metadata from the org `PROPERTIES' alist."
  (let ((id (alist-get "ID" properties nil nil 'equal))
        (model (alist-get "MODEL" properties nil nil 'equal))
        (temperature (string-to-number (alist-get "TEMPERATURE" properties nil nil 'equal) 10)))
    `((id . ,id) (model . ,model) (temperature . ,temperature))))

(defun ellm-org--get-conversation-messages (posn)
  "Reconstruct the messages from the conversation at `POSN'."
  (let (messages)
    (save-excursion
      (goto-char posn)
      (unless (ellm-org--at-headline-level-p 1)
        (error "ellm-org--reconstruct-messages: Point should be at the beginning of a conversation"))
      (org-down-element)
      (unless (ellm-org--at-headline-level-p 2)
        (org-next-visible-heading 1))
      (when (ellm-org--at-headline-level-p 2)
        (push (ellm-org--message-at-point) messages))
      (while (org-get-next-sibling)
        (push (ellm-org--message-at-point) messages)))
    (nreverse messages)))

(defun ellm-org--message-at-point ()
  "Captures the current message as a string.
It is assumed that the point is located at the beginning of a message."
  (let ((role
         (intern
          (concat ":" (downcase
                       (org-element-property :raw-value
                                             (org-element-at-point-no-context))))))
        (content
         (save-excursion
           (save-restriction
             (org-narrow-to-subtree)
             (org-down-element)
             (when (org-at-property-drawer-p)
               (org-forward-element))
             (buffer-substring-no-properties (point) (point-max))))))
    `((role . ,role) (content . ,(s-trim content)))))

(defun ellm-org--at-headline-level-p (level &optional posn)
  "Check if the point is at a headline of the given `LEVEL'.
If `POSN' is nil, the current point is used."
  (save-excursion
    (goto-char (or posn (point)))
    (and (org-at-heading-p)
         (= (org-element-property :level (org-element-at-point-no-context)) level))))

(defun ellm-org--plain-text (data)
  "Get the plain text content of the given org `DATA'.
The `DATA' must be as the data accepted by `org-element-interpret-data'
\(which see\)."
  (with-temp-buffer
    (insert (org-element-interpret-data data))
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun ellm--conversations-buffer-p (buffer)
  "Check if the `BUFFER' is an ellm conversations buffer.
This could either be the buffer determined
by `ellm--temp-conversations-buffer-name', or a buffer visiting one
of the conversations files. The conversations files are those org files
in the `ellm--conversations-dir' directory, with filename starting with
the `ellm--conversations-filename-prefix' prefix."
  (or (equal (buffer-name) ellm--temp-conversations-buffer-name)
      (with-current-buffer buffer
        (and
         (f-equal-p (f-dirname (buffer-file-name (current-buffer)))
                    ellm--conversations-dir)
         (string-prefix-p ellm--conversations-filename-prefix
                          (f-filename (buffer-file-name (current-buffer))))))))

(defun ellm--point-at-conversation-p ()
  "Return non-nil if point is within a conversation subtree.
The current buffer needs to be either one of the org conversations
files or the temporary conversations buffer."
  (and
   (derived-mode-p 'org-mode)
   (ellm--conversations-buffer-p (current-buffer))
   (not (org-before-first-heading-p))))

(defun ellm--at-temp-conversations-buffer-p ()
  "Check if the current buffer is the temporary conversations buffer.
That buffer's is set by the value of `ellm--temp-conversations-buffer-name'.
This is the buffer used to output conversations when `ellm--test-mode'
is toggle on."
  (equal (buffer-name) ellm--temp-conversations-buffer-name))

(defun ellm-chat-at-point (&optional prompt)
  "Resume the conversation at point.
The current buffer must be visiting `ellm-conversations-file' and
the point be within some conversation subtree.
In that case, that conversation is resumed with the next user message.
Optionally, the content of that message can be passed as the `PROMPT' argument."
  (interactive)
  (unless (ellm--point-at-conversation-p)
    (user-error "Point is not within a conversation"))
  (let ((id
         (unless (ellm--at-temp-conversations-buffer-p)
           (org-entry-get (point) "ID" t))))
    (ellm--resume-conversation id prompt)))

(defun ellm-chat-external (selection &optional id)
  "Entry point for making a prompt via `emacsclient'.
The context of the next (or first) user message is passed
as the `SELECTION' argument. Optionally, the `ID' of a previous
conversation can be specified to continue that conversation."
  (setq ellm-auto-export t)
    (when selection
      (with-temp-buffer
        (goto-char (point-min))
        (set-mark (point))
        (insert selection)
        (if id
            (ellm--resume-conversation id)
          (ellm-chat)))))

(defun ellm--display-conversations-buffer (buffer &optional highlight-last-message)
  "Display the conversations in `BUFFER'.
The window is scrolled to the last user message of the last conversation,
and the point is placed at the next assistant message.
If `HIGHLIGHT-LAST-MESSAGE' is non-nil, the last user and assistant messages
is temporarily highlighted to indicate they are new messages."
  (unless (ellm--conversations-buffer-p buffer)
    (error "Buffer is not a conversations buffer"))
  (with-current-buffer buffer
    (display-buffer (current-buffer))
    (ellm-org--apply-rating-face)
    (read-only-mode 1)
    (org-overview)
    (ellm--goto-first-top-level-heading)
    (org-fold-show-subtree)
    (if-let* ((headlines
               (org-element-map (ellm--conversation-subtree) 'headline 'identity))
              (last-assistant-message
               (cl-find-if
                (lambda (hl) (string-match-p "Assistant" (org-element-property :raw-value hl)))
                headlines :from-end t))
              (last-assistant-message-begin
               (org-element-property :begin last-assistant-message)))
        (with-selected-window (get-buffer-window (current-buffer))
          (goto-char last-assistant-message-begin)
          (recenter-top-bottom 4)
          (when highlight-last-message
            (save-excursion
              (org-end-of-subtree)
              (pulse-momentary-highlight-region last-assistant-message-begin (point)))))
      (message "Last user message not found"))))

(defun ellm-show-conversations-buffer ()
  "Show the relevant conversations file.
Which file that is is determined by the current project
via `ellm-conversations-buffer-from-project' (which see)."
  (interactive)
  (let* ((fun-call
          (if (called-interactively-p 'interactive)
              #'call-interactively
            #'funcall))
         (buffer
          (if ellm-save-conversations
              (funcall fun-call #'ellm-conversations-buffer-from-project)
            (get-buffer-create ellm--temp-conversations-buffer-name))))
    (ellm--display-conversations-buffer buffer)))

(defun ellm-find-conversations-file-other-window ()
  "Open a conversation file in another window.
The file is selected from the directory specified by `ellm--conversations-dir'
and the file prefix specified by `ellm--conversations-filename-prefix'."
  (interactive)
  (let* ((default-directory (expand-file-name ellm--conversations-dir))
         (conversation-files
          (directory-files
            default-directory t
            (concat "^" (regexp-quote ellm--conversations-filename-prefix) ".*\\.org$")))
         (chosen-file (completing-read "Choose conversation file: " conversation-files)))
    (when chosen-file
      (find-file-other-window chosen-file))))

(defun ellm-export-conversation ()
  "Mark the current conversation in the `ellm--conversations-file'."
  (interactive)
  (save-excursion
    (ellm--goto-conversation-top)
    (let* ((html-file (org-html-export-to-html nil 'subtree)))
      (browse-url html-file))))

(defun ellm--goto-conversation-top ()
    "Go to the top of the current conversation in the current buffer.
This function navigates to the top-level headline (level 1) of the
current conversation. It first checks if the point is within a
conversation, and if not, it throws an error. It then uses
`org-element-lineage' to get all the ancestors of the element at
point and, if found, moves the point to the first headline of
level 1 found.
This function is more reliable than using `org-back-to-header' or
`org-up-heading-safe' to navigate the document structure, as it will
work even if the headline levels are not singly incremented.
Returns the resulting POINT if the point is moved to the top of the
conversation, or throws an error otherwise."
  (unless (ellm--point-at-conversation-p)
    (user-error "Point is not within a conversation"))
  (if-let* ((ancestors (org-element-lineage (org-element-at-point-no-context) nil t))
            (top-headline
             (cl-find-if (lambda (hl)
                           (ellm-org--at-headline-level-p
                            1 (org-element-property :begin hl)))
                         ancestors
                         :from-end)))
        (goto-char (org-element-property :begin top-headline))
      (error "No top-level headline found")))

(defun ellm--goto-first-top-level-heading ()
  "Move the point to the first top-level heading in the current buffer.
This function is specifically designed for Org mode buffers. It starts at the
beginning of the buffer and then navigates to the first top-level heading.
If the buffer is empty or does not contain any top-level headings, the point
will remain at the beginning of the buffer.
Note: This function relies on the `org-goto-first-child` function from the
Org mode library. Ensure that Org mode is properly installed and loaded before
using this function."
  (goto-char (point-min))
  (org-goto-first-child))

(defun ellm--setup-persistance ()
  "Register ellm configuration variables with the `savehist' package.
This function is responsible for maintaining the persistence of certain
configuration variables used by the ellm package. It does this by adding
those variables to the `savehist-additional-variables' list. This ensures
that the values of these variables are saved and restored across Emacs
sessions.
The variables that are registered for persistence are
`ellm-current-system-message', `ellm-max-tokens', `ellm-model-size',
`ellm-provider', `ellm-temperature' and `ellm--debug-mode'.
Additionally, the `ellm--prompt-history' is persisted to maintain the prompt
history.
This function does not take any arguments and returns nil."
  (let ((symbols-to-add '(ellm-current-system-message
                          ellm-max-tokens
                          ellm-model-size
                          ellm-provider
                          ellm-temperature
                          ellm--debug-mode
                          ellm--prompt-history)))
    (dolist (symbol symbols-to-add)
      (cl-pushnew symbol savehist-additional-variables))))

(defun ellm--log (data &optional label _)
  "Log `DATA' with an optional `LABEL'."
  (when ellm--debug-mode
    (let* ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
           (log-entry `(("TIMESTAMP" . ,timestamp)
                        (,(or label "INFO") . ,data))))
      (with-current-buffer (get-buffer-create ellm--log-buffer-name)
        (goto-char (point-max))
        (unless (bolp) (newline))
        (save-excursion
          (insert (json-encode log-entry))
          (newline))
        (json-pretty-print (point) (point-max))))))

(defun ellm-org--apply-rating-face ()
  "Apply custom face to org headlines based on their RATING property."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (when-let* ((rating (org-entry-get (point) "RATING"))
                  (face (alist-get rating ellm-org--faces-alist nil nil 'equal))
                  (title-begin (line-beginning-position))
                  (title-end (re-search-forward org-element--timestamp-regexp nil t)))
        (remove-overlays title-begin title-end 'ellm-rating t)
        (let ((ov (make-overlay title-begin title-end)))
          (overlay-put ov 'ellm-rating t)
          (overlay-put ov 'face face))))))

(defun ellm-org-rate-response-and-refresh (&optional rating)
  "Rate the current org headline with a RATING and apply the corresponding face."
  (interactive)
  (unless (ellm--point-at-conversation-p)
    (user-error "Point is not within a conversation"))
  (if rating
      (progn
        (when (numberp rating)
          (setq rating (number-to-string rating)))
        (unless (member rating (mapcar 'car ellm-org--faces-alist))
      (error "Invalid rating. Please enter a value between 1 and 5")))
    (setq rating (completing-read "Rate the conversation (1-5): " '(1 2 3 4 5))))
  (save-excursion
    (ellm--goto-conversation-top)
    (org-set-property "RATING" rating)
    (save-buffer)
    (save-restriction
      (org-narrow-to-element)
      (ellm-org--apply-rating-face))))

(defun ellm-org-fold-conversations-buffer ()
  "Fold all conversations in the `ellm--conversations-file'."
  (interactive)
  (org-overview)
  (ellm--goto-first-top-level-heading))

(defun ellm-org-next-message ()
  "Move to the next User or Assistant message based on TYPE."
  (interactive)
  (save-restriction
    (save-excursion
      (ellm--goto-conversation-top)
      (org-narrow-to-subtree)
      (org-fold-show-subtree))
    (let ((message-regex "^\\*\\{1,8\\} +\\(User\\|Assistant\\)")
          match-pos)
      (save-excursion
        (when (looking-at message-regex) (forward-line))
        (setq match-pos (re-search-forward message-regex nil t)))
      (when match-pos
        (goto-char (match-beginning 0))))))

(defun ellm-org-previous-message ()
  "Move to the previous User or Assistant message.
When at the top of the conversation, fold the subtree."
  (interactive)
  (save-restriction
    (save-excursion
      (ellm--goto-conversation-top)
      (org-narrow-to-subtree))
    (let ((regex "^\\*\\{1,8\\} +\\(User\\|Assistant\\)"))
      (when (re-search-backward regex nil t)
        (goto-char (match-beginning 0))))))

(defun ellm--extract-symbol-definition-at-point ()
  "Extract the definition of the symbol at point."
  (let* ((symbol (symbol-at-point))
         (documentation-fn (lambda (s)
                             (if (fboundp s)
                                 (or (documentation s) "not documented")
                               (or (documentation-property s 'variable-documentation) "not documented"))))
         (doc (funcall documentation-fn (symbol-at-point))))
    (cons symbol doc)))

; TODO Make this more portable by using different search tools as available.
(defun ellm-search-in-conversations (&optional pattern)
  "Search for headlines matching `PATTERN' in the conversations files."
  (interactive)
  (let ((effective-pattern
         (or pattern
             (read-string "Enter search pattern: ")))
        (files
         (f-files ellm--conversations-dir
                  (lambda (f)
                    (and
                     (string-prefix-p ellm--conversations-filename-prefix (f-filename f))
                     (string-suffix-p ".org" (f-filename f)))))))
    (consult-ripgrep files effective-pattern)))

(defun ellm--describe-symbols (pattern)
  "Describe the Emacs Lisp symbols matching `PATTERN'."
  (interactive "sDescribe symbols matching: ")
  (let* ((fun-list '())
         (var-list '())
         (doc-func
          (lambda (s)
            (if (fboundp s)
                (push (cons s (or (documentation s) "not documented")) fun-list)
              (when (boundp s)
                (push (cons s (or (documentation-property s 'variable-documentation) "not documented")) var-list))))))
    (mapatoms (lambda (sym)
                (when (string-match pattern (symbol-name sym))
                    (funcall doc-func sym))))
    `((variables . ,(nreverse var-list)) (functions . ,(nreverse fun-list)))))

(defun ellm--extract-definitions-in-buffer ()
  "Extract all variable and function definitions in the current buffer."
  (interactive)
  (let ((functions '())
        (variables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^(\\(?:defvar\\|defcustom\\) " nil t)
        (push (ellm--extract-symbol-definition-at-point) variables))
      (goto-char (point-min))
      (while (re-search-forward "^(defun " nil t)
        (push (ellm--extract-symbol-definition-at-point) functions)))
    `((variables . ,(nreverse variables)) (functions . ,(nreverse functions)))))

(defun ellm--extract-and-show-definitions-in-json ()
  "Extract and print all variable and function definitions in the current buffer."
  (interactive)
  (let* ((definitions (ellm--describe-symbols "ellm-"))
         (json-string (json-serialize definitions)))
    (with-current-buffer (get-buffer-create "*Definitions JSON*")
      (erase-buffer)
      (insert json-string)
      (json-pretty-print-buffer)
      (display-buffer (current-buffer)))))

(defun ellm-extract-and-save-definitions-in-json (filename)
  "Extract and save all definitions in the current buffer to `FILENAME'.
Note that `FILENAME' should be an absolute path to the file."
  (interactive "FEnter filename to save definitions: ")
  (let* ((definitions (ellm--describe-symbols "ellm-"))
         (json-string (json-serialize definitions)))
    (with-temp-file filename
      (insert json-string)
      (newline))))

(defvar ellm--server-process nil
  "The process object for the ellm server.")

(defconst ellm--server-buffer-name "*ellm server*"
  "The name of the buffer used to display the ellm server output.")

(defun ellm--server-running-p ()
  "Return non-nil if the ellm server is running."
  (and ellm--server-process (process-live-p ellm--server-process)))

(defun ellm-start-server ()
  "Start the ellm Node.js server."
  (interactive)
  (unless (ellm--server-running-p)
    (let ((default-directory (file-name-directory (locate-library "ellm")))
          (prev-provider ellm-provider))
      (ellm-set-provider 'openai)
      (let ((openai-api-key (ellm-get-api-key-from-env)))
        (unless openai-api-key
          (setenv "OPENAI_API_KEY" (funcall ellm-get-api-key))))
      (ellm-set-provider 'anthropic)
      (let ((anthropic-api-key (ellm-get-api-key-from-env)))
        (unless anthropic-api-key
          (setenv "ANTHROPIC_API_KEY" (funcall ellm-get-api-key))))
      (ellm-set-provider prev-provider))
    (setq ellm--server-process
          (start-process "ellm-server" ellm--server-buffer-name
                         "node" "--trace-deprecation"
                         (expand-file-name "server.js")
                         "--host" ellm-server-host
                         "--port" (number-to-string ellm-server-port)))
    (message "ellm webserver started")))

(defun ellm-stop-server ()
  "Stop the ellm Node.js server."
  (interactive)
  (when (ellm--server-running-p)
    (kill-process ellm--server-process)
    (setq ellm--server-process nil)
    (message "ellm webserver stopped")))

(defun ellm-text-to-speech (&optional text)
  "Convert `TEXT' to speech, or the content of the active region if `TEXT' is nil."
  (interactive)
  (unless text
    (unless (use-region-p)
      (user-error "No active region to convert to speech"))
    (setq text (buffer-substring-no-properties (region-beginning) (region-end)))
    (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
           (filepath (concat "~/.ellm/speech/" timestamp ".wav"))
           (url (concat ellm-server-host ":" (number-to-string ellm-server-port) "/tts"))
           (data `(("input" . ,text)
                  ("filepath" . ,filepath))))
      (request
        url
        :type "POST"
        :data (json-encode `(("data" . ,data)))
        :headers '(("Content-Type" . "application/json"))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (progn
                      (start-process "aplay-process" nil "aplay" "-q" filepath)
                      (message data))))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (message "Error: %S" error-thrown)))))))

(defconst ellm--context-buffer-name "*ellm context*"
  "The name of the buffer used to display the context chunks.")

(defface ellm-context-buffer-face
  '((((background dark)) (:background "#328C0411328C" :extend t))  ; Very dark magenta
    (t                   (:background "#CD73FBEECD73" :extend t))) ; Very pale green
  "Face for context overlays in the ellm context buffer."
  :group 'ellm)

(defvar ellm-context-buffer-face 'ellm-context-buffer-face)

(defface ellm-context-face
  '((((background dark)) (:background "#111313F03181" :extend t))  ; Very dark blue
    (t                   (:background "#EEECEC0FCE7E" :extend t))) ; Very pale goldenrod
  "Face for context overlays in their original buffer."
  :group 'ellm)

(defvar ellm-context-face 'ellm-context-face)

(defvar ellm-context-overlays nil
  "List of overlays representing context chunks.")

(defun ellm--context-buffer-setup ()
  "Setup the context buffer."
  (with-current-buffer (get-buffer-create ellm--context-buffer-name)
    (ellm-context-buffer-mode)
    (current-buffer)))

(defun ellm--get-or-create-context-buffer ()
  "Return the context buffer, setting it up it does not exist."
  (or (get-buffer ellm--context-buffer-name)
      (ellm--context-buffer-setup)))

(defun ellm--context-at (posn)
  "Return the context overlay at position `POSN'."
  (let ((overlays (overlays-at posn)))
    (seq-find #'ellm--overlay-context-overlay-p overlays)))

(defun ellm--contexts-in-region (beg end)
  "Return the context overlays in the region between `BEG' and `END'."
  (let ((overlays (overlays-in beg end)))
    (seq-filter (lambda (ov) (overlay-get ov 'ellm-context)) overlays)))

(defun ellm--context-buffer-overlay (overlay)
  "Return the context overlay in the context buffer corresponding to `OVERLAY'."
  (unless (and (ellm--overlay-context-overlay-p overlay)
               (buffer-live-p (overlay-buffer overlay)))
    (error "Not a valid context chunk overlay: %S" overlay))
  (if (buffer-match-p (ellm--get-or-create-context-buffer) (overlay-buffer overlay))
      overlay (overlay-get overlay 'ellm-other-overlay)))

(defun ellm--overlay-context-overlay-p (overlay)
  "Return non-nil if `OVERLAY' is a context overlay."
  (overlay-get overlay 'ellm-context))

(defun ellm--overlay-empty-p (overlay)
  "Return non-nil if `OVERLAY' is empty or deleted."
  (or (null (overlay-buffer overlay))
      (not (buffer-live-p (overlay-buffer overlay)))
      (= (overlay-start overlay) (overlay-end overlay))))

(defun ellm-add-context-chunk ()
   "Create an overlay for the current region and add it to `ellm-context-overlays'."
   (interactive)
   (if (use-region-p)
       (let* ((start (region-beginning))
              (end (region-end))
              (overlay (make-overlay start end)))
         (seq-do #'ellm-remove-context-chunk (ellm--contexts-in-region start end))
         (ellm--insert-context-content overlay)
         (overlay-put overlay 'ellm-context t)
         (overlay-put overlay 'face ellm-context-face)
         (overlay-put overlay 'evaporate t)
         (push overlay ellm-context-overlays)
         (deactivate-mark)
         (message "Context added"))
     (message "No region selected")))

(defun ellm--prepare-context-chunk (overlay)
  "Prepare the context chunk of `OVERLAY' for making a prompt.
If the `OVERLAY' has a non-nil `ellm-language' property, the
text content of the context chunk is wrapped in triple backticks
and the value of the `ellm-language' is used as the language
attribute."
  (with-current-buffer (overlay-buffer overlay)
    (s-trim-right
     (let* ((buffer (overlay-buffer (overlay-get overlay 'ellm-other-overlay)))
            (language (with-current-buffer buffer
                        (or (alist-get major-mode ellm--major-mode-to-org-lang-alist)
                            (and (derived-mode-p 'prog-mode)
                                 (thread-last
                                   (symbol-name major-mode)
                                   (string-remove-suffix "-mode")
                                   (string-remove-suffix "-ts")))
                            "text")))
            (start (overlay-start overlay))
            (end (overlay-end overlay))
            (content (buffer-substring-no-properties start end)))
       (if language
           (format "```%s\n%s\n```\n\n" language content)
         (format "```\n%s\n```\n\n" content))))))

(defun ellm--insert-context-content (overlay)
   "Insert the content of the `OVERLAY' into the context buffer.
We create a corresponding overlay in the context buffer to keep track
of the original context chunks."
   (let ((start (overlay-start overlay))
         (end (overlay-end overlay))
         (source-buffer (overlay-buffer overlay)))
     (with-current-buffer ellm--context-buffer-name
       (goto-char (point-max))
       (let ((inhibit-read-only t)
             (insert-start (point)))
         (insert (with-current-buffer source-buffer
                   (buffer-substring start end)))
         (let* ((insert-end (point))
                (new-overlay (make-overlay insert-start insert-end)))
           (newline 2)
           (overlay-put new-overlay 'ellm-context t)
           (overlay-put new-overlay 'face 'ellm-context-buffer-face)
           (overlay-put new-overlay 'ellm-other-overlay overlay)
           (overlay-put overlay 'ellm-other-overlay new-overlay))))))

(defun ellm--try-delete-context-content (overlay)
  "Delete the content of the `OVERLAY' from the context buffer.
If the overlay is not in the context buffer, is empty, deleted or
is not found, do nothing."
  (when (and overlay
             (not (ellm--overlay-empty-p overlay))
             (buffer-match-p (ellm--get-or-create-context-buffer) (overlay-buffer overlay))
    (with-current-buffer (overlay-buffer overlay)
      (let ((inhibit-read-only t)
            (start (overlay-start overlay))
            (end (overlay-end overlay)))
        (when (and start end)
          (delete-region start end)
          (delete-blank-lines)))))))

(defun ellm-remove-context-chunk (&optional overlay)
  "Remove the content of the `OVERLAY' from the context buffer."
  (interactive)
  (when overlay
    (unless (ellm--overlay-context-overlay-p overlay)
      (error "Not a valid context overlay: %S" overlay)))
  (unless (or overlay (setq overlay (ellm--context-at (point))))
    (error "No context overlay at point: %S" (point)))
  (let* ((context-buffer-ov (ellm--context-buffer-overlay overlay))
         (context-ov (if (null context-buffer-ov) overlay
                       (overlay-get context-buffer-ov 'ellm-other-overlay))))
    (ellm--try-delete-context-content context-buffer-ov)
    (delete-overlay context-buffer-ov)
    (delete-overlay context-ov)
    (setq ellm-context-overlays (delq context-ov ellm-context-overlays))))

(defun ellm-view-context-buffer ()
  "View the context overlays in the current buffer."
  (interactive)
  (let ((buffer (ellm--get-or-create-context-buffer)))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (recenter 4)))

(defun ellm-clear-context ()
  "Remove all context overlays."
  (interactive)
  (seq-do #'delete-overlay ellm-context-overlays)
  (setq ellm-context-overlays nil)
  (when-let ((context-buffer (get-buffer ellm--context-buffer-name)))
    (when (buffer-live-p context-buffer)
      (with-current-buffer (ellm--get-or-create-context-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (seq-do #'delete-overlay (overlays-in (point-min) (point-max)))))))
  (message "All context chunks cleared"))

(defun ellm-context--build-prompt-wrapper ()
  "Use the context accumulated in the context buffer for a prompt."
  (if-let* ((string-template
             (if (eq ellm-provider 'anthropic)
                 ellm-prompt-context-fmt-string-anthropic ellm-prompt-context-fmt-string))
            (context-overlays
             (seq-filter
              #'ellm--overlay-context-overlay-p
              (with-current-buffer (ellm--get-or-create-context-buffer)
                (overlays-in (point-min) (point-max)))))
            (context-string
             (mapconcat (lambda (ov) (ellm--prepare-context-chunk ov))
                        context-overlays)))
      (lambda (prompt)
        (format string-template context-string prompt))))

(defun ellm-context-complete ()
  "Complete the context in the context buffer."
  (interactive)
  (let ((prompt-wrapper (ellm-context--build-prompt-wrapper)))
    (ellm-chat nil nil prompt-wrapper)))

(defvar ellm-context-buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ellm-complete-context)
    (define-key map (kbd "d") 'ellm-remove-context-chunk)
    map)
  "Keymap for `ellm-context-buffer-mode'.")

(define-derived-mode ellm-context-buffer-mode special-mode "ellm-context"
  "Major mode for managing the ellm context buffer."
  (use-local-map ellm-context-buffer-keymap))

;;;###autoload
(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :lighter " ellm"
  :keymap (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ;") (make-sparse-keymap))
    (define-key map (kbd "C-c ; N") #'ellm-chat-at-point)
    (define-key map (kbd "C-c ; n") #'ellm-context-complete)
    (define-key map (kbd "C-c ; e") #'ellm-export-conversation)
    (define-key map (kbd "C-c ; r") #'ellm-org-rate-response-and-refresh)
    (define-key map (kbd "C-c ; c") #'ellm-set-config)
    (define-key map (kbd "C-c ; ;") #'ellm-show-conversations-buffer)
    (define-key map (kbd "C-c ; s") #'ellm-search-in-conversations)
    (define-key map (kbd "C-c ; o") #'ellm-org-fold-conversations-buffer)
    (define-key map (kbd "C-c ; j") #'ellm-org-next-message)
    (define-key map (kbd "C-c ; k") #'ellm-org-previous-message)
    (define-key map (kbd "C-c ; m") #'ellm-add-context-chunk)
    (define-key map (kbd "C-c ; d") #'ellm-remove-context-chunk)
    (define-key map (kbd "C-c ; x") #'ellm-view-context-buffer)
    (define-key map (kbd "C-c ; C-M-k") #'ellm-clear-context)
    map)
  :global nil)

;;;###autoload
(define-globalized-minor-mode global-ellm-mode ellm-mode
  (lambda ()
    (unless global-ellm-mode
      (dolist (buffer (list ellm--context-buffer-name
                            ellm--log-buffer-name
                            ellm--temp-conversations-buffer-name))
        (ignore-errors (kill-buffer buffer))))
    (ellm-mode))
  :global t
  :group 'ellm)

(add-hook 'kill-emacs-hook 'ellm-stop-server)

(provide 'ellm)
;;; ellm.el ends here
