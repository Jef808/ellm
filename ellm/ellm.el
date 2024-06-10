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
(require 'markdown-mode)
(require 'org-element)
(require 'org-fold)
(require 'org-id)
(require 'ox-html)
(require 'savehist)
(require 'url)

(defgroup ellm nil
  "Make API calls to LLMs."
  :group 'tools
  :prefix "ellm-")

(defcustom ellm-server-port 5040
  "The port to use for the webserver."
  :type 'integer
  :group 'ellm)

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

(defcustom ellm-get-mistral-api-key
  #'ellm--get-mistral-api-key-from-env
  "A function which retrieves your Mistral API key."
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

(defcustom ellm--temp-conversations-buffer-name "*LLM Conversations*"
  "The file to store conversation history."
  :type 'string
  :group 'ellm)

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

(defcustom ellm--mistral-models-alist `((big . "codestral-latest")
                                        (medium . "mistral-large-latest")
                                        (small . "mistral-small-latest"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm-model-alist `(("gpt-4o" . (:provider openai :size big))
                              ("gpt-3.5-turbo" . (:provider openai :size small))
                              ("claude-3-opus-20240229" . (:provider anthropic :size big))
                              ("claude-3-sonnet-20240229" . (:provider anthropic :size medium))
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

(defvar ellm--providers-supported '(anthropic openai groq mistral)
  "List of supported providers.")

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

(defvar ellm-org--faces-alist
  `((rating-5 '(:foreground "gold"))
    (rating-4 '(:foreground "green"))
    (rating-3 '(:foreground "yellow"))
    (rating-2 '(:foreground "orange"))
    (rating-1 '(:foreground "red")))
  "Face for the ellm-org mode.")

(defcustom ellm-system-message "You are a useful emacs-integrated general assistant.
Your goal is to execute the user's task or precisely answer their questions, using all \
the CONTEXT the user provides (if any).
You are very cautious and give thorough justifications for each claim that you make.
When you are not very confident about certain details, you say so and, if it can help, \
ask the user for clarifications needed for those details.
You avoid unnecessary politeness formulae and organizational sections. \
You instead focus on examples and the technical aspects of the subject at hand."
  "The system message to set the stage for new conversations."
  :type 'string
  :group 'ellm)

(defvar ellm--system-message-suffix "\nFinally, you accompany any response with a short title \
which describes the discussion.
You separate the title and the response by a markdown horizontal \
rule (i.e. a line consisting of three or more dashes).
Your answers are thus formatted as follows:

Your title here

--------------------------------------------

Your response here.
Format your response in markdown."
  "The system message suffix to append to the system message.")

(defcustom ellm-system-messages
  `(("default" . "You are a useful general assistant integrated with Emacs.
Your goal is to execute the user's task or precisely answer their questions, \
using all the CONTEXT the user provides (if any).
Ensure your responses are relevant and adequate based on the nature of the query.
You are very cautious and give thorough justifications for each claim made.
When not confident about certain details, you say so.
If needed, request further information or clarifications from the user.
Avoid unnecessary politeness and organizational sections.
Instead, focus on providing clear, relevant examples and the technical aspects \
of the subject at hand.")

    ("question-and-answer" . "You are an expert technical assistant integrated with Emacs.
Your primary task is to precisely and succinctly answer the user's technical questions.
When answering, ensure that your responses are directly relevant to the question asked.
If you are unsure or don't have enough information to provide a confident answer, \
simply say \"I don't know\" or \"I'm not sure.\"
Avoid unnecessary details and focus on accuracy and relevance.")

    ("code-refactoring" . "You are an expert code refactoring assistant integrated with Emacs.
Your primary task is to assist with improving and optimizing existing code.
Identify inefficient or outdated code patterns and suggest better alternatives.
Ensure that the refactored code remains functionally equivalent to the original.
If there are multiple valid ways to refactor, present the options and recommend the most efficient one.
Provide clear, concise code samples highlighting the changes.")

    ("code-review" . "You are an expert code review assistant integrated with Emacs.
Your primary task is to assist with reviewing code, identifying potential issues, and suggesting improvements.
Focus on code logic, best practices, performance, and readability.
Provide constructive feedback, indicating both strengths and areas for improvement.
Ensure your comments are clear, specific, and actionable.
Feel free to request additional context or clarifications if necessary.")

    ("code-generation" . "You are an expert code assistant integrated with Emacs.
Your primary task is to assist with code generation, ensuring accuracy and efficiency.
When generating code, use the provided CONTEXT to tailor your responses to the user's needs.
Provide well-commented, clean, and efficient code samples.
If the problem can be solved in multiple ways, briefly state the options and recommend the most efficient one.
When you encounter incomplete or ambiguous instructions, seek clarifications from the user.
Maintain a focus on technical precision and completeness without redundant explanations or politeness."))
  "Alist mapping system message names to their content.
The content of those messages are the system messages that will be used as
instructions for the language models in new conversations."
  :type 'alist
  :group 'ellm)

(defvar ellm--current-system-message "default"
  "The index of the current system message in `ellm-system-messages'.")

(defvar ellm-prompt-context-fmt-string
  "## CONTEXT:\n```%s\n%s\n```\n\n## PROMPT:\n%s"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--add-context-from-region' for usage details.")

(defvar ellm-prompt-context-fmt-string-anthropic
  "<context>\n```%s\n%s\n```\n</context>\n<prompt>\n%s\n</prompt>"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--add-context-from-region' for usage details.")

(defvar ellm-auto-export nil
  "If non-nil, the chat is automatically exported when the response is received.")

(defvar ellm--is-external nil
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
             (models-alist . ,ellm--groq-models-alist)))
    (mistral . ((base-url . ,ellm--mistral-api-url)
                (get-api-key . ,(lambda () (funcall ellm-get-mistral-api-key)))
                (prepare-request-headers . ellm--prepare-request-headers-default)
                (prepare-request-body . ellm--prepare-request-body-default)
                (parse-response . ellm--parse-response-openai)
                (models-alist . ,ellm--mistral-models-alist))))
  "Alist mapping providers to their API configurations.")

(defun ellm--get-system-message ()
  "Get the system message based on `ellm--current-system-message'."
  (concat
   (alist-get ellm--current-system-message ellm-system-messages nil nil 'equal)
   ellm--system-message-suffix))

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

(defun ellm--get-mistral-api-key-from-env ()
  "Get the Mistral API key from the environment."
  (getenv "MISTRAL_API_KEY"))

(defun ellm-set-system-message (&optional system-message)
  "Set the `SYSTEM-MESSAGE' to use for the next prompt."
  (interactive)
  (let ((sm (or system-message
                (completing-read "Choose system message: "
                                 (mapcar
                                  #'(lambda (cons-message)
                                      (format
                                       (propertize (car cons-message) 'face 'font-lock-string-face)))
                                       ellm-system-messages)))))
    (setq ellm--current-system-message sm)
    (message "...system message set to %s..." sm)))

(defun ellm-set-provider (&optional provider)
  "Set the API `PROVIDER' to use."
  (interactive)
  (let* ((p (or provider
                (intern (completing-read
                         "Choose provider: "
                         (mapcar #'(lambda (item)
                                     (format
                                      (propertize (symbol-name item) 'face 'font-lock-type-face)))
                                      ellm--providers-supported)))))
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
  (and (numberp temperature) (>= temperature 0.0) (<= temperature 2.0)))

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
   (if (ellm--validation-temperature temperature)
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
                  (cons (ellm--max-tokens-description) 'ellm-set-max-tokens)
                  (cons (ellm--system-message-description) 'ellm-set-system-message)))
        (minibuffer-local-map (copy-keymap minibuffer-local-map)))
    (define-key minibuffer-local-map (kbd "q") 'abort-recursive-edit)
    (let ((minibuffer-allow-text-properties t)
          (minibuffer-local-map minibuffer-local-map))
      (alist-get
       (completing-read "Choose a setting to configure: " (mapcar 'car choices))
       choices nil nil 'equal))))

(defun ellm--system-message-description ()
  "Return a string describing the current system message."
  (format "System Message                  %s"
          (propertize ellm--current-system-message
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

(defun ellm--model-provider (model)
  "Get the provider of the `MODEL'."
  (plist-get (alist-get model ellm-model-alist) :provider))

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
    (rjsx-mode . "js")
    (sh-mode . "shell"))
  "Alist mapping major modes to Org mode source block languages.")

(defun ellm--add-context-from-region (prompt)
  "Use the active region if any to attach context to the `PROMPT' string.
Use the `ellm-context-fmt-string' template to add context from that region
in the form of a markdown code block.
To determine the language label of the code block,
a lookup to `ellm--major-mode-to-org-lang-alist' with the buffer's major mode
is used except for the special case of `org-mode'.
If the buffer's major mode is `org-mode' and a region within an org code
block is marked, use the source block's language."
  (when (use-region-p)
    (let* ((r-beg (region-beginning))
           (r-end (region-end))
           (mode major-mode)
           (lang (or (alist-get mode ellm--major-mode-to-org-lang-alist "text")))
           (string-template
            (if (eq (ellm--model-provider ellm-model) 'anthropic)
                ellm-prompt-context-fmt-string-anthropic
              ellm-prompt-context-fmt-string)))
      (when (eq mode 'org-mode)
        (save-excursion
          (goto-char r-beg)
          (let* ((el (org-element-at-point-no-context))
                 (el-beg (org-element-property :begin el))
                 (el-end (org-element-property :end el)))
            (when (and (eq (org-element-type el) 'src-block)
                       (<= el-beg r-beg)
                       (>= el-end r-end))
              (progn
                (setq lang (org-element-property :language el))
                (let ((cmp-lines
                       (lambda (op p q)
                         (let ((p-line (progn (goto-char p)
                                              (line-number-at-pos)))
                               (q-line (progn (goto-char q)
                                              (line-number-at-pos))))
                           (funcall op p-line q-line)))))
                  (when (funcall cmp-lines #'= r-beg el-beg)
                    (goto-char el-beg)
                    (setq r-beg (+ (line-end-position) 1)))
                  (goto-char (- el-end (+ (org-element-property :post-blank el) 1)))
                  (goto-char (line-beginning-position))
                  (when (<= (point) r-end)
                    (setq r-end (- (point) 1)))))))))
      (deactivate-mark)
      (format string-template
              lang
              (buffer-substring-no-properties r-beg r-end)
              prompt))))

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
  (and (nconc (alist-get 'messages conversation)
              (list (ellm--make-message :assistant content)))
       conversation))

(defun ellm--initialize-conversation (prompt)
  "Initialize a new conversation starting with `PROMPT'.

Return the conversation-data alist."
  (let* ((contextualized-prompt (ellm--add-context-from-region prompt))
         (user-message (ellm--make-message :user contextualized-prompt)))
    `((messages . (,user-message))
      (temperature . ,ellm-temperature)
      (max_tokens . ,ellm-max-tokens)
      (model . ,ellm-model)
      (title . nil)
      (system . ,(ellm--get-system-message)))))

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

(defun ellm--serialize-conversation (conversation)
  "Serialize the `CONVERSATION' to a JSON string."
  (let (messages)
    (dolist (msg (alist-get 'messages conversation) messages)
      (push `((role . ,(substring (symbol-name (alist-get 'role msg)) 1))
              (content . ,(alist-get 'content msg)))
            messages))
    (json-serialize (seq-uniq (append `((messages . ,(vconcat messages))) conversation)))))

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
    (ellm--serialize-conversation data)))

(defun ellm--prepare-request-body (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations)))
    (funcall (alist-get 'prepare-request-body config) conversation)))
    ;;      (prepare-fn (alist-get 'prepare-request-body config)))
    ;; (funcall prepare-fn conversation)))

(defun ellm--get-route-target (conversation)
  "Get the endpoint to send the request to based on `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations)))
    (or (alist-get 'route-target config)
        (error "ellm--get-route-target: Unknown provider or missing route target: %s"
               (symbol-name provider)))))

(defun ellm--get-url (conversation)
  "Get the URL to send the request to based on `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations)))
    (or (alist-get 'base-url config)
        (error "ellm--get-url: Unknown provider or missing base url: %s"
               (symbol-name provider)))))

(defun ellm--get-conversation-file-path ()
  "Get the filename for the conversation."
  (let ((filename-suffix
         (if-let ((current (project-current)))
             (project-name current)
           "general")))
    (f-join ellm--conversations-dir
            (concat ellm--conversations-filename-prefix
                    filename-suffix
                    ".org"))))

(defun ellm-chat (&optional current-conversation conversation-filepath next-prompt)
  "Send a request to the current provider's chat completion endpoint.

This function serves as the main entry point for interacting with the `ellm`
package, which facilitates chat-based communication with a specified model
provider.

Arguments:
- `CURRENT-CONVERSATION` (optional): A conversation object representing the
ongoing
 conversation. If provided, the conversation will be continued with the next
prompt
 and the associated response. If nil, a new conversation will be initialized.
- `CONVERSATION-FILEPATH` (optional): A string representing the file path where
the conversation should be saved. If nil, the conversation will be saved to a
default location determined by the `ellm--get-conversation-file-path` function.
- `NEXT-PROMPT` (optional): A string representing the next prompt to be sent to
the model provider. If nil, the prompt will be read interactively from the
minibuffer.

Behavior:
1. The function determines the prompt message based on whether
`CURRENT-CONVERSATION` is provided.
2. It reads the prompt either from `NEXT-PROMPT` or interactively from the
minibuffer.
3. It determines the file path for saving the conversation using
`CONVERSATION-FILEPATH` or a default method.
4. It updates the conversation object by adding the user's message if
`CURRENT-CONVERSATION` is provided,
   or initializes a new conversation if not.
5. It constructs the URL for the request using the conversation object.
6. It prepares the request headers and body based on the conversation object.
7. It sets the HTTP method to \"POST\" and configures the request with the
prepared headers and body.
8. It logs the request details for debugging purposes.
9. It sends the request to the model provider's endpoint and specifies
`ellm--handle-response` as the callback
   function to handle the response.
10. It displays a message indicating that it is waiting for a response from the
model provider.

Returns:
- Always returns nil.

Example usage:
  (ellm-chat)
  (ellm-chat my-conversation)
  (ellm-chat nil nil \"What is the weather today?\")
  (ellm-chat my-conversation \"/path/to/save/conversation\" \"Tell me a joke.\")"
  (interactive)
  (let* ((prompt-message (if current-conversation
                             "Enter your next prompt: "
                           "Enter your prompt: "))
         (prompt (or next-prompt (read-string prompt-message)))
         (filepath (or conversation-filepath (ellm--get-conversation-file-path)))
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
      (ellm--log-request url request-headers request-body)
      (url-retrieve url #'ellm--handle-response (list filepath conversation))
      (message "...Waiting for response from %s..." (symbol-name (ellm--get-model-provider conversation)))
  nil))

(defun ellm--handle-response (status conversation-filepath conversation)
  "Handle the response to the prompt made using `CONVERSATION'.

Information about the response is contained in `STATUS' (see `url-retrieve').
The response is added to the `CONVERSATION' and the conversation is added or
updated in the file at `CONVERSATION-FILEPATH'."
  (message "...Received response from %s..."
           (symbol-name (ellm--get-model-provider conversation)))
  (cond ((plist-get status :error)
         (ellm--log-http-error status))
        ((plist-get status :redirect)
         (ellm--log-http-redirect status))
        (t
         (goto-char url-http-end-of-headers)
         (if (<= (point-max) (point))
             (ellm--log-no-response-body)
           (let ((response (ellm--parse-json-response)))
             (when response
               (ellm--add-response-to-conversation conversation-filepath conversation response)
               (message "...Conversation updated..."))
             response)))))

(defun ellm--add-response-to-conversation (conversation-filepath conversation response)
  "Process the `RESPONSE' from the API call made with config `CONVERSATION'.

The `RESPONSE' should be an emacs-lisp hash table.
The `CONVERSATION' is then saved to the file at `CONVERSATION-FILEPATH'."
  (unless (hash-table-p response)
    (error "ellm--add-response-to-conversation: Expected cons, got %s" (type-of response)))
  (let* ((provider (ellm--get-model-provider conversation))
         (config (ellm--get-provider-configuration provider))
         (parse-response (alist-get 'parse-response config))
         (response-content (funcall parse-response response)))
    (ellm--handle-assistant-response response-content conversation)
    (ellm--log-conversation conversation)
    (ellm--insert-conversation-into-org conversation-filepath conversation)))

(defun ellm--handle-assistant-response (response conversation)
  "Add the `RESPONSE' to the `CONVERSATION'.

The `RESPONSE' is expected to be a string."
  (unless (stringp response)
    (error "ellm--handle-assistant-response: Expected string, got %s" (type-of response)))
  (let* ((split-response (ellm--split-response response))
         (title (elt split-response 0))
         (content (elt split-response 1)))
    (ellm--add-or-update-title title conversation)
    (ellm--add-assistant-message content conversation)))

(defun ellm--insert-conversation-into-org (conversation-filepath conversation)
  "Insert the `CONVERSATION' into the org file at `CONVERSATION-FILEPATH'."
  (let* ((messages (alist-get 'messages conversation))
         (number-previous-messages (- (length messages) 2))
         (previous-messages (take number-previous-messages messages))
         (new-messages (nthcdr number-previous-messages messages))
         (title (alist-get 'title conversation))
         (model (alist-get 'model conversation))
         (temperature (alist-get 'temperature conversation))
         (id (alist-get 'id conversation))
         (buffer (if ellm-save-conversations
                     (find-file-noselect conversation-filepath)
                   (get-buffer-create ellm--temp-conversations-buffer-name)))
         (stringified-previous-messages (ellm--messages-to-string previous-messages "**"))
         (org-formatted-new-messages (ellm--convert-messages-to-org new-messages))
         (messages-to-insert (if stringified-previous-messages
                                 (concat stringified-previous-messages
                                         "\n\n"
                                         org-formatted-new-messages)
                               org-formatted-new-messages)))
    (ellm--log-org-messages messages-to-insert)
    (with-current-buffer buffer
      (delay-mode-hooks (org-mode))
      (read-only-mode -1)
      (if-let ((pos (and ellm-save-conversations (org-id-find id 'marker))))
          (progn (goto-char pos)
                 (org-cut-subtree))
        (setq id (or id (org-id-new))))
      (goto-char (point-min))
      (ellm--insert-heading-and-metadata title id model temperature)
      (insert messages-to-insert)
      (read-only-mode +1)
      (when ellm-save-conversations
        (save-buffer))
      (ellm--display-conversations-buffer)
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
  (let ((markdown-string (ellm--messages-to-string messages)))
    (ellm--log-markdown-messages markdown-string)
    (let ((org-string (ellm--markdown-to-org-sync markdown-string)))
      (ellm--log-org-messages org-string)
      org-string)))

(defun ellm--messages-to-string (messages &optional headline-char)
  "Convert the MESSAGES to a markdown string.

Use `HEADLINE-CHAR' as the character for headlines."
  (mapconcat (lambda (message) (ellm--stringify-message message headline-char)) messages "\n\n"))

(defun ellm--stringify-message (message &optional headline-char)
  "Convert the `MESSAGE' to a string.

A message of the form

  `((role . :role) (content . \"content\"))'

is converted to the string

  \"[HC] Role
     content\"

where [HC] is `HEADLINE-CHAR' or \"#\" by default."
  (let* ((role (capitalize (substring (symbol-name (alist-get 'role message)) 1)))
         (content (alist-get 'content message))
         (hc (or headline-char "#")))
    (format "%s %s\n\n%s" hc role content)))

(defun ellm--markdown-to-org-sync (markdown-string)
  "Convert `MARKDOWN-STRING' from Markdown to Org using Pandoc."
  (let* ((pandoc-command "pandoc -f markdown+tex_math_single_backslash -t org --shift-heading-level-by=1 --lua-filter=/home/jfa/projects/emacs/ellm/format_org.lua")
         (org-string
          (with-temp-buffer
            (insert markdown-string)
            (shell-command-on-region (point-min) (point-max) pandoc-command (current-buffer) t ellm--log-buffer-name)
            (buffer-string))))
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
  (let* ((pos (if id (org-id-find id 'marker)
                (save-excursion (ellm--goto-conversation-top) (point))))
         (conversation (ellm--parse-conversation pos))
         (filepath (unless (ellm--at-temp-conversations-buffer-p)
                     (buffer-file-name (current-buffer)))))
    (ellm--set-model (alist-get 'model conversation))
    (ellm-set-temperature (alist-get 'temperature conversation))
    (setf (alist-get 'system conversation) (ellm--get-system-message)
          (alist-get 'max_tokens conversation) ellm-max-tokens)
    (ellm-chat conversation filepath prompt)))

(defun ellm--conversation-subtree ()
  "Return the current conversation subtree."
  (save-restriction
    (progn
      (org-narrow-to-subtree)
      (car (org-element-contents (org-element-parse-buffer))))))

(defun ellm--parse-conversation (&optional posn)
  "Parse the org subtree starting at `POSN' into a conversation object."
  (let* (conversation
         (properties (org-entry-properties posn))
         (metadata (ellm-org--get-conversation-metadata properties));; (subtree (ellm--conversation-subtree))
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
  (let ((role (intern (concat ":" (downcase (org-element-property
                                             :raw-value
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

(defun ellm-org--at-headline-level-p (level)
  "Check if the point is at a headline of the given `LEVEL'."
  (and (org-at-heading-p)
       (= (org-element-property :level (org-element-at-point-no-context)) level)))

(defun ellm--org-plain-text (org-element)
  "Get the plain text content of the given `ORG-ELEMENT'."
  (with-temp-buffer
    (insert (org-element-interpret-data org-element))
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun ellm--point-at-conversation-p ()
  "Return non-nil if point is within a conversation subtree."
  (and
   (eq major-mode 'org-mode)
   (or (and
        (f-equal-p (f-dirname (buffer-file-name (current-buffer)))
                   ellm--conversations-dir)
        (string-prefix-p ellm--conversations-filename-prefix
                         (f-filename (buffer-file-name (current-buffer)))))
       (ellm--at-temp-conversations-buffer-p))
   (not (org-before-first-heading-p))))

(defun ellm--at-temp-conversations-buffer-p ()
  "Check if the current buffer is `ellm--temp-conversations-buffer-name'."
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

(defun ellm--display-conversations-buffer ()
  "Prepare the conversations buffer for viewing.

It is assumed that the current buffer is the conversations buffer."
  (display-buffer (current-buffer))
  (org-overview)
  (ellm--goto-first-top-level-heading)
  (org-fold-show-subtree)
  (if-let* ((headlines
               (org-element-map (ellm--conversation-subtree) 'headline 'identity))
              (last-user-message
               (cl-find-if
                (lambda (hl) (string-match-p "User" (org-element-property :raw-value hl)))
                headlines
                :from-end t)))
      (with-selected-window (get-buffer-window (current-buffer))
        (goto-char (org-element-property :begin last-user-message))
        (recenter-top-bottom 0)
        (org-next-visible-heading 1))
    (message "No assistant messages found")))

(defun ellm-show-conversations-buffer (&optional filepath)
  "Show the conversations contined in the conversations file.

Which file that is is determined by `ellm--get-conversations-file-path',
unless `FILEPATH' is non-nil in which case that file is used."
  (interactive)
  (let* ((path
           (cond ((not (null filepath)) filepath)
                 ((called-interactively-p 'interactive)
                  (read-file-name "Select conversation file: " ellm--conversations-dir nil t "/"
                                  (lambda (f)
                                    (and
                                     (equal (file-name-extension f) "org")
                                     (string-prefix-p ellm--conversations-filename-prefix (f-filename f))))))
                 (t (ellm--get-conversation-file-path))))
         (buffer (find-file-other-window path)))
    (with-current-buffer buffer
      (delay-mode-hooks (org-mode))
      (read-only-mode 1)
      (org-overview)
      (ellm--org-apply-rating-face))))

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
  (unless (looking-at-p "^\\* ")
    (if-let* ((top-headline-p
               (lambda (el) (and (eq 'headline (org-element-type el))
                                 (eql 1 (org-element-property :level el)))))
              (ancestors (org-element-lineage (org-element-at-point)))
              (top-headline
               (cl-find-if top-headline-p
                           ancestors
                           :from-end)))
        (goto-char (org-element-property :begin top-headline))
      (error "No top-level headline found"))))

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

The variables that are registered for persistence are:

- `ellm-max-tokens': The maximum number of tokens to generate in the
  completion.
- `ellm-model-size': The size of the model to use.
- `ellm-provider': The provider of the language model.
- `ellm-temperature': The temperature to use for sampling during completion.
- `ellm-save-conversations': A boolean value indicating whether to save
  conversations.
- `ellm--debug-mode': A boolean value indicating whether to enable debug
  mode.

Note that this function does not actually save the variables. It only
registers them with the `savehist' package, which will save them when it
saves the Emacs session. Similarly, it does not restore the variables.
The `savehist' package will restore them when it restores the Emacs
session.

This function does not take any arguments and returns nil."
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
  (ellm--log (if (fboundp 'http-status-code)
                 (http-status-code status)
               (plist-get status :error) "HTTP-ERROR" t)))

(defun ellm--log-http-redirect (status)
  "Log HTTP `STATUS' redirects."
  (ellm--log (plist-get status :redirect) "HTTP-REDIRECT") t)
(defun ellm--log-no-response-body ()
  "Log that were no response body."
  (ellm--log "No response body (DNS resolve or TCP error)" "REQUEST-ERROR" t))

(defun ellm--log-request (url headers body)
  "Log the request `URL', `HEADERS', and `BODY'."
  (when ellm--debug-mode
    (ellm--log url "REQUEST-URL" t)
    (ellm--log headers "REQUEST-HEADERS" t)
    (ellm--log body "REQUEST-BODY")))

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

(defun ellm--org-rate-response (rating)
  "Rate the current org headline with a RATING."
  (interactive "sRate the conversation (1-5): ")
  (unless (ellm--point-at-conversation-p)
    (user-error "Point is not within a conversation"))
  (let ((valid-ratings '("1" "2" "3" "4" "5")))
    (if (member rating valid-ratings)
        (progn
          (read-only-mode -1)
          (save-excursion
            (ellm--goto-conversation-top)
            (org-set-property "RATING" rating))
          (read-only-mode +1)
          (save-buffer))
      (error "Invalid rating. Please enter a value between 1 and 5"))))

(defun ellm--org-apply-rating-face ()
  "Apply custom face to org headlines based on their RATING property."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (when-let ((rating (org-entry-get (point) "RATING")))
        (let ((face (pcase (string-to-number rating)
                      ((pred (lambda (r) (assoc r ellm-org--faces-alist)))
                       (cdr (assoc (string-to-number rating) ellm-org--faces-alist)))
                      (_ nil))))
          (when face
            (let ((title-begin (line-beginning-position))
                  (title-end (re-search-forward "  " nil t)))
              (remove-overlays title-begin title-end 'ellm-rating t)
              (let ((ov (make-overlay title-begin title-end)))
                (overlay-put ov 'ellm-rating t)
                (overlay-put ov 'face face)))))))))

(defun ellm-org-rate-response-and-refresh (rating)
  "Rate the current org headline with a RATING and apply the corresponding face."
  (interactive "sRate the conversation (1-5): ")
  (ellm--org-rate-response rating)
  (org-redisplay-inline-images)
  (save-restriction
    (save-excursion
      (ellm--goto-conversation-top)
      (org-narrow-to-element)
      (ellm--org-apply-rating-face))))

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

(defun ellm--extract-symbol-definition-at-point (&optional variablep)
  "Extract the definition of the symbol at point.

If `VARIABLEP' is non-nil, treat the symbol as a variable."
  (let* ((symbol (symbol-at-point))
         (documentation-fn (lambda (s)
                             (if variablep
                                 (documentation-property s 'variable-documentation 'raw)
                               (documentation s 'raw))))
         (doc (funcall documentation-fn (symbol-at-point))))
    (cons symbol doc)))

(defun ellm--extract-definitions-in-buffer ()
  "Extract all variable and function definitions in the current buffer."
  (interactive)
  (let ((functions '())
        (variables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^(\\(?:defvar\\|defcustom\\) " nil t)
        (push (ellm--extract-symbol-definition-at-point 'variable) variables))
      (goto-char (point-min))
      (while (re-search-forward "^(defun " nil t)
        (push (ellm--extract-symbol-definition-at-point) functions)))
    `((variables . ,(nreverse variables)) (functions . ,(nreverse functions)))))

(defun ellm--extract-and-show-definitions-in-json ()
  "Extract and print all variable and function definitions in the current buffer."
  (interactive)
  (let* ((definitions (ellm--extract-definitions-in-buffer))
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
  (let* ((definitions (ellm--extract-definitions-in-buffer)))
    (ellm--save-object-in-json definitions filename)))

(defun ellm--save-object-in-json (object filename)
  "Save an `OBJECT' to a `FILENAME' in JSON format."
  (interactive)
  (let* ((json-string (json-serialize object)))
    (with-temp-file filename
      (insert json-string)
      (json-pretty-print (point-min) (point-max) 'minimize)
      (buffer-substring-no-properties (point-min) (point-max))
      (newline)))
  nil)

(defun ellm--server-running-p ()
  "Return non-nil if the ellm server is running."
  (not (string= "" (shell-command-to-string "lsof -i :5040"))))

(defun ellm-start-server ()
  "Start the ellm server."
  (interactive)
  (unless (ellm--server-running-p)
    (let ((default-directory "~/projects/emacs/ellm")
          (openai-api-key (funcall ellm-get-openai-api-key)))
      (setenv "OPENAI_API_KEY" openai-api-key)
      (start-process "ellm-server" "*ellm-server*" "node" "server/server.js"))))

;;;###autoload
(define-minor-mode ellm-mode
  "Minor mode for interacting with LLMs."
  :group 'ellm
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ; N") #'ellm-chat-at-point)
            (define-key map (kbd "C-c ; n") #'ellm-chat)
            (define-key map (kbd "C-c ; e") #'ellm-export-conversation)
            (define-key map (kbd "C-c ; r") #'ellm-org-rate-response-and-refresh)
            (define-key map (kbd "C-c ; t") #'ellm-toggle-test-mode)
            (define-key map (kbd "C-c ; c") #'ellm-set-config)
            (define-key map (kbd "C-c ; ;") #'ellm-show-conversations-buffer)
            (define-key map (kbd "C-c ; s") #'ellm-toggle-save-conversations)
            (define-key map (kbd "C-c ; o") #'ellm-org-fold-conversations-buffer)
            (define-key map (kbd "C-c ; j") #'ellm-org-next-message)
            (define-key map (kbd "C-c ; k") #'ellm-org-previous-message)
            map)
  :global true
  (if ellm-mode
      (ellm-start-server)))

;;;###autoload
(define-globalized-minor-mode global-ellm-mode ellm-mode
  (lambda ()
    (ellm--setup-persistance)
    (ellm-mode 1)))

(provide 'ellm)
;;; ellm.el ends here
