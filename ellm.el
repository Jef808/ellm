;;; ellm.el --- Simple interface to interact with LLMs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jean-François Arbour
;;
;; Author: Jean-François Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-François Arbour <jf.arbour@gmail.com>
;; Created: March 25, 2024
;; Modified: August 02, 2024
;; Version: 1.0.0
;; Keywords: convenience extensions lisp local hypermedia matching outlines tools
;; Homepage: https://github.com/Jef808/ellm
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package provides a simple interface to interact with LLMs within your workflow.
;;
;;; Code:

(require 'f)
(require 'json)
(require 'project)
(require 'markdown-mode)
(require 'org-element)
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

(defcustom ellm-api-key #'ellm-api-key-from-env
  "A function taking a PROVIDER and returning its API key."
  :type 'function
  :group 'ellm)

(defconst ellm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defconst ellm--anthropic-api-url "https://api.anthropic.com/v1/messages"
  "The URL to send requests to the Anthropic API.")

(defconst ellm--xai-api-url "https://api.x.ai/v1/chat/completions"
  "The URL to send requests to the xAI API.")

(defconst ellm--perplexity-api-url "https://api.perplexity.ai/chat/completions"
  "The URL to send requests to the Perplexity API.")

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
          (const :tag "xAI" xai)
          (const :tag "Perplexity" perplexity))
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

(defcustom ellm-temperature 0.4
  "The temperature to use when making a prompt."
  :type 'float
  :group 'ellm)

(defcustom ellm-max-tokens 2048
  "The temperature to use when making a prompt."
  :type 'integer
  :group 'ellm)

(defcustom ellm--openai-models-alist `((big . "gpt-4.1")
                                       (medium . "gpt-4.1-mini")
                                       (small . "gpt-4.1-nano"))
  "Alist mapping model sizes to OpenAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm--anthropic-models-alist `((big . "claude-sonnet-4-20250514")
                                          (medium . "claude-3-7-sonnet-20250219")
                                          (small . "claude-3-5-haiku-20241022"))
  "Alist mapping model sizes to Anthropic model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm--xai-models-alist `((big . "grok-beta")
                                    (medium . "grok-beta")
                                    (small . "grok-beta"))
  "Alist mapping model sizes to xAI model names."
  :type 'alist
  :group 'ellm)

(defcustom ellm--perplexity-models-alist `((big . "sonar-pro")
                                           (medium . "sonar")
                                           (small . "sonar"))
  "Alist mapping model sizes to Perplexity model names."
  :type 'alist
  :group 'ellm)

(defvar ellm--models-alist
  (list
   (cons 'openai `((big . "gpt-4.1")
                   (medium . "gpt-4.1-mini")
                   (small . "gpt-4.1-nano")))
   (cons 'anthropic `((big . "claude-sonnet-4-20250514")
                      (medium . "claude-3-7-sonnet-20250219")
                      (small . "claude-3-5-haiku-20241022")))
   (cons 'xai `((big . "grok-beta")
                (medium . "grok-beta")
                (small . "grok-beta")))
   (cons 'perplexity `((big . "sonar-pro")
                       (medium . "sonar")
                       (small . "sonar"))))
  "Alist mapping providers to their models.")

(defcustom ellm-model-alist `(("gpt-4.1" . (:provider openai :size big))
                              ("gpt-4.1-mini" . (:provider openai :size medium))
                              ("gpt-4.1-nano" . (:provider openai :size small))
                              ("claude-sonnet-4-20250514" . (:provider anthropic :size big))
                              ("claude-3-7-sonnet-20250219" . (:provider anthropic :size medium))
                              ("claude-3-5-haiku-20241022" . (:provider anthropic :size small))
                              ("grok-beta" . (:provider xai :size big))
                              ("grok-beta" . (:provider xai :size medium))
                              ("grok-beta" . (:provider xai :size small))
                              ("sonar-pro" . (:provider perplexity :size big))
                              ("sonar" . (:provider perplexity :size medium))
                              ("sonar" . (:provider perplexity :size small)))
  "Alist mapping model names to their providers."
  :type 'alist
  :group 'ellm)

(defvar ellm-provider-configurations
  `((openai . ((prepare-request-headers . ellm--prepare-request-headers-default)
               (prepare-request-body . ellm--prepare-request-body-default)
               (parse-response . ellm--parse-response-openai)
               (models-alist . ,ellm--openai-models-alist)))
    (anthropic . ((prepare-request-headers . ellm--prepare-request-headers-anthropic)
                  (prepare-request-body . ellm--prepare-request-body-anthropic)
                  (parse-response . ellm--parse-response-anthropic)
                  (models-alist . ,ellm--anthropic-models-alist)))
    (xai . ((prepare-request-headers . ellm--prepare-request-headers-default)
            (prepare-request-body . ellm--prepare-request-body-default)
            (parse-response . ellm--parse-response-openai)
            (models-alist . ,ellm--xai-models-alist)))
    (perplexity . ((prepare-request-headers . ellm--prepare-request-headers-default)
                   (prepare-request-body . ellm--prepare-request-body-default)
                   (parse-response . ellm--parse-response-openai)
                   (models-alist . ,ellm--perplexity-models-alist))))
  "Alist mapping providers to their API configurations.")

(defun ellm-providers-supported ()
  "List of supported providers."
  (seq-uniq (mapcar
             (lambda (model)
               (plist-get (cdr model) :provider)) ellm-model-alist)))

(defun ellm-api-key (provider)
  "Get the api key for `PROVIDER'."
  (funcall (symbol-value 'ellm-api-key) provider))

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

(defvar ellm--system-message-suffix "you accompany any response with a short title \
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
  `((none :type string
     :value nil)
    (default :type string
             :value "You are a useful general assistant integrated with Emacs.
Your goal is to execute the user's task or precisely answer their questions, \
using all the CONTEXT the user provides (if any).
Ensure your responses are relevant and adequate based on the nature of the query.
You are very cautious and give thorough justifications for each claim you make.
Avoid unnecessary politeness and organizational sections,
focusing instead on clear, relevant examples and the technical aspects of the subject at hand.")
    (question-and-answer :type string
                         :value "You are an expert technical assistant integrated with Emacs.
Your primary task is to accurately answer the user's technical questions.
When answering, ensure that your responses are directly relevant to the question asked and delve in the technicalities when possible.
If you are unsure or don't have enough information to provide a confident answer, \
simply say \"I don't know\" or \"I'm not sure.\"
Avoid unnecessary politeness details and focus on accuracy and relevance.")
    (code-refactoring :type function
                      :args (:language)
                      :value (lambda (language)
                               (format "You are an expert %s programmer.
Your primary task is to assist with improving and optimizing existing code.
Identify inefficient or outdated code patterns and suggest better alternatives.
Ensure that the refactored code remains functionally equivalent to the original.
If there are multiple valid ways to refactor, present the options and recommend the most efficient one.
Provide clear, concise code samples highlighting the changes."
                                       language)))
    (code-review :type function
                 :args (:language)
                 :value (lambda (language)
                          (format "You are an expert %s programmer.
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
                              (format "You are an expert %s programmer.
Your primary task is to assist with code generation, ensuring accuracy and efficiency.
When generating code, use the provided CONTEXT to tailor your responses to the user's needs.
If the problem can be solved in multiple ways, briefly state the options and make a recommendation for which one to use.
Maintain a focus on technical precision and completeness without redundant explanations or politeness."
                                      language)))
    (code :type function
          :args (:language)
          :value (lambda (language)
                   (concat "You are an expert"
                           (if (null language) " " (format " %s " language))
                           "assisting a professional developer.
Please maintain a focus on technical precision and completeness without redundant explanations or politeness.")))
    (latex-expert :type string
                  :value "You are an expert in LaTeX.")
    (prompt-generator :type string
                      :value "I want you to act as a prompt generator.
Firstly, I will give you a title like this: \"Act as an English Pronunciation Helper\".
Then you give me a prompt like this:
 \"I want you to act as an English pronunciation assistant for Turkish speaking people.
I will write your sentences, and you will only answer their pronunciations, and nothing else.
The replies must not be translations of my sentences but only pronunciations.
Pronunciations should use Turkish Latin letters for phonetics.
Do not write explanations on replies.\"
(You should adapt the sample prompt according to the title I gave. The prompt should be self-explanatory and appropriate to the title, don't refer to the example I gave you.)")
    (software-architect :type string
                        :value "I want you to act as a software architecture blueprint generator. I will
provide you with a brief description of a software project, and you will
create a high-level architectural design for it. Your blueprint should
include:

1. A brief overview of the system's main components
2. The relationships and interactions between these components
3. Key design patterns or architectural styles to be used
4. Considerations for scalability, security, and performance
5. Potential technologies or frameworks to be utilized

Please present your blueprint in a clear, structured format using
markdown. Include a simple diagram using ASCII art if possible. Do not
implement any code; focus solely on the architectural design."))
  "Alist mapping system message names to their content.
The content of those messages are the system messages that will be used as
instructions for the language models in new conversations.
Allowing for string templates is useful e.g. for automatically setting the
language of code generation system messages."
  :safe #'always
  :type '(alist :key-type symbol
          :value-type (choice
                       (list :tag "String message"
                             (const :type)
                             (const string)
                             (const :value)
                             string)
                       (list :tag "Function message"
                             (const :type)
                             (const function)
                             (const :args)
                             (repeat symbol)
                             (const :value)
                             function)))
  :group 'ellm)

(defvar ellm-current-system-message 'default
  "The index of the current system message in `ellm-system-messages'.")

(defvar ellm-prompt-context-fmt-string
  "## CONTEXT:\n%s\n\n## PROMPT:\n%s\n"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--add-context-chunk' for usage details.")

(defvar ellm-prompt-context-fmt-string-anthropic
  "<context>\n%s\n</context>\n\n<prompt>\n%s\n</prompt>"
  "The format string to use with `format' for building the context message.
This message will be prepended to the first user prompt of a conversation.
See `ellm--add-context-chunk' for usage details.")

(defconst ellm-org--buffer-props
  '(:STARTUP overview
    :SETUPFILE "~/repos/org-html-themes/org/theme-readtheorg-local.setup"
    :OPTIONS toc:nil)
  "Properties to set for conversations buffers.")

(defconst ellm--log-buffer-name "*ellm-logs*"
  "Log buffer for LLM messages.")

(defconst ellm--temp-conversations-buffer-name "*ellm-test*"
  "The file to store conversation history.")

(defconst ellm--context-buffer-name "*ellm-context*"
  "The name of the buffer used to display the context chunks.")

(defun ellm-api-key-from-env (provider)
  "Get the API key from the environment for the `PROVIDER'."
  (getenv (format "%s_API_KEY" (upcase (symbol-name provider)))))

(defun ellm--get-system-message (&rest args)
  "Get the system message based on `ellm-current-system-message'.
The `ARGS' should be the keyword arguments corresponding to the signature of the
system message function, if any are needed."
  (unless ellm--test-mode
    (let* ((message-entry (alist-get ellm-current-system-message ellm-system-messages))
           (message-type (plist-get message-entry :type))
           (effective-system-message
            (cond
             ((eq message-type 'function)
              (let ((func (plist-get message-entry :value))
                    (arg-keys (plist-get message-entry :args)))
                (apply func (mapcar (lambda (key) (plist-get args key)) arg-keys))))
             ((eq message-type 'string)
              (plist-get message-entry :value))))
           (effective-system-message-suffix
            (concat
             (unless (eq ellm-current-system-message 'none) "\nFinally, ")
             ellm--system-message-suffix)))
      (concat effective-system-message effective-system-message-suffix))))

(defun ellm--get-provider-configuration (provider)
  "Get the configuration for the `PROVIDER'."
  (alist-get provider ellm-provider-configurations))

(defun ellm-set-config ()
  "Call the `SETTING-FUNCTION' according to the user's choice."
  (interactive)
  (while-let ((config-function (ellm--config-prompt)))
    (funcall-interactively config-function)))

(defun ellm--config-prompt ()
  "Prompt the user to choose a setting to configure."
  (let ((choices
         (list (cons (ellm--toggle-test-mode-description) #'ellm-toggle-test-mode)
               (cons (ellm--toggle-debug-mode-description) #'ellm-toggle-debug-mode)
               (cons (ellm--provider-description) #'ellm-set-provider)
               (cons (ellm--model-size-description) #'ellm-set-model-size)
               (cons (ellm--temperature-description) #'ellm-set-temperature)
               (cons (ellm--max-tokens-description) #'ellm-set-max-tokens)
               (cons (ellm--system-message-description) #'ellm-set-system-message))))
    (alist-get
     (completing-read "Choose a setting to configure: " (mapcar 'car choices))
     choices nil nil 'equal)))

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
                                 ellm-system-messages))))))
    (setq ellm-current-system-message sm)
    (message "...system message set to %s..." sm)))

(defun ellm-set-provider (&optional provider)
  "Set the API `PROVIDER' to use."
  (interactive)
  (let* ((format-choice-fn
          (lambda (p)
            (format (propertize (symbol-name p) 'face 'font-lock-type-face))))
         (select-choice-fn
          (lambda ()
            (completing-read
             "Choose provider: " (mapcar format-choice-fn (ellm-providers-supported)))))
         (choice (or provider (intern (funcall select-choice-fn))))
         (new-model (ellm--get-model choice ellm-model-size)))
    (unless new-model
      (setq ellm-model-size 'big
            new-model (ellm--get-model choice ellm-model-size)))
    (setq ellm-model new-model
          ellm-provider choice))
  (message "...provider set to %s, model set to %s..." ellm-provider ellm-model))

(defun ellm--validation-model-size (model-size)
  "Validate that the `MODEL-SIZE' is available for value of `ellm-provider'."
  (when (ellm--get-model ellm-provider model-size)
    model-size))

(defun ellm-set-model-size (&optional model-size)
  "Set the `MODEL-SIZE' to use."
  (interactive)
  (let ((ms model-size)
        (make-error-message
         (lambda (size)
           (propertize
            (format
             "Model size `%s' is not supported by `%s'"
             (propertize
              (symbol-name size) 'face 'font-lock-type-face)
             (propertize
              (symbol-name ellm-provider) 'face 'font-lock-type-face))
            'face 'font-lock-warning-face)))
        (prompt-message "Choose model size: "))
    (if (called-interactively-p 'interactive)
        (while (null (setq ms (ellm--validation-model-size model-size)))
          (setq model-size (intern (completing-read
                                    prompt-message
                                    (mapcar #'(lambda (item)
                                                (format
                                                 (propertize (symbol-name item) 'face 'font-lock-type-face)))
                                            '(big medium small))))
                prompt-message (funcall make-error-message model-size)))
      (unless (setq ms (ellm--validation-model-size model-size))
        (error (funcall make-error-message model-size))))
    (let ((model (ellm--get-model ellm-provider ms)))
      (ellm--set-model model)
      (setq ellm-model-size ms)))
  (message "...model set to %s..." ellm-model))

(defun ellm--validation-max-tokens (max-tokens)
  "Validate the `MAX-TOKENS' value."
  (when (and (integerp max-tokens) (> max-tokens 0) (<= max-tokens 8192))
    max-tokens))

(defun ellm-set-max-tokens (&optional max-tokens)
  "Set the `MAX-TOKENS' to use for the LLM prompt."
  (interactive)
  (let ((mt max-tokens))
    (if (called-interactively-p 'interactive)
        (while (null (setq mt (ellm--validation-max-tokens max-tokens)))
          (setq max-tokens (read-number "Max tokens (between 1 and 8192): ")))
      (unless (setq mt (ellm--validation-max-tokens max-tokens))
        (error "Invalid argument: `%s' should be integer between 1 and 8192" max-tokens)))
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
        (insert (ellm--conversations-file-header))
        (message "...ellm-test-mode enabled...")))))

(defun ellm-toggle-debug-mode ()
  "Toggle debug mode."
  (interactive)
  (setq ellm--debug-mode (not ellm--debug-mode))
  (message "...debug mode %s..." (if ellm--debug-mode "enabled" "disabled")))

(defun ellm--true-false-menu-item (item-title ptrue)
  "Return a menu description for `ITEM-TITLE' with a true/false value.
Its value will be a propertized t if `PTRUE' is non-nil, nil otherwise."
  (let ((padding (make-string (- 32 (length item-title)) ? ))
        (value (if ptrue (propertize "t" 'face 'font-lock-builtin-face)
                 (propertize "nil" 'face 'font-lock-comment-face))))
    (format "%s%s%s" item-title padding value)))

(defun ellm--toggle-test-mode-description ()
  "Return a string describing the current save conversations setting."
  (ellm--true-false-menu-item "Test Mode" ellm--test-mode))

(defun ellm--toggle-debug-mode-description ()
  "Return a string describing the current debug mode setting."
  (ellm--true-false-menu-item "Debug Mode" ellm--debug-mode))

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

(defun ellm--get-conversation-model (conversation)
  "Get the model used in the `CONVERSATION'."
  (alist-get 'model conversation))

(defun ellm--get-model (provider model-size)
  "Get the model name for the given `PROVIDER' and `MODEL-SIZE'."
  (let ((models-by-size (alist-get provider ellm--models-alist)))
    (alist-get model-size models-by-size)))

(defun ellm--get-model-properties (arg)
  "Get the property list associated to the model passed in `ARG'.
The `ARG' can be either a string (the model name) or
a list with a `model' key. (e.g. a conversation)."
  (cond
   ((stringp arg)
    (alist-get arg ellm-model-alist nil nil 'equal))
   ((consp arg)
    (let ((model (alist-get 'model arg)))
      (alist-get model ellm-model-alist nil nil 'equal)))
   (t (error "Invalid argument: `%s' should be a string or a list" arg))))

(defun ellm--get-model-provider (arg)
  "Get the provider of the model passed in `ARG'.
The `ARG' can be either a string (the model name) or
a list with a `model' key. (e.g. a conversation)."
  (plist-get (ellm--get-model-properties arg) :provider))

(defun ellm--set-model (model)
  "Set the `MODEL' to use for the LLM prompt."
  (let ((model-properties (ellm--get-model-properties model)))
    (if model-properties
        (setq ellm-model model
              ellm-provider (plist-get model-properties :provider)
              ellm-model-size (plist-get model-properties :size))
      (error "Model `%s' not found" model))))

(defvar ellm--major-mode-to-org-lang-alist
  '((python-ts-mode . "python")
    (python-mode . "python")
    (emacs-lisp-mode . "emacs-lisp")
    (org-mode . "org")
    (lua-mode . "lua")
    (c++-mode . "cpp")
    (c++-ts-mode . "cpp")
    (rjsx-mode . "typescript")
    (tsx-mode . "typescript")
    (sh-mode . "shell"))
  "Alist mapping major modes to Org mode source block languages.")

(defmacro ellm--append-to! (place element)
  "Append ELEMENT to the end of the list stored in PLACE.
Similar to `push', but for the end of the list."
  `(setf ,place
         (if ,place
             (nconc ,place (list ,element))
           (list ,element))))

(defun ellm--detect-media-type (image-path)
  "Detect the media type of the image at `IMAGE-PATH'."
  (cond
   ((string-match-p "\\.png\\'" image-path) "image/png")
   ((string-match-p "\\.jpe?g\\'" image-path) "image/jpeg")
   ((string-match-p "\\.gif\\'" image-path) "image/gif")
   ((string-match-p "\\.webp\\'" image-path) "image/webp")
   (t (error "Unsupported image format: %s" image-path))))

(defun ellm--encode-image-to-base64 (filepath)
  "Encode the image at FILEPATH to a base64 string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filepath)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

(defun ellm--make-message (role content &optional image-path)
  "Create a message with given `ROLE', `CONTENT' and optional `IMAGE-PATH'."
  (unless (and
           (memq role '(:user :assistant :system))
           (stringp content))
    (error "Invalid message role or content: %S %S" role content))
  (let* ((image-msg (when (and
                           image-path
                           (eq ellm-provider 'anthropic))
                      `((type . "image")
                        (source . ((type . "base64")
                                   (media_type . ,(ellm--detect-media-type image-path))
                                   (data . ,(ellm--encode-image-to-base64 image-path)))))))
         (text-msg `((type . "text")
                     (text . ,content)))
         (content (if image-msg (list image-msg text-msg) (list text-msg))))
    `((role . ,role)
      (content . ,content))))

(defun ellm--add-system-message (content conversation)
  "Add a system message with `CONTENT' to the `CONVERSATION'.
The system message is prepended to the `messages' list."
  (push (ellm--make-message :system content)
        (alist-get 'messages conversation)))

(defun ellm--add-user-message (conversation content &optional image-path)
  "Append a user message with `CONTENT' to the `CONVERSATION'.
The user message is appended to the `messages' list."
  (ellm--append-to! (alist-get 'messages conversation)
                    (ellm--make-message :user content image-path)))

(defun ellm--add-assistant-message (conversation content)
  "Append an assistant message with `CONTENT' to the `CONVERSATION'.
The assistant message is appended to the `messages' list."
  (ellm--append-to! (alist-get 'messages conversation)
                    (ellm--make-message :assistant content)))

(defun ellm--initialize-conversation (prompt &optional image-path)
  "Initialize a new conversation starting with `PROMPT' and optional `IMAGE-PATH'.
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
           (cons 'system-alias ellm-current-system-message)
           (cons 'title nil)
           (cons 'system (apply #'ellm--get-system-message system-message-args)))))
    (ellm--add-user-message conversation prompt image-path)
    conversation))

(defun ellm--prepare-request-headers (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations))
         (prepare-fn (alist-get 'prepare-request-headers config)))
    (funcall prepare-fn)))

(defun ellm--prepare-request-headers-default ()
  "Prepare the headers for API requests as done for openai models.
The `ellm-api-key' function is used to retrieve the api key
for the current provider, which is stored in `ellm-provider'."
  `(("Authorization" . ,(concat "Bearer " (funcall ellm-api-key ellm-provider)))
    ("Content-Type" . "application/json; charset=utf-8")))

(defun ellm--prepare-request-headers-anthropic ()
  "Prepare the headers for API requests to the anthropic models.
The `ellm-api-key' function is used to retrieve the api key."
  `(("x-api-key" . ,(funcall ellm-api-key ellm-provider))
    ("anthropic-version" . "2023-06-01")
    ("Content-Type" . "application/json; charset=utf-8")))

(defun ellm--vmessage (msg)
  "Transform the `content' of a message `MSG' to a vector.
This is necessary to be compatible with `json-serialize'."
  (let ((role (substring (symbol-name (alist-get 'role msg)) 1))
        (content (alist-get 'content msg)))
    `((role . ,role) (content . ,(vconcat content)))))

(defun ellm--serialize-conversation (conversation)
  "Serialize the `CONVERSATION' to a JSON string.
The `messages' list is transform into a vector of messages to be
compatible with `json-serialize'."
  (let* ((messages (mapcar #'ellm--vmessage (alist-get 'messages conversation))))
    (json-serialize (cons `(messages . ,(vconcat messages)) conversation))))

(defun ellm--prepare-request-body-default (conversation)
  "Prepare the messages list with a new user message based on `CONVERSATION'.
The SYSTEM entry, if non-nil, is removed and made into a system message intead.
Also remove the TITLE and ID entries."
  (let ((conversation-copy (copy-alist conversation))
        (system-directives (alist-get 'system conversation)))
    (setf (alist-get 'title conversation-copy nil 'remove) nil
          (alist-get 'id conversation-copy nil 'remove) nil
          (alist-get 'system-alias conversation-copy nil 'remove) nil
          (alist-get 'system conversation-copy nil 'remove) nil)
    (when system-directives
      (let ((messages-copy (cl-copy-list (alist-get 'messages conversation))))
        (setf (alist-get 'messages conversation-copy) messages-copy)
        (ellm--add-system-message system-directives conversation-copy)))
    conversation-copy))

(defun ellm--prepare-request-body-anthropic (conversation)
  "Prepare the API call body to send `CONVERSATION'.
Remove the SYSTEM entry in case it is nil.
Also remove the TITLE and ID entries."
  (let ((conversation-copy (copy-alist conversation))
        (system-directives (alist-get 'system conversation)))
    (setf (alist-get 'title conversation-copy nil 'remove) nil
          (alist-get 'id conversation-copy nil 'remove) nil
          (alist-get 'system-alias conversation-copy nil 'remove) nil
          (alist-get 'system conversation-copy nil 'remove) system-directives)
    conversation-copy))

(defun ellm--prepare-request-body (conversation)
  "Prepare the API call body to send `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (config (alist-get provider ellm-provider-configurations)))
    (funcall (alist-get 'prepare-request-body config) conversation)))

(defun ellm--get-url (conversation)
  "Get the URL to send the request to based on the provider of `CONVERSATION'."
  (let* ((provider (ellm--get-model-provider conversation))
         (provider-name (symbol-name provider))
         (url (intern (format "ellm--%s-api-url" provider-name))))
    (if (boundp url) (symbol-value url)
      (error "ellm--get-url: Unknown provider or missing url: %s"
             provider-name))))

(defun ellm-conversations-buffer-from-project ()
  "Get the conversations buffer associated to the current project.
The project is determined by calling `project-current' (which see).
In case the function was called interactively, `project-current' is
called with a non-nil MAYBE-PROMPT argument.
If `FORCE-PROMPT' is non-nil, instead prompt the user for which
project to target."
  (interactive)
  (let* ((maybe-prompt (and (project-current) (called-interactively-p 'interactive)))
         (filename-suffix
          (if-let ((current (project-current maybe-prompt)))
              (project-name current)
            "general"))
         (filepath (f-join ellm--conversations-dir
                           (concat ellm--conversations-filename-prefix
                                   filename-suffix
                                   ".org"))))
    (find-file-noselect filepath)))

(defun ellm-chat (&optional current-conversation conversations-buffer prompt-wrapper user-prompt image-filepath)
  "Send a chat request to the current provider's completion endpoint.
This function is the main entry point for interacting with the `ellm' package.
Optional arguments:
- CURRENT-CONVERSATION: Conversation object to continue. Start a new one if nil.
- CONVERSATIONS-BUFFER: File path to save the conversation. If nil, use default.
- PROMPT-WRAPPER: Function to wrap the prompt.
- USER-PROMPT: Prompt string. If nil, read interactively.
- IMAGE-FILEPATH: List of image file paths to include in the prompt.
The function sends the request, handles the response, and updates the
conversation.
This function always returns nil."
  (interactive)
  (let* ((prompt-message
          (if current-conversation
              "Enter your next prompt: "
            "Enter your prompt: "))
         (prompt
          (or user-prompt (read-string prompt-message nil 'ellm--prompt-history)))
         (wrapped-prompt (funcall (or prompt-wrapper 'identity) prompt))
         (conversation
          (if current-conversation
              (progn
                (ellm--add-user-message current-conversation wrapped-prompt image-filepath)
                current-conversation)
            (ellm--initialize-conversation wrapped-prompt image-filepath)))
         (output-buffer
          (or conversations-buffer
              (if ellm-save-conversations
                  (ellm-conversations-buffer-from-project)
                (get-buffer-create ellm--temp-conversations-buffer-name))))
         (url (ellm--get-url conversation))
         (request-headers (ellm--prepare-request-headers conversation))
         (request-body (ellm--prepare-request-body conversation))
         (serialized-request-body (ellm--serialize-conversation request-body))
         (url-request-method "POST")
         (url-request-extra-headers request-headers)
         (url-request-data serialized-request-body))
    (ellm--log `((conversation . ,conversation)
                 (output-buffer . ,(buffer-name output-buffer))
                 (request-url . ,url)
                 (request-body . ,serialized-request-body))
               "REQUEST")
    (url-retrieve url #'ellm--handle-response (list output-buffer conversation))
    (message "...Waiting for response from %s..."
             (symbol-name (ellm--get-model-provider conversation))))
  nil)

(defun ellm--handle-response (status conversations-buffer conversation)
  "Handle the response to the prompt made using `CONVERSATION'.
Information about the response is contained in `STATUS' (see `url-retrieve').
The response is added to the `CONVERSATION' and the conversation is added or
updated in the `CONVERSATIONS-BUFFER'."
  (message "...Received response from %s..."
           (symbol-name (ellm--get-model-provider conversation)))
  (cond ((plist-get status :error)
         (let* ((error-info (plist-get status :error))
                (error-code (elt error-info 2)))
           (goto-char url-http-end-of-headers)
           (if-let* ((body (string-trim (buffer-substring-no-properties (point) (point-max))))
                     (parsed-body (json-parse-string body :object-type 'plist))
                     (error-object (plist-get parsed-body :error))
                     (error-type (plist-get error-object :type))
                     (error-message (plist-get error-object :message)))
               (message (ellm--log (format "%d - (%s): %s" error-code error-type error-message) "HTTP-ERROR")))
           nil))
        ((plist-get status :redirect)
         (progn
           (message "...HTTP redirect...")
           (ellm--log status "HTTP-REDIRECT"))
         nil)
        (t (goto-char url-http-end-of-headers)
           (if (<= (point-max) (point))
             (message (ellm--log "No response body (DNS resolve or TCP error)" "REQUEST-ERROR"))
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

(defun ellm--conversations-file-header ()
  "Concatenate `ellm-org--buffer-props' into a header string."
  (with-temp-buffer
    (let ((plist ellm-org--buffer-props))
      (while plist
        (let ((prop (substring (symbol-name (car plist)) 1))
              (value (cadr plist)))
          (insert (format "#+%s: %s\n" prop value))
          (setq plist (cddr plist)))))
    (buffer-string)))

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
         (system-alias (alist-get 'system-alias conversation))
         (stringified-previous-messages (ellm--messages-to-string previous-messages "**"))
         (org-formatted-new-messages (ellm--convert-messages-to-org new-messages))
         (messages-to-insert (concat stringified-previous-messages
                                     (unless (string-empty-p stringified-previous-messages) "\n\n")
                                     org-formatted-new-messages)))
    (with-current-buffer conversations-buffer
      (let ((inhibit-read-only t))
        (when (= (point-min) (point-max))
          (goto-char (point-min))
          (insert (ellm--conversations-file-header)))
        (if-let ((pos (and ellm-save-conversations (org-id-find id 'marker))))
            (progn (goto-char pos)
                   (org-cut-subtree))
          (setq id (or id (org-id-new))))
        (goto-char (point-min))
        (ellm--insert-heading-and-metadata title id model temperature system-alias)
        (insert messages-to-insert)
        (when ellm-save-conversations
          (save-buffer)))
      (ellm--display-conversations-buffer conversations-buffer 'highlight-last-message))))

(defun ellm--insert-heading-and-metadata (title id model temperature system-alias)
  "Insert an Org heading with properties.
The required properties are TITLE, ID, MODEL, TEMPERATURE and SYSTEM-ALIAS."
  (org-insert-heading)
  (insert title "  ")
  (org-insert-time-stamp nil t t)
  (newline)
  (org-set-property "ID" id)
  (org-set-property "MODEL" model)
  (org-set-property "TEMPERATURE" (number-to-string temperature))
  (when system-alias
    (org-set-property "SYSTEM" (symbol-name system-alias))))

(defun ellm--parse-json-response ()
  "Parse the json response from the API call."
  (condition-case error
      (let ((response-body
             (string-trim (buffer-substring-no-properties (point) (point-max)))))
        (ellm--log (json-parse-string response-body) "RESPONSE")
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
        (replace-regexp-in-string "\\\\\"" "\"" content))
    (wrong-type-argument (ellm--log error "RESPONSE-ERROR"))))

(defun ellm--parse-response-anthropic (response)
  "Extract the text from the json `RESPONSE'."
  (condition-case error
      (let* ((messages (gethash "content" response))
             (first-message (aref messages 0))
             (content (gethash "text" first-message)))
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
    (ellm--markdown-to-org-sync markdown-string)))

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
         (content (ellm--stringify-message-content (alist-get 'content message)))
         (hc (or headline-char "#")))
    (format "%s %s\n\n%s" hc role content)))

(defun ellm--stringify-message-content (content)
  "Convert the `CONTENT' of a message to a string."
  (mapconcat (lambda (part)
               (let ((text (alist-get 'text part))
                     (fmt-string "%s\n"))
                 (format fmt-string text)))
             content))

(defconst ellm--lua-filter-path
  (expand-file-name "format_org.lua" (file-name-directory (locate-library "ellm")))
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
    (push (cons 'system-alias ellm-current-system-message) conversation)
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
      (cl-assert
       (ellm-org--at-headline-level-p 1) nil
       "ellm-org--get-conversation-messages: Point should be at the beginning of a conversation")
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
    (ellm--make-message role (s-trim content))))

(defun ellm-org--at-headline-level-p (level &optional posn)
  "Check if the point is at a headline of the given `LEVEL'.
If `POSN' is nil, the current point is used."
  (save-excursion
    (goto-char (or posn (point)))
    (and (org-at-heading-p)
         (= (org-element-property :level (org-element-at-point-no-context)) level))))

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
  (let ((id (unless (ellm--at-temp-conversations-buffer-p)
              (org-entry-get (point) "ID" t))))
    (ellm--resume-conversation id prompt)))

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
    (unless (= (point-min) (point-max))
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
        (message "Last user message not found")))))

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

(defun ellm-setup-persistance ()
  "Register ellm configuration variables with the `savehist' package.
This function is responsible for maintaining the persistence of certain
configuration variables used by the ellm package. It does this by adding
those variables to the `savehist-additional-variables' list. This ensures
that the values of these variables are saved and restored across Emacs
sessions.
The variables registered for persistence are
`ellm-current-system-message', `ellm-max-tokens', `ellm-model-size',
`ellm-provider', `ellm-model', `ellm-temperature' and `ellm--debug-mode'.
Additionally, the `ellm--prompt-history' is persisted to maintain the prompt
history.
This function does not take any arguments and returns nil."
  (let ((symbols-to-add '(ellm-current-system-message
                          ellm-max-tokens
                          ellm-model-size
                          ellm-provider
                          ellm-model
                          ellm-temperature
                          ellm--debug-mode
                          ellm--prompt-history)))
    (dolist (symbol symbols-to-add)
      (cl-pushnew symbol savehist-additional-variables))))

(defun ellm--log (data &optional label)
  "Log `DATA' with an optional `LABEL'."
  (when ellm--debug-mode
    (let* ((timestamp (format-time-string "%Y-%m-%d %a %H:%M"))
           (log-entry `(("TIMESTAMP" . ,timestamp)
                        (,(or label "INFO") . ,data))))
      (with-current-buffer (get-buffer-create ellm--log-buffer-name)
        (goto-char (point-max))
        (unless (bolp) (newline))
        (save-excursion
          (insert (json-encode log-entry))
          (newline))
        (json-pretty-print (point) (point-max)))))
  data)

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
        (cl-assert
         (member rating (mapcar 'car ellm-org--faces-alist)) nil
         "Invalid rating. Please enter a value between 1 and 5"))
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

;; TODO Make this more portable by using different search tools as available.
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

(defface ellm-context-buffer-face
  '((((background dark)) (:background "#328C0411328C" :extend t))  ; Very dark magenta
    (t                   (:background "#CD73FBEECD73" :extend t))) ; Very pale green
  "Face for context overlays in the ellm context buffer."
  :group 'ellm)

(defvar ellm-context-buffer-face 'ellm-context-buffer-face
  "Face for context overlays in the ellm context buffer.")

(defface ellm-context-face
  '((((background dark)) (:background "#111313F03181" :extend t))  ; Very dark blue
    (t                   (:background "#EEECEC0FCE7E" :extend t))) ; Very pale goldenrod
  "Face for context overlays in their original buffer."
  :group 'ellm)

(defvar ellm-context-face 'ellm-context-face
  "Face for context overlays in their original buffer.")

(defvar ellm-context-overlays nil
  "List of overlays representing context chunks.")

(defun ellm--context-at (posm)
  "Return the context overlay at position or marker `POSM'."
  (let* ((beg
          (save-excursion
            (goto-char posm)
            (if (bolp) posm (- posm 1))))
         (end (max posm (+ 1 beg)))
         (overlays (overlays-in beg end)))
    (seq-find #'ellm--overlay-context-overlay-p overlays)))

(defun ellm--contexts-in-region (beg end)
  "Return the context overlays in the region between `BEG' and `END'."
  (let ((overlays (overlays-in beg end)))
    (seq-filter (lambda (ov) (overlay-get ov 'ellm-context)) overlays)))

(defun ellm--context-buffer-overlay (overlay)
  "Return the context overlay in the context buffer corresponding to `OVERLAY'."
  (if (buffer-match-p ellm--context-buffer-name (overlay-buffer overlay))
      overlay
    (overlay-get overlay 'ellm-other-overlay)))

(defun ellm--overlay-context-overlay-p (overlay)
  "Return non-nil if `OVERLAY' is a context overlay."
  (overlay-get overlay 'ellm-context))

(defun ellm--overlay-inline-image-p (overlay)
  "Return non-nil if `OVERLAY' is an inline image overlay."
  (overlay-get overlay 'org-image-overlay))

(defun ellm--overlay-empty-p (overlay)
  "Return non-nil if `OVERLAY' is empty or deleted."
  (or (null (overlay-buffer overlay))
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
           (content (s-trim (buffer-substring-no-properties start end))))
      (if language
          (format "```%s\n%s\n```\n\n" language content)
        (format "```\n%s\n```\n\n" content)))))

(defun ellm--insert-context-content (overlay)
  "Insert the content of the `OVERLAY' into the context buffer.
We create a corresponding overlay in the context buffer to keep track
of the original context chunks."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay))
        (source-buffer (overlay-buffer overlay))
        (target-buffer (ellm-get-or-create-context-buffer)))
    (with-current-buffer target-buffer
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
             (buffer-match-p ellm--context-buffer-name
                             (overlay-buffer overlay)))
    (with-current-buffer (overlay-buffer overlay)
      (let ((inhibit-read-only t)
            (start (overlay-start overlay))
            (end (overlay-end overlay)))
        (when (and start end)
          (delete-region start end)
          (delete-blank-lines))))))

(defun ellm-remove-context-chunk (&optional overlay)
  "Remove the content of the `OVERLAY' from the context buffer."
  (interactive)
  (when overlay
    (cl-assert (ellm--overlay-context-overlay-p overlay) nil
               "Not a valid context overlay: %S" overlay))
  (cl-assert (or overlay (setq overlay (ellm--context-at (point)))) nil
             "No context overlay at point: %S" (point))
  (let* ((context-buffer-ov (ellm--context-buffer-overlay overlay))
         (context-ov (overlay-get context-buffer-ov 'ellm-other-overlay)))
    (ellm--try-delete-context-content context-buffer-ov)
    (delete-overlay context-buffer-ov)
    (delete-overlay context-ov)
    (setq ellm-context-overlays (delq context-ov ellm-context-overlays))))

(defun ellm-org--delete-inline-image-at-point ()
  "Delete the inline image at point in the context buffer."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (catch 'deleted
      (dolist (overlay overlays)
        (when (overlay-get overlay 'org-image-overlay)
          (let ((inhibit-read-only t)
                (start (overlay-start overlay))
                (end (overlay-end overlay)))
            (delete-overlay overlay)
            (delete-region start end)
            (throw 'deleted t))))
      nil)))

(defun ellm-org-delete ()
  "Delete inline image at point, or remove context chunk at point."
  (interactive)
  (unless (ellm-org--delete-inline-image-at-point)
    (ellm-remove-context-chunk)))

(defun ellm-view-context-buffer ()
  "View the context overlays in the current buffer."
  (interactive)
  (let ((buffer (ellm-get-or-create-context-buffer)))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (recenter 4)))

(defun ellm-clear-context ()
  "Remove all context overlays."
  (interactive)
  (seq-do #'delete-overlay ellm-context-overlays)
  (setq ellm-context-overlays nil)
  (let ((buffer (ellm-get-or-create-context-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook #'ellm-clear-context t))
      (kill-buffer buffer)))
  (when (called-interactively-p 'any)
    (message "All context chunks cleared")))

(defun ellm-context--build-prompt-wrapper ()
  "Use the context accumulated in the context buffer for a prompt."
  (if-let* ((string-template
             (if (eq ellm-provider 'anthropic)
                 ellm-prompt-context-fmt-string-anthropic ellm-prompt-context-fmt-string))
            (context-overlays
             (seq-filter
              #'ellm--overlay-context-overlay-p
              (with-current-buffer (ellm-get-or-create-context-buffer)
                (overlays-in (point-min) (point-max)))))
            (context-string
             (mapconcat (lambda (ov) (ellm--prepare-context-chunk ov))
                        context-overlays)))
      (lambda (prompt)
        (format string-template context-string prompt))))

(defun ellm-org--collect-image-filepaths ()
  "Collect filepaths from all org-image-overlay links in context buffer."
  (let ((filepaths '()))
    (with-current-buffer (ellm-get-or-create-context-buffer)
      (let ((overlays (overlays-in (point-min) (point-max))))
        (dolist (overlay overlays)
          (when (overlay-get overlay 'org-image-overlay)
            (let* ((start (overlay-start overlay))
                   (end (overlay-end overlay))
                   (link-text (buffer-substring-no-properties start end)))
              (when (string-match "\\[\\[file:\\([^]]+\\)\\]" link-text)
                (let ((filepath (match-string 1 link-text)))
                  (unless (member filepath filepaths)
                    (push (match-string 1 link-text) filepaths)))))))))
  (nreverse filepaths)))

(defun ellm-context-complete ()
  "Complete the context in the context buffer."
  (interactive)
  (let ((prompt-wrapper (ellm-context--build-prompt-wrapper))
        (image-filepaths (ellm-org--collect-image-filepaths)))
    (ellm-chat nil nil prompt-wrapper nil (nth 0 image-filepaths))))

(defvar ellm-context-buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ellm-context-complete)
    (define-key map (kbd "C-c C-k") 'ellm-clear-context)
    (define-key map (kbd "d") 'ellm-org-delete)
    map)
  "Keymap for `ellm-context-buffer-mode'.")

(define-derived-mode ellm-context-buffer-mode special-mode "ellm-context"
  "Major mode for managing the ellm context buffer."
  (use-local-map (make-composed-keymap ellm-context-buffer-keymap special-mode-map))
  (add-hook 'kill-buffer-hook #'ellm-clear-context nil t))

(defun ellm-get-or-create-context-buffer ()
  "Return the context buffer, creating it if necessary."
  (let ((buffer (get-buffer-create ellm--context-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'ellm-context-buffer-mode)
        (ellm-context-buffer-mode)))
    buffer))

(defun ellm--insert-image-into-context-buffer (filepath)
  "Insert link to image at FILEPATH into the *ORG-BUFFER* buffer."
  (interactive "fSelect image file: ")
  (let ((inhibit-read-only t)
        (buffer (get-buffer-create ellm--context-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (format "[[file:%s]]\n" (expand-file-name filepath)))
      (org-display-inline-images)
      (when (called-interactively-p 'interactive)
        (display-buffer buffer)))))

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
            (define-key map (kbd "C-c ; i") #'ellm--insert-image-into-context-buffer)
            (define-key map (kbd "C-c ; d") #'ellm-remove-context-chunk)
            (define-key map (kbd "C-c ; x") #'ellm-view-context-buffer)
            (define-key map (kbd "C-c ; C-M-k") #'ellm-clear-context)
            map))

(defun ellm-cleanup ()
  "Clean up the ellm context buffer and overlays."
  (ellm-clear-context)
  (dolist (buffer (list ellm--context-buffer-name
                        ellm--log-buffer-name
                        ellm--temp-conversations-buffer-name))
    (ignore-errors (kill-buffer buffer))))

;;;###autoload
(define-globalized-minor-mode global-ellm-mode ellm-mode
  (lambda () (ellm-mode 1))
  :group 'ellm)

(add-hook 'global-ellm-mode-hook
          (lambda ()
            (unless global-ellm-mode
              (ellm-cleanup))))

(provide 'ellm)
;;; ellm.el ends here
