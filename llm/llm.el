;;; llm.el --- Description -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "27.1"))
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

(defgroup llm nil
  "Make API calls to LLMs."
  :group 'tools
  :prefix "llm-")

(defcustom llm-openai-api-key (string-trim (shell-command-to-string "pass openai/api_key"))
  "Your OpenAI API key."
  :type 'string
  :group 'llm)

(defconst llm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defcustom llm-context-prefix "Context\n"
  "The prefix to use when building the system message."
  :type 'string
  :group 'llm)

(defcustom llm-context-suffix "\nFormat your response for =org-mode= instead of =markdown=.\nIn particular, format code blocks as `#+BEGIN_SRC lang\n<CODE>\n#+END_SRC' instead of surrounding it with triple backticks and inline code should be surrounded by `~' characters instead of single backticks.\n.Moreover, if using any headings, start at depth 3 with `***' instead of `#'."
  "The prefix to use when building the system message."
  :type 'string
  :group 'llm)

(defcustom llm-openai-default-model "gpt-4-turbo-preview"
  "The default model to use when sending requests to OpenAI."
  :type 'string
  :group 'llm)

(defun llm--insert-region (start end)
  "Return the selected region (from START to END) as a string."
  (buffer-substring-no-properties start end))

(defun llm--surround-with-prefix-and-suffix (str)
  "Enclose STR in the context prefix and suffix."
  (concat llm-context-prefix str llm-context-suffix))

(defun llm--openai-request (body prompt &optional context)
  "Send a request to OpenAI with the given BODY.
Pass the PROMPT to the response handler for visualizing the conversation.
Optionally, include the CONTEXT of the prompt for the response handler."
  (let* ((bearer-token (concat "Bearer " llm-openai-api-key))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,bearer-token))))
    (let ((url-request-method "POST")
          (url-request-extra-headers headers)
          (url-request-data body))
      ;; Debug: Print the headers to *Messages* buffer for inspection.
      (message "Headers: %S" url-request-extra-headers)
      (message "Method: %S" url-request-method)
      (message "Body: %S" url-request-data)
      (url-retrieve llm--openai-api-url #'llm--openai-handle-response (list prompt context)))))


(defun llm--openai-chat (beg end prompt &optional max-tokens temperature model)
  "Send the PROMPT to OpenAI using the region (BEG to END) as context."
  (interactive "r\nsEnter your prompt:")
  (let ((region-content (llm--insert-region beg end)))
    (let* ((context
            (llm--surround-with-prefix-and-suffix
             (or region-content "You are a useful assistant")))
           (effective-model (or model llm-openai-default-model))
           (effective-temperature (or temperature 0.2))
           (effective-max-tokens (or max-tokens 1000))
           (messages `((("role" . "system") ("content" . ,context))
                       (("role" . "user") ("content" . ,prompt))))
           (request-body (json-encode `(("model" . ,effective-model)
                                        ("messages" . ,messages)
                                        ("temperature" . ,effective-temperature)
                                        ("max_tokens" . ,effective-max-tokens)))))
      (message "Request body: %S" request-body)
      (llm--openai-request request-body prompt region-content))))

(defun llm--extract-response-content (response)
  "Extract the content of the json RESPONSE."
  ;; Parse the JSON string. The result is a hash table where each key is a string.
  (let* ((parsed-json (json-parse-string response))
         (choices (gethash "choices" parsed-json))
         ;; Note: `aref` is used to access elements of vectors (arrays) in Elisp.
         (first-choice (aref choices 0))
         (message (gethash "message" first-choice))
         (content (gethash "content" message)))
    (or content
        (error
         (concat "Failed to extract content from JSON response:\n" response)))))

(defun llm--openai-handle-response (status &optional prompt context)
  "Handle response. Information about the request is contained in STATUS.
Optionally, include the original PROMPT for visualizing the whole conversation.
You can also include the CONTEXT of the prompt for the response handler."
  ;; Check for error in the response
  (when (plist-get status :error)
    (error "Request failed: %s" (plist-get status :error)))
  ;; Move to the end of the headers in the response
  (goto-char url-http-end-of-headers)
  ;; Extract the response body
  (let ((response (buffer-substring-no-properties (point) (point-max))))
    ;; Create a new buffer or switch to it if it already exists
    (let ((response-buffer (get-buffer-create "*LLM Chats*")))
      ;; (content (llm--extract-and-wrap-content response)))
      ;; to the dedicated buffer in another window
      (with-current-buffer response-buffer
        ;; Optionally, clear the buffer before inserting new content
        (org-mode)
        ;;(erase-buffer)
        (org-fold-hide-sublevels 1)
        (goto-char (point-max))
        ;; Insert the response
        (org-insert-heading nil nil t)
        (insert "New prompt")
        (when context
          (org-insert-heading nil nil t)
          (org-demote)
          (insert "Context")
          (org-insert-structure-template
           (concat "src\n" context)))
        (when prompt
          (org-insert-heading)
          (insert "User")
          (org-insert-structure-template
           (concat "quote\n" prompt)))
        (let ((response-content (llm--extract-response-content response)))
          (org-insert-heading)
          (insert "Assistant\n" response-content)))
      ;; Save the buffer to a file
      (llm--save-conversations)
      ;; Finally, display the buffer with the response
      (display-buffer response-buffer))))

(defun llm--save-conversations ()
  "Save the current LLM conversation to a file."
  (interactive)
  (let ((file-path "~/.llm/conversations.org"))
    (with-current-buffer "*LLM Chats*"
      (write-region (point-min) (point-max) file-path))))

(defun llm--load-conversations ()
  "Load the LLM conversation from a file."
  (interactive)
  (let ((file-path "~/.llm/conversations.org"))
    (when (file-exists-p file-path)
      (with-current-buffer (get-buffer-create "*LLM Chats*")
        (erase-buffer)
        (insert-file-contents file-path)
        (org-mode)))))

;;;###autoload
(define-minor-mode llm-mode
  "Minor mode for interacting with LLMs."
  :lighter " LLM"
  :group 'llm
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c /") #'llm--openai-chat)
            map)
  (if llm-mode
      (message "LLM mode enabled")
    (message "LLM mode disabled")))

(provide 'llm)
;;; llm.el ends here
