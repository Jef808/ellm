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
;; Homepage: https://github.com/jfa/llm
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

(defvar llm--openai-api-key (string-trim (shell-command-to-string "pass openai/api_key"))
  "Your OpenAI API key.")

(defvar llm--openai-api-url "https://api.openai.com/v1/chat/completions"
  "The URL to send requests to the OpenAI API.")

(defvar llm--context-prefix "* Context\n"
  "The prefix to use when building the system message.")

(defvar llm--context-suffix "\n* Format\nYour response should be formatted for =org-mode=."
  "The prefix to use when building the system message.")

(defvar llm--openai-default-model "gpt-4-turbo-preview"
  "The default model to use when sending requests to OpenAI.")

(defun llm--insert-region (start end)
  "Return the selected region (from START to END) as a string."
  (buffer-substring-no-properties start end))

(defun llm--surround-with-prefix-and-suffix (str)
  "Enclose STR in the context prefix and suffix."
  (concat llm--context-prefix str llm--context-suffix))

(defun llm--openai-request (body)
  "Send a request to OpenAI with the given BODY."
  (let* ((bearer-token (concat "Bearer " llm--openai-api-key))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,bearer-token))))
    (let ((url-request-method "POST")
          (url-request-extra-headers headers)
          (url-request-data body))
      ;; Debug: Print the headers to *Messages* buffer for inspection.
      (message "Headers: %S" url-request-extra-headers)
      (message "Method: %S" url-request-method)
      (message "Body: %S" url-request-data)
      (url-retrieve llm--openai-api-url #'llm--openai-handle-response))))


(defun llm--openai-chat (beg end prompt &optional max-tokens temperature model)
  "Send the PROMPT to OpenAI using the region (BEG to END) as context."
  (interactive "r\nsEnter your prompt:")
  (let* ((region-context
          (llm--surround-with-prefix-and-suffix
           (or (llm--insert-region beg end) "You are a useful assistant")))
         (effective-model (or model llm--openai-default-model))
         (effective-temperature (or temperature 0.2))
         (effective-max-tokens (or max-tokens 1000))
         (messages `((("role" . "system") ("content" . ,region-context))
                     (("role" . "user") ("content" . ,prompt))))
         (request-body (json-encode `(("model" . ,effective-model)
                                      ("messages" . ,messages)
                                      ("temperature" . ,effective-temperature)
                                      ("max_tokens" . ,effective-max-tokens)))))
    (message "Request body: %S" request-body)
    (llm--openai-request request-body)))

(defun llm--wrap-string (str &optional width)
  "Wrap STR to a maximum of WIDTH characters."
  (with-temp-buffer
    (insert str)
    (let ((fill-column (or width 80)))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

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

(defun llm--openai-handle-response (status)
  "Handle response. Information about the request is contained in STATUS."
  ;; Check for error in the response
  (when (plist-get status :error)
    (error "Request failed: %s" (plist-get status :error)))
  ;; Move to the end of the headers in the response
  (goto-char url-http-end-of-headers)
  ;; Extract the response body
  (let ((response (buffer-substring-no-properties (point) (point-max))))
    ;; Create a new buffer or switch to it if it already exists
    (let ((response-buffer (get-buffer-create "*HTTP Response*")))
          ;; (content (llm--extract-and-wrap-content response)))
      ;; to the dedicated buffer in another window
      (with-current-buffer response-buffer
        ;; Optionally, clear the buffer before inserting new content
        (erase-buffer)
        ;; Insert the response
        (insert (llm--extract-response-content response))
        ;; Optionally, set the major mode of the buffer. For example, if the response is JSON, you might want to use `json-mode`.
        (org-mode))
      ;; Finally, display the buffer with the response
      (display-buffer response-buffer))))
;;What this modified function does is:
;;1. Checks if there was an error in the HTTP request and reports it if present.
;;2. Extracts the response body from the current buffer (which contains the raw HTTP response).
;;3. Creates a new buffer named `*HTTP Response*` or switches to it if it already exists.
;;4. Clears the existing content of the buffer (to ensure that you're seeing only the latest response).
;;5. Inserts the HTTP response into this buffer.
;;6. Optionally, you can set the major mode of the buffer to something appropriate for the content, such as `json-mode` if you're dealing with JSON responses. This step is commented out and can be activated if needed.
;;7. Finally, it displays the buffer containing the response in another window, making it easy to view alongside other work.
;;
;;This approach ensures that you have a dedicated space for viewing HTTP responses, which can be particularly useful for debugging or monitoring responses from multiple requests over time.

;; Example usage:
(let ((json-response "{\\"name\\":\\"John Doe\\",\\"age\\":30}"))
  (message "Name: %s" (parse-json-and-access-field json-response "name"))
  (message "Age: %d" (parse-json-and-access-field json-response "age")))


(provide 'llm)
;;; llm.el ends here
