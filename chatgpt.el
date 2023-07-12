;;; chatgpt.el --- ChatGPT in Emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Jungmin "Josh" Cho <joshchonpc@gmail.com>
;; Version: 0.2
;; Package-Requires: ((polymode "0.2.2") (dash "2.19.1") (s "1.13.1"))
;; Keywords: ai, openai, chatgpt, assistant
;; URL: https://github.com/joshcho/ChatGPT.el

;;; Commentary:

;; This package provides an interactive interface with ChatGPT API via chatgpt-wrapper.

;;; Code:

;; user facing functionality prefixed chatgpt-, developer oriented API prefixed cg-

(require 'cl-lib)
(require 'comint)
(defgroup chatgpt nil
  "Configuration for chatgpt."
  :prefix "chatgpt-"
  :group 'ai)

(defcustom chatgpt-cli-file-path
  (replace-regexp-in-string
   "\n$" "" (shell-command-to-string "which chatgpt"))
  "Path of chatgpt executable."
  :type 'string
  :group 'chatgpt)

(defcustom chatgpt-cli-arguments '()
  "Arguments for chatgpt executable."
  :type 'list
  :group 'chatgpt)

(defcustom chatgpt-prompt-regexp "^[^@]+@[^@]+>"
  "Prompt for `chatgpt-run'. Customize this if you customized chatgpt-wrapper prompt."
  :type 'string
  :group 'chatgpt)

(defconst cg-cmds '("ask" "chat" "config" "context" "copy" "delete" "echo" "editor" "exit" "file" "functions" "help" "history" "log" "login" "logout" "max-submission-tokens" "model" "nav" "new" "preset-delete" "preset-edit" "preset-load" "preset-save" "preset-show" "presets" "provider" "providers" "quit" "read" "stream" "switch" "system-message" "template" "template-copy" "template-delete" "template-edit" "template-edit-run" "template-prompt-edit-run" "template-prompt-run" "template-run" "templates" "title" "user" "user-delete" "user-edit" "user-login" "user-logout" "user-register" "users" "workflow-delete" "workflow-edit" "workflow-run" "workflow-show" "workflows"))

(defun cg-completion-at-point ()
  "Completes / commands interactively"
  (interactive)
  (let ((line
         (buffer-substring (line-beginning-position) (point))))
    (if-let (filtered-cmds
             (when (string-match "\\s-*/\\([^ ]*\\)$" line)
               (cl-remove-if-not (lambda (str)
                                   (or
                                    (string-prefix-p
                                     (match-string 1 line)
                                     str)
                                    (equal str
                                           (match-string 1 line))))
                                 cg-cmds)))
        (insert
         (substring
          (if (= (length filtered-cmds) 1)
              (car filtered-cmds)
            (completing-read "Select cmd: " filtered-cmds))
          (length (match-string 1 line))))
      (call-interactively #'completion-at-point))))

(defvar chatgpt-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" #'cg-completion-at-point)
    ;; (define-key map (kbd "RET") #'chatgpt-send-input)
    map)
  "Basic mode map for `run-chatgpt'.")

(defun cg-get-buffer ()
  "Find and return the buffer with name matching '*ChatGPT*[lang]' or '*ChatGPT*'."
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (string-match "\\*ChatGPT\\*\\(\\[.*\\]\\)?"
                                   (buffer-name (car buffers)))))
      (setq buffers (cdr buffers)))
    (car buffers)))

(defun cg-get-base-buffer ()
  (when (cg-get-buffer)
    (with-current-buffer (cg-get-buffer)
      (pm-base-buffer))))

;;;###autoload
(defun chatgpt-run ()
  "Run an inferior instance of `chatgpt-cli' inside Emacs."
  (interactive)
  (unless (getenv "OPENAI_API_KEY")
    (error "Please set the environment variable \"OPENAI_API_KEY\" to your API key"))
  (let* ((base-buffer
          (or (cg-get-base-buffer)
              (get-buffer-create "*ChatGPT*")))
         (proc-alive (comint-check-proc base-buffer)))
    ;; if process is dead, recreate base-buffer and reset mode
    (unless proc-alive
      (with-current-buffer base-buffer
        (apply 'make-comint-in-buffer "ChatGPT" base-buffer
               chatgpt-cli-file-path nil chatgpt-cli-arguments)
        (chatgpt-mode)
        (let ((continue-loop t)
              (end-time (+ (float-time (current-time))
                           cg-load-wait-time-in-secs)))
          ;; block until ready
          (while (and continue-loop (< (float-time (current-time)) end-time))
            (sleep-for 0.1)
            (when (string-match-p (concat chatgpt-prompt-regexp
                                          "[[:space:]]*$")
                                  (buffer-string))
              ;; output response from /read command received
              (setq continue-loop nil)))
          (when continue-loop
            (message
             (format
              "No response from chatgpt-wrapper after %d seconds"
              cg-load-wait-time-in-secs))))))
    ;; Regardless, provided we have a valid base-buffer, we pop to it.
    (pop-to-buffer base-buffer)))

(defun cg-initialize ()
  "Helper function to initialize ChatGPT."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode chatgpt-mode comint-mode "ChatGPT"
  "Major mode for `run-chatgpt'.

\\<chatgpt-mode-map>"
  (setq comint-prompt-regexp chatgpt-prompt-regexp)
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(cg-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) chatgpt-prompt-regexp))

(add-hook 'chatgpt-mode-hook 'cg-initialize)

;; currently doesn't work with polymode
(defvar cg-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_</" (regexp-opt cg-cmds) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `chatgpt-mode'.")

(defcustom chatgpt-code-query-map
  '(("bug" . "There is a bug in the following, please help me fix it.")
    ("doc" . "Please write the documentation for the following.")
    ("improve" . "Please improve the following.")
    ("fix" . "Please fix.")
    ("understand" . "What is the following?")
    ("refactor" . "Please refactor the following.")
    ("suggest" . "Please make suggestions for the following."))
  "An association list that maps query types to their corresponding query."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (string :tag "Query String"))
  :group 'chatgpt)

(defcustom chatgpt-display-on-query t
  "Whether *ChatGPT* is displayed when a query is sent."
  :type 'boolean
  :group 'chatgpt)

(defvar cg-load-wait-time-in-secs 10)

;; (mapc 'cancel-timer timer-list)
(defun cg-query (query &optional code)
  (let ((mode major-mode))
    (with-current-buffer (cg-get-base-buffer)
      (comint-kill-input))
    (if (or code (string-match "\n" query))
        (with-current-buffer (cg-get-base-buffer)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "/read")
            (comint-send-input)
            (let ((continue-loop t)
                  (end-time (+ (float-time (current-time))
                               cg-load-wait-time-in-secs)))
              (while (and continue-loop (< (float-time (current-time)) end-time))
                ;; block until /read command finishes
                (with-current-buffer (cg-get-buffer)
                                        ; NOT cg-get-base-buffer
                                        ; we want the cursor to update
                  (sleep-for 0.1)
                  (when (string-match-p "^ • Reading prompt, hit ^d when done, or write line with /end.[[:space:]]*\n\n$" (buffer-string))
                    ;; output response from /read command received
                    (setq continue-loop nil)
                    (let ((inhibit-read-only t))
                      (insert query)
                      (if (not code)
                          (progn
                            (insert "\n/end")
                            (call-interactively #'comint-send-input))
                        (let ((saved-point (point)))
                          ;; if possible, don't refactor this. very fragile,
                          ;; and probably won't change in the way that you expect
                          (insert code)
                          (insert "\n/end")
                          (call-interactively #'comint-send-input)
                          (save-excursion
                            (goto-char saved-point)
                            (remove-list-of-text-properties (point) (+ (point)
                                                                       (length code))
                                                            '(font-lock-face))
                            (cg-insert-syntax-highlight
                             (buffer-substring saved-point(+ saved-point (length code)))
                             mode)
                            (delete-region (point)
                                           (+ (point) (length code))))))))))
              (when continue-loop
                (message
                 (format
                  "No response from chatgpt-wrapper after %d seconds"
                  cg-load-wait-time-in-secs))))))
      ;; Else, send the query directly
      (comint-simple-send
       (get-buffer-process (cg-get-base-buffer))
       query))
    ;; Display the chatgpt buffer if necessary
    (when chatgpt-display-on-query
      (pop-to-buffer (cg-get-buffer)))))

;;;###autoload
(defun chatgpt-code-query (code)
  ;; assumes *ChatGPT* is alive
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  (let* ((query-type (completing-read "Type of Query: "
                                      (cons "custom"
                                            (mapcar #'car chatgpt-code-query-map))))
         (query
          (format "%s\n\n"
                  (cond ((assoc query-type chatgpt-code-query-map)
                         (cdr (assoc query-type chatgpt-code-query-map)))
                        ((equal query-type "custom")
                         (read-from-minibuffer "ChatGPT Custom Prompt: "))
                        (t query-type)))))
    (cg-query query code)))

;;;###autoload
(defun chatgpt-query ()
  (interactive)
  (save-window-excursion
    (chatgpt-run))
  ;; (cg-query (read-from-minibuffer "ChatGPT Query: "))
  (if (region-active-p)
      (chatgpt-code-query
       (buffer-substring-no-properties (region-beginning) (region-end)))
    (cg-query (read-from-minibuffer "ChatGPT Query: ")))
  (when chatgpt-display-on-query
    (pop-to-buffer (cg-get-buffer))))

(defun cg-insert-syntax-highlight (text mode)
  (cl-flet ((fontify-using-faces
              (text)
              (let ((pos 0)
                    next)
                (while (setq next (next-single-property-change pos 'face text))
                  (put-text-property pos next 'font-lock-face
                                     (get-text-property pos 'face text) text)
                  (setq pos next))
                (add-text-properties 0  (length text) '(fontified t) text)
                text)))
    (insert
     (fontify-using-faces
      (with-temp-buffer
        (insert text)
        (funcall mode)
        (font-lock-ensure)
        (buffer-string))))))

(defun cg-string-match-positions (regexp str)
  "Find positions of all matches of REGEXP in STR."
  (let ((pos 0)
        matches)
    (while (string-match regexp str pos)
      (setq matches (cons (match-beginning 0) matches))
      (setq pos (match-end 0)))
    (nreverse matches)))

(require 'polymode)
(define-hostmode poly-chatgpt-hostmode
  :mode 'chatgpt-mode)

(define-auto-innermode poly-chatgpt-fenced-code-innermode
  :head-matcher (cons "^\\(```{?[[:alpha:]][^\n]*\n\\)" 1)
  :tail-matcher (cons "\\(^[ \t]*\\(```\\)\\|Request to interrupt streaming\\)[ \t]*$" 1)
  :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-chatgpt-mode
  :hostmode 'poly-chatgpt-hostmode
  :innermodes '(poly-chatgpt-fenced-code-innermode))
(add-hook 'chatgpt-mode-hook #'poly-chatgpt-mode)

(require 'cl)
(lexical-let ((dir default-directory))
  (defun tokenize-string (text model)
    (with-temp-buffer
      (call-process "python" nil t nil (format "%s%s" dir "tokenizer.py")
                    text model)
      (goto-char (point-min))
      (read (current-buffer)))))

(require 'dash)
(require 's)

;; improvements: set a cap so that after, say, window-size 10, we check anywhere in the remaining string for match
(defun cg-accept-token (confirmed-tokens unconfirmed-tokens token)
  (assert (-every #'stringp confirmed-tokens))
  (assert (-every #'stringp unconfirmed-tokens))
  (if (and (>= (length unconfirmed-tokens) 1)
           (string= (car unconfirmed-tokens) token))
      (list (append confirmed-tokens (list token)) (cdr unconfirmed-tokens))
    (let ((window-size 1)
          (continue-loop t)
          (exists-match nil))
      (while (and (<= (* 2 window-size) (length unconfirmed-tokens))
                  continue-loop)
        (if (equal (last (append confirmed-tokens (list token)) window-size)
                   (-take window-size
                          (-drop window-size unconfirmed-tokens)))
            (progn (setq exists-match t)
                   (setq continue-loop nil))
          (setq window-size (1+ window-size))))
      (list
       (append confirmed-tokens (list token))
       (if exists-match
           (-drop (* window-size 2) unconfirmed-tokens)
         unconfirmed-tokens)))))

(defun cg-code-query-json (code query)
  (json-encode `(("model" . "gpt-4")
                 ("messages" . ((("role" . "system")
                                 ("content" . "You are UpdatedCodeAI. The user provides a request or a query followed by the code. Please provide the answer in prose and the full updated code. Do NOT provide just the differences. Provide the updated code in full."))
                                (("role" . "user")
                                 ("content" .
                                  ,(concat
                                    query
                                    "\n\n"
                                    code)))))
                 ("functions" .
                  ((("name" . "answer_with_code")
                    ("description" . "Provide answer and modified code with explanation.")
                    ("parameters" .
                     (("type" . "object")
                      ("properties" . (("answer" .
                                        (("type" . "string")
                                         ("description" . "Prose answer for the query.")))
                                       ("code" .
                                        (("type" . "string")
                                         ("description" . "Modified code (just the code)")))))
                      ("required" . ("answer" "code")))))))
                 ("function_call" .
                  (("name" . "answer_with_code")))
                 ("stream" . t))))

(defun cg-openai-curl-command (json-data-string)
  (let* ((url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
         (headers-string (mapconcat (lambda (header)
                                      (concat "-H \"" (car header) ": " (cdr header) "\""))
                                    headers " ")))
    (with-current-buffer "*outputs*"
      (insert
       (concat "curl -X POST "
               headers-string
               " -d \""
               (s-replace
                "`" "\\`"
                (s-replace "\"" "\\\""
                           json-data-string))
               "\" "
               url)))
    (concat "curl -X POST "
            headers-string
            " -d \""
            (s-replace
             "`" "\\`"
             (s-replace "\"" "\\\""
                        json-data-string))
            "\" "
            url)))

(defun cg-sanitize (string)
  (s-replace
   "\\\\\""
   "\""
   (s-replace
    "\\\""
    "\""
    (s-replace
     "\\n"
     "\n"
     (s-replace
      "\\\\n"
      "\n"
      string)))))

(defun cg-make-stream-filter (display-buffer uuid)
  (lexical-let ((stream-state 0)
                (confirmed-tokens '())
                (unconfirmed-tokens (tokenize-string code "gpt-4"))
                (stream-msg "")
                (stream-collected "")
                (display-buffer display-buffer)
                (uuid uuid))
    (lambda (process output)
      (flet ((transition
              ()
              (setq stream-state (1+ stream-state))
              (with-current-buffer "*deltas*"
                (goto-char (point-max))
                (insert (format "\n\n%d\n" stream-state)))))
        (with-current-buffer (process-buffer process)
          (goto-char (point-max))
          ;; catching errors, doesn't work i think
          ;; (when (string-match "^data: {\"error\":{\"message\":\"\\([^\"]*\\)\""
          ;;                     output)
          ;;   (message (match-string 1 output)))

          ;; severe debugging
          (with-current-buffer (get-buffer-create "*outputs*")
            (insert (format "\"%s\"\n" output)))
          (let ((start 0))
            (while (string-match
                    ;; abomination, captures escaped quotation marks
                    "\"arguments\":\"\\(\\(\\\\.\\|[^\"\\]\\)*\\)\""
                    output start)
              (let ((delta (match-string 1 output)))
                (with-current-buffer "*deltas*"
                  (insert (format "\"%s\", " delta)))
                (setq start (match-end 0))
                (setq stream-collected (concat stream-collected delta))
                (cond ((= stream-state 0)
                       ;; (with-current-buffer "*deltas*"
                       ;;   (insert (format "\"%s\"\n" stream-collected)))
                       (when (string-match
                              (rx (and
                                   line-start "{"
                                   (zero-or-more (or whitespace "\\n"))
                                   "\\\"answer\\\":"
                                   (zero-or-more (or whitespace "\\n"))
                                   "\\\""
                                   (group (zero-or-more not-newline))))
                              stream-collected)
                         (setq stream-msg (match-string 1 stream-collected))
                         (message "%s" (cg-sanitize stream-msg))
                         (transition)
                         (setq stream-collected "")))
                      ((= stream-state 1)
                       (if (string-match
                            "\\(.*[^\\\\]\\)\\\\\"\\(.*\\)" stream-collected)
                           (progn
                             (message "%s" (cg-sanitize (match-string 1 stream-collected)))
                             (setq stream-collected (match-string 2 stream-collected))
                             (transition))
                         (progn
                           (setq stream-msg (concat stream-msg delta))
                           (message "%s" (cg-sanitize stream-msg)))))
                      ((= stream-state 2)
                       ;; (with-current-buffer "*deltas*"
                       ;;   (insert (format "\"%s\"\n" stream-collected)))
                       (when (string-match
                              (rx (and
                                   "\\\"code\\\":"
                                   (zero-or-more (or whitespace "\\n"))
                                   "\\\""
                                   (zero-or-more (or whitespace "\\n"))
                                   (group (zero-or-more not-newline))))
                              stream-collected)
                         (setq confirmed-tokens (list
                                                 (match-string 1 stream-collected)))
                         (setq stream-collected "")
                         (transition)))
                      ((= stream-state 3)
                       (if (string-match ;; "\\([^\\]\\|^\\)\\\\\\\""
                            (rx
                             (and
                              (zero-or-more (or whitespace "\\n"))
                              "\\\""
                              (zero-or-more (or whitespace "\\n"))
                              "}"))
                            ;; "\\\"\\n}"
                            stream-collected)
                           (let ((final-string
                                  (concat (apply #'s-concat confirmed-tokens)
                                          delta)))
                             (transition)
                             (cg-replace-block display-buffer
                                               uuid
                                               (cg-sanitize
                                                (substring
                                                 final-string
                                                 0
                                                 (- (length final-string)
                                                    (length (match-string 0 stream-collected))))))
                             ;; (with-current-buffer "*test*"
                             ;;   (erase-buffer)
                             ;;   (insert
                             ;;    (cg-sanitize
                             ;;     (concat (mapconcat 'identity confirmed-tokens "")
                             ;;             (if (string-match "\\(.*\\([^\\]\\|^\\)\\)\\\\\\\""
                             ;;                               delta)
                             ;;                 (match-string 1 delta)
                             ;;               "")))))
                             )
                         (cl-destructuring-bind (new-confirmed-tokens
                                                 new-unconfirmed-tokens)
                             (cg-accept-token confirmed-tokens
                                              unconfirmed-tokens
                                              delta)
                           (setq confirmed-tokens new-confirmed-tokens
                                 unconfirmed-tokens new-unconfirmed-tokens)
                           ;; trick to not display partial \
                           (unless (equal (substring
                                           (mapconcat 'identity confirmed-tokens "")
                                           -1)
                                          "\\")
                             (cg-replace-block display-buffer
                                               uuid
                                               (cg-sanitize
                                                (concat (mapconcat 'identity confirmed-tokens "")
                                                        (mapconcat 'identity unconfirmed-tokens ""))))
                             ;; (with-current-buffer "*test*"
                             ;;   (erase-buffer)
                             ;;   (insert
                             ;;    (cg-sanitize
                             ;;     (concat (mapconcat 'identity confirmed-tokens "")
                             ;;             (mapconcat 'identity unconfirmed-tokens "")))))
                             )))))))))))))

(defun cg-run-this (code query display-buffer uuid)
  (let ((process (start-process "curl-process" "*curl-output*" "/bin/bash" "-c"
                                (cg-openai-curl-command
                                 (cg-code-query-json code query)))))
    (with-current-buffer (get-buffer-create "*test*")
      (erase-buffer)
      (insert code))
    (with-current-buffer (get-buffer-create "*deltas*")
      (erase-buffer))
    (set-process-filter
     process
     (cg-make-stream-filter display-buffer uuid))))

(require 'org-id)

(defmacro cg-with-functions (bindings &rest body)
  "Bind functions in BINDINGS and make them available in BODY.

Each element of BINDINGS is a list (FUNC ARGS ... BODY ...).
Each element of BODY is a `defun` or `defmacro` form.

Unlike `cl-flet`, the function bindings in BINDINGS are available
inside the functions defined in BODY.

Signals an error if a form in BODY is not `defun` or `defmacro`."
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (def)
                 (let ((defun (car def))
                       (name (cadr def))
                       (args (caddr def))
                       (body (cdddr def)))
                   (unless (memq defun '(defun defmacro))
                     (error "Expected 'defun or 'defmacro, got %s" defun))
                   `(,defun ,name ,args
                      (cl-flet ,bindings
                        ,@body))))
               body)))

(cg-with-functions ((line-at-pos
                     (pos)
                     (save-excursion
                       (goto-char pos)
                       (thing-at-point 'line)))
                    (on-last-line
                     (pos)
                     (eq (line-number-at-pos pos)
                         (line-number-at-pos (point-max))))
                    (on-first-line
                     (pos)
                     (eq (line-number-at-pos pos)
                         (line-number-at-pos (point-min))))
                    (line-end-position-at-pos
                     (pos)
                     (save-excursion
                       (goto-char pos)
                       (line-end-position)))
                    (line-beginning-position-at-pos
                     (pos)
                     (save-excursion
                       (goto-char pos)
                       (line-beginning-position))))
  (defun cg-get-block-start ()
    (when (region-active-p)
      (cond
       ;; check this line
       ((string-match "^# *chatgpt \\([a-z0-9-]*\\) start"
                      (line-at-pos (region-beginning)))
        (unless (on-last-line (region-beginning))
          (line-beginning-position-at-pos
           (save-excursion
             (goto-char (region-beginning))
             (next-line)
             (point)))))
       ;; check the next line
       ((and
         (not (on-first-line (region-beginning)))
         (save-excursion
           (goto-char (region-beginning))
           (previous-line)
           (string-match "\\`# *chatgpt \\([a-z0-9-]*\\) start"
                         (line-at-pos (point)))))
        (line-beginning-position-at-pos (region-beginning)))
       (t
        (line-beginning-position-at-pos (region-beginning))
        ;; (let ((region (buffer-substring
        ;;                (region-beginning)
        ;;                (region-end))))
        ;;   (if (string-match "^\\([ \t]*\n+\\)" region)
        ;;       (+ (region-beginning)
        ;;          (length (match-string 1 region)))
        ;;     (line-beginning-position-at-pos (region-beginning))))
        ))))

  (defun cg-get-block-end ()
    (when (region-active-p)
      (cond
       ;; check this line
       ((string-match "^# *chatgpt \\([a-z0-9-]*\\) end"
                      (line-at-pos (region-end)))
        (unless (on-first-line (region-end))
          (line-end-position-at-pos
           (save-excursion
             (goto-char (region-end))
             (previous-line)
             (point)))))
       ;; check the next line
       ((and
         (not (on-last-line (region-end)))
         (save-excursion
           (goto-char (region-end))
           (next-line)
           (string-match "^# *chatgpt \\([a-z0-9-]*\\) end"
                         (line-at-pos (point)))))
        (line-end-position-at-pos (region-end)))
       ((and
         (not (on-first-line (region-end)))
         (string= (line-at-pos (region-end)) "\n")
         (save-excursion
           (goto-char (region-end))
           (previous-line)
           (string-match "^# *chatgpt \\([a-z0-9-]*\\) end"
                         (line-at-pos (point)))))
        (line-end-position-at-pos
         (save-excursion
           (goto-char (region-end))
           (previous-line 2)
           (point))))
       (t
        (- (line-end-position-at-pos (region-end))
           (if (string= (line-at-pos (region-end))
                        "\n")
               1
             0))
        ;; (let ((region (buffer-substring
        ;;                (region-beginning)
        ;;                (region-end))))
        ;;   (if (string-match "\\([ \t\n]+\\)\\'" region)
        ;;       (- (region-end)
        ;;          (length (match-string 1 region)))
        ;;     (line-end-position-at-pos (region-end))))
        ))))

  (defun cg-get-block-uuid (block-start block-end)
    (when (and block-start block-end)
      (unless (on-last-line block-end)
        (let ((end-comment-line (save-excursion
                                  (goto-char block-end)
                                  (next-line)
                                  (thing-at-point 'line))))
          end-comment-line
          (when (string-match
                 "^# *chatgpt \\([a-z0-9-]*\\) end"
                 end-comment-line)
            (let ((maybe-uuid
                   (match-string 1 end-comment-line)))
              (when (and maybe-uuid (org-uuidgen-p maybe-uuid))
                (let ((uuid maybe-uuid))
                  (unless (on-first-line block-start)
                    (save-excursion
                      (goto-char block-start)
                      (previous-line)
                      (match-string 1 end-comment-line)
                      (when (string-match
                             (format
                              "^# *chatgpt %s start"
                              uuid)
                             (thing-at-point 'line))
                        (set-text-properties 0 (length uuid) nil uuid)
                        uuid)))))))))))

  (defun cg-replace-block (buffer uuid string)
    (with-current-buffer buffer
      (let ((saved-point (point)))
        (goto-char (point-min))
        (let* ((block-start
                (re-search-forward
                 (format
                  "^# *chatgpt %s start\n"
                  uuid)
                 nil t))
               (block-end
                ;; nil if block-end is nil
                (when block-start
                  ;; this (next-line) is critical for making sure that
                  ;; the block is at least 1 line high
                  (next-line)
                  (re-search-forward
                   (format
                    "^# *chatgpt %s end"
                    uuid)
                   nil t)
                  (previous-line)
                  (line-end-position))))
          (when block-end
            ;; we know block-start is non-nil
            (delete-region block-start block-end)
            (goto-char block-start)
            (insert string)
            (cond ((< saved-point block-start)
                   (goto-char saved-point))
                  ((< block-end saved-point)
                   (goto-char
                    (+
                     ;; difference between old and new blocks
                     (- (length string)
                        (- block-end block-start))
                     saved-point)))
                  (t
                   (goto-char saved-point)))))))))

(defun cg-true-comment-start (buffer)
  (with-current-buffer buffer
    (cond ((memq major-mode '(emacs-lisp-mode
                              lisp-mode
                              clojure-mode
                              scheme-mode
                              hy-mode))
           ";;")
          (t comment-start))))

;; need to properly escape things, look for the general solution
(defun chatgpt-self-edit (code query)
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           "")
         (if (region-active-p)
             (let ((query-type (completing-read "Type of Query: "
                                                (cons "custom"
                                                      (mapcar #'car chatgpt-code-query-map)))))
               (cond ((assoc query-type chatgpt-code-query-map)
                      (cdr (assoc query-type chatgpt-code-query-map)))
                     ((equal query-type "custom")
                      (read-from-minibuffer "ChatGPT Custom Prompt: "))
                     (t query-type)))
           "")))
  (flet ((block-comment-line
          (uuid suffix)
          (format
           "%schatgpt %s %s%s\n"
           (cg-true-comment-start (current-buffer))
           uuid suffix
           comment-end)))
    (if (region-active-p)
        (let* ((block-start (cg-get-block-start))
               (block-end (cg-get-block-end))
               (saved-point (point))
               (existing-uuid (cg-get-block-uuid block-start block-end))
               (uuid (or existing-uuid (org-id-new))))
          ;; insert lines if this block doesn't have block comments
          (unless existing-uuid
            (goto-char block-end)
            (newline)
            (insert (block-comment-line uuid "end"))
            (goto-char block-start)
            (insert (block-comment-line uuid "start"))
            (goto-char
             (cond ((< saved-point block-start)
                    saved-point)
                   ((< block-end saved-point)
                    (+ saved-point
                       (length (block-comment-line uuid "start"))
                       (length (block-comment-line uuid "end"))))
                   (t
                    ;; point in block
                    (+ saved-point
                       (length (block-comment-line uuid "start")))))))
          (cg-run-this code query (current-buffer) uuid))
      (chatgpt-query))))

;; (get-buffer-create "*test*")


(provide 'chatgpt)
;;; chatgpt.el ends here
