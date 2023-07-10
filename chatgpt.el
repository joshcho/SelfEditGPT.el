;;; chatgpt.el --- ChatGPT in Emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Jungmin "Josh" Cho <joshchonpc@gmail.com>
;; Version: 0.2
;; Package-Requires: ((polymode "0.2.2") (dash "2.19.1"))
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
                                 ("content" . "Respond with answer_with_code."))
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
                                         ("description" . "Answer for the query. Return in progressive form, with -ing")))
                                       ("code" .
                                        (("type" . "string")
                                         ("description" . "Modified code (just the code)")))))
                      ("required" . ("answer" "code")))))))
                 ("function_call" .
                  (("name" . "answer_with_code")))
                 ("stream" . t))))

(defun cg-openai-curl-command (data)
  (let* ((url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
         (headers-string (mapconcat (lambda (header)
                                      (concat "-H \"" (car header) ": " (cdr header) "\""))
                                    headers " "))
         (curl-command (concat "curl -X POST "
                               headers-string
                               " -d '" data "' "
                               url)))
    (concat "curl -X POST "
            headers-string
            " -d '" data "' "
            url)))

(defvar cg-confirmed-tokens '())
(defvar cg-unconfirmed-tokens '())
(defvar cg-stream-msg "")
(defvar cg-stream-state 0)
(defvar cg-stream-collected "")

(defun cg-sanitize (string)
  (replace-regexp-in-string
   "\\\\\\\\\\\\\\\""
   "\""
   (replace-regexp-in-string
    "\\\\\\n"
    "\n"
    (replace-regexp-in-string
     "\\\\\\\\\\n"
     "\n"
     string))))

(defun cg-run-this (code query)
  (let ((process (start-process "curl-process" "*curl-output*" "/bin/bash" "-c"
                                (cg-openai-curl-command
                                 (cg-code-query-json code query)))))
    (setq cg-stream-state 0)
    (setq cg-confirmed-tokens '())
    (setq cg-unconfirmed-tokens (tokenize-string code "gpt-4"))
    (setq cg-stream-msg "")
    (setq cg-stream-collected "")
    (with-current-buffer (get-buffer-create "*test*")
      (erase-buffer)
      (insert code))
    (with-current-buffer (get-buffer-create "*deltas*")
      (erase-buffer))

    (set-process-filter
     process
     (lambda (process output)
       ;; This function could be called multiple times if the output is large,
       ;; so we append to the process buffer rather than erasing it each time.
       (with-current-buffer (process-buffer process)
         (goto-char (point-max))
         ;; (with-current-buffer "*deltas*"
         ;;   (insert (format "\"%s\"\n" output)))
         (let ((start 0))
           (while (string-match
                   ;; abomination, captures escaped quotation marks
                   "\"arguments\":\"\\(\\(\\\\.\\|[^\"\\]\\)*\\)\""
                   output start)
             (let ((delta (match-string 1 output)))
               (with-current-buffer "*deltas*"
                 (insert (format "\"%s\", " delta)))
               (setq start (match-end 0))
               (setq cg-stream-collected (concat cg-stream-collected delta))
               (cond ((= cg-stream-state 0)
                      ;; (with-current-buffer "*deltas*"
                      ;;   (insert (format "\"%s\"\n" cg-stream-collected)))
                      (when (string-match
                             (rx (and
                                  line-start "{"
                                  (zero-or-more (or whitespace "\\n"))

                                  "\\\"answer\\\":"
                                  (zero-or-more (or whitespace "\\n"))
                                  "\\\""
                                  (group (zero-or-more not-newline))))
                             cg-stream-collected)
                        (setq cg-stream-msg (match-string 1 cg-stream-collected))
                        (message "%s" (cg-sanitize cg-stream-msg))
                        (cg-transition)
                        (setq cg-stream-collected "")))
                     ((= cg-stream-state 1)
                      (if (string-match
                           "\\(.*[^\\\\]\\)\\\\\"\\(.*\\)" cg-stream-collected)
                          (progn
                            (message "%s" (cg-sanitize (match-string 1 cg-stream-collected)))
                            (setq cg-stream-collected (match-string 2 cg-stream-collected))
                            (cg-transition))
                        (progn
                          (setq cg-stream-msg (concat cg-stream-msg delta))
                          (message "%s" (cg-sanitize cg-stream-msg)))))
                     ((= cg-stream-state 2)
                      ;; (with-current-buffer "*deltas*"
                      ;;   (insert (format "\"%s\"\n" cg-stream-collected)))
                      (when (string-match
                             (rx (and   
                                  "\\\"code\\\":"
                                  (zero-or-more (or whitespace "\\n"))
                                  "\\\""
                                  (group (zero-or-more not-newline))))
                             cg-stream-collected)
                        (setq cg-confirmed-tokens (list
                                                   (match-string 1 cg-stream-collected)))
                        (setq cg-stream-collected "")
                        (cg-transition)))
                     ((= cg-stream-state 3)
                      (if (string-match "\\([^\\]\\|^\\)\\\\\\\"" cg-stream-collected)
                          (progn
                            (cg-transition)
                            ;; (with-current-buffer "*deltas*"
                            ;;   (insert (format "\"%s\"\n" delta))
                            ;;   (insert (format "\"%s\"\n" (match-string 1 delta))))
                            (with-current-buffer "*test*"
                              (erase-buffer)
                              (insert
                               (cg-sanitize
                                (concat (mapconcat 'identity cg-confirmed-tokens "")
                                        (if (string-match "\\(.*\\([^\\]\\|^\\)\\)\\\\\\\""
                                                          delta)
                                            (match-string 1 delta)
                                          ""))))))
                        (cl-destructuring-bind (new-confirmed-tokens
                                                new-unconfirmed-tokens)
                            (cg-accept-token cg-confirmed-tokens
                                             cg-unconfirmed-tokens
                                             delta)
                          (setq cg-confirmed-tokens new-confirmed-tokens
                                cg-unconfirmed-tokens new-unconfirmed-tokens)
                          ;; trick to not display partial \
                          (unless (equal (substring
                                          (mapconcat 'identity cg-confirmed-tokens "")
                                          -1)
                                         "\\")
                            (with-current-buffer "*test*"
                              (erase-buffer)
                              (insert
                               (cg-sanitize
                                (concat (mapconcat 'identity cg-confirmed-tokens "")
                                        (mapconcat 'identity cg-unconfirmed-tokens ""))))))))))))))))))

(defun cg-transition ()
  (setq cg-stream-state (1+ cg-stream-state))
  (with-current-buffer "*deltas*"
    (goto-char (point-max))
    (insert (format "\n\n%d\n" cg-stream-state))))

;; (cg-run-this
;;  "def flatten(list_of_dicts):
;;     flat_dict = {}
;;     for dict_ in list_of_dicts:
;;         flat_dict.update(dict_)
;;     return flat_dict"
;;  "Please improve this.")

;; need to properly escape things, look for the general solution
(defun chatgpt-self-edit (code query)
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           "")
         (let ((query-type (completing-read "Type of Query: "
                                            (cons "custom"
                                                  (mapcar #'car chatgpt-code-query-map)))))
           (cond ((assoc query-type chatgpt-code-query-map)
                  (cdr (assoc query-type chatgpt-code-query-map)))
                 ((equal query-type "custom")
                  (read-from-minibuffer "ChatGPT Custom Prompt: "))
                 (t query-type)))))
  (cg-run-this code query))

;; (get-buffer-create "*test*")


(provide 'chatgpt)
;;; chatgpt.el ends here
