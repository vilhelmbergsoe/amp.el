;;; amp.el --- Interface for the Amp CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Shane Kennedy
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, development, ai

;;; Commentary:

;; This package provides an interactive command to run the Amp CLI
;; in a terminal buffer. If the amp command is not found, it will
;; attempt to install it via npm.

;;; Code:

(require 'term)
(require 'cl-lib)
(require 'filenotify)

(defvar amp--file-watchers nil
  "List of active file watchers for amp-modified files.")

(defvar amp--modified-files nil
  "List of files that amp has recently modified.")

(defun amp--revert-buffer-if-exists (file)
  "Revert buffer for FILE if it exists and is not modified."
  (let ((buffer (get-file-buffer file)))
    (when (and buffer (not (buffer-modified-p buffer)))
      (with-current-buffer buffer
        (revert-buffer t t t)
        (message "Auto-reverted %s" (file-name-nondirectory file))))))

(defun amp--watch-file (file)
  "Add a file watcher for FILE to auto-revert when modified."
  (when (and (file-exists-p file)
             (not (assoc file amp--file-watchers)))
    (condition-case err
        (let ((watcher (file-notify-add-watch
                       file '(change)
                       (lambda (event)
                         (when (eq (nth 1 event) 'changed)
                           (amp--revert-buffer-if-exists (nth 2 event)))))))
          (push (cons file watcher) amp--file-watchers)
          ;; Remove watcher after 30 seconds to avoid accumulating watchers
          (run-with-timer 30.0 nil
                         (lambda ()
                           (amp--remove-file-watcher file))))
      (error (message "Failed to watch file %s: %s" file (error-message-string err))))))

(defun amp--remove-file-watcher (file)
  "Remove file watcher for FILE."
  (let ((entry (assoc file amp--file-watchers)))
    (when entry
      (file-notify-rm-watch (cdr entry))
      (setq amp--file-watchers (delq entry amp--file-watchers)))))

(defun amp--parse-output-for-files (output)
  "Parse amp OUTPUT to find files being modified and watch them."
  (let ((lines (split-string output "\n")))
    (dolist (line lines)
      ;; Look for common patterns that indicate file operations
      (cond
       ;; "Creating file: path/to/file"
       ((string-match "Creating file: \\(.+\\)" line)
        (amp--watch-file (match-string 1 line)))
       ;; "Editing file: path/to/file"
       ((string-match "Editing file: \\(.+\\)" line)
        (amp--watch-file (match-string 1 line)))
       ;; "Modified: path/to/file"
       ((string-match "Modified: \\(.+\\)" line)
        (amp--watch-file (match-string 1 line)))
       ;; Look for file paths in the current directory
       ((string-match "\\./\\([^[:space:]]+\\)" line)
        (let ((file (match-string 1 line)))
          (when (file-exists-p file)
            (amp--watch-file file))))))))

(defun amp--output-filter (output)
  "Filter function to parse amp terminal output for file modifications."
  (amp--parse-output-for-files output)
  output)

(defun amp--get-project-root ()
  "Get the current project root directory."
  (cond
   ;; Try project.el first
   ((and (featurep 'project) (project-current))
    (project-root (project-current)))
   ;; Try projectile
   ((and (featurep 'projectile) (projectile-project-p))
    (projectile-project-root))
   ;; Fall back to current directory
   (t default-directory)))

(defun amp--get-project-name ()
  "Get the current project name."
  (cond
   ;; Try project.el first
   ((and (featurep 'project) (project-current))
    (if (fboundp 'project-name)
        (project-name (project-current))
      (file-name-nondirectory (directory-file-name (project-root (project-current))))))
   ;; Try projectile
   ((and (featurep 'projectile) (projectile-project-p))
    (projectile-project-name))
   ;; Fall back to directory name
   (t (file-name-nondirectory (directory-file-name default-directory)))))

(defun amp--get-buffer-name (&optional project-name)
  "Get the amp buffer name for PROJECT-NAME or current project."
  (format "*amp-%s*" (or project-name (amp--get-project-name))))

(defun amp--check-installation ()
  "Check if amp CLI is installed."
  (executable-find "amp"))

(defun amp--install-amp ()
  "Install amp CLI via npm."
  (message "Installing amp CLI via npm...")
  (let ((install-buffer (get-buffer-create "*amp-install*")))
    (with-current-buffer install-buffer
      (erase-buffer)
      (let ((proc (start-process "amp-install" install-buffer "npm" "install" "-g" "@sourcegraph/amp")))
        (set-process-sentinel proc
                             (lambda (process event)
                               (when (string-match "finished" event)
                                 (message "Amp CLI installation completed")
                                 (kill-buffer install-buffer))
                               (when (string-match "exited abnormally" event)
                                 (message "Failed to install amp CLI. Check *amp-install* buffer for details")
                                 (display-buffer install-buffer))))
        proc))))

(defun amp--start-terminal ()
  "Start amp in a terminal buffer."
  (let* ((buffer-name (amp--get-buffer-name))
         (buffer (get-buffer buffer-name))
         (project-root (amp--get-project-root)))
    (if (and buffer (term-check-proc buffer))
        (display-buffer buffer)
      (progn
        (when buffer (kill-buffer buffer))
        ;; Set environment variables before creating terminal
        (setenv "TERM" "dumb")
        (setenv "NO_COLOR" "1")
        (setenv "CI" "1")
        ;; Set default-directory to project root before creating terminal
        (let ((default-directory project-root))
          (setq buffer (make-term (substring buffer-name 1 -1) "amp"))
          (with-current-buffer buffer
            (rename-buffer buffer-name)
            ;; Ensure buffer's default-directory is set to project root
            (setq default-directory project-root)
            ;; Add output filter to watch for file modifications
            (add-hook 'comint-output-filter-functions 'amp--output-filter nil t)))
        (set-buffer buffer)
        (term-mode)
        (term-char-mode)
        (display-buffer buffer)))
    buffer))

(defun amp--find-amp-buffers ()
  "Find all amp buffers currently running."
  (cl-remove-if-not
   (lambda (buf)
     (and (string-match "^\\*amp-.*\\*$" (buffer-name buf))
          (with-current-buffer buf (term-check-proc buf))))
   (buffer-list)))

(defun amp--choose-amp-buffer ()
  "Let user choose which amp buffer to use or create a new one."
  (let ((amp-buffers (amp--find-amp-buffers)))
    (cond
     ((null amp-buffers)
      (when (yes-or-no-p "No running amp processes found. Start one for this project? ")
        'create-new))
     (t
      (let* ((choices (append (mapcar #'buffer-name amp-buffers)
                             '("Create new for this project")))
             (choice (completing-read "Choose amp process: " choices nil t)))
        (if (string= choice "Create new for this project")
            'create-new
          (get-buffer choice)))))))

(defun amp--display-and-focus-buffer (buffer)
  "Display and focus BUFFER, positioning cursor at end."
  (let ((window (get-buffer-window buffer)))
    (if window
        ;; Buffer already displayed, just select the window
        (select-window window)
      ;; Buffer not displayed, split window and show it on the left
      (split-window-right)
      (switch-to-buffer buffer))
    ;; Move cursor to end of buffer
    (with-current-buffer buffer
      (goto-char (point-max)))))

(defun amp--send-to-process (text)
  "Send TEXT to the amp process, starting it if necessary."
  (let* ((cleaned-text (string-trim-right text))
         (current-buffer-name (amp--get-buffer-name))
         (buffer (get-buffer current-buffer-name)))
    ;; If current project has an amp buffer, use it
    (if (and buffer (term-check-proc buffer))
        (progn
          (with-current-buffer buffer
          (term-send-string (get-buffer-process buffer) cleaned-text)
          (term-send-string (get-buffer-process buffer) "\n"))
          (amp--display-and-focus-buffer buffer))
      ;; Otherwise, try to find any amp buffer or ask user to choose
      (let ((chosen-buffer (amp--choose-amp-buffer)))
        (cond
         ((bufferp chosen-buffer)
          (with-current-buffer chosen-buffer
            (term-send-string (get-buffer-process chosen-buffer) cleaned-text)
            (term-send-string (get-buffer-process chosen-buffer) "\n"))
          (amp--display-and-focus-buffer chosen-buffer))
         ((eq chosen-buffer 'create-new)
          ;; User chose to create new process, start one for current project
          (amp--start-terminal)
          (setq buffer (get-buffer current-buffer-name))
          (when buffer
            ;; Wait for process to be ready before sending text
            (run-with-timer 1.0 nil
                           (lambda ()
                           (when (and buffer (term-check-proc buffer))
                           (with-current-buffer buffer
                           (term-send-string (get-buffer-process buffer) cleaned-text)
                           (term-send-string (get-buffer-process buffer) "\n"))
                           (amp--display-and-focus-buffer buffer))))
            (amp--display-and-focus-buffer buffer))
         (t
          ;; User chose not to create a process, do nothing
          nil)))))))

;;;###autoload
(defun amp--fix-region ()
  "Send selected text to amp with 'fix this: ' prefix."
  (interactive)
  (if (use-region-p)
      (let ((error-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (amp--send-to-process (concat "fix this: " error-text)))
    (message "No region selected")))

;;;###autoload
(defun amp--improve-region ()
  "Send selected text to amp with 'improve this: ' prefix."
  (interactive)
  (if (use-region-p)
      (let ((code-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (amp--send-to-process (concat "improve this: " code-text)))
    (message "No region selected")))

;;;###autoload
(defun amp--explain-region ()
  "Send selected text to amp with 'explain this: ' prefix."
  (interactive)
  (if (use-region-p)
      (let ((code-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (amp--send-to-process (concat "explain this: " code-text)))
    (message "No region selected")))

;;;###autoload
(defun amp--prompt-for-region ()
  "Send selected text to amp with custom prompt entered by user."
  (interactive)
  (if (use-region-p)
      (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (prompt (read-string "Enter prompt: "))
             (full-text (if (string-empty-p prompt)
                           selected-text
                         (concat prompt " " selected-text))))
        (amp--send-to-process full-text))
    (message "No region selected")))

;;;###autoload
(defun amp--prompt ()
  "Send text to the amp process."
  (interactive)
  (let ((text (read-string "Amp prompt: ")))
    (unless (string-empty-p text)
      (amp--send-to-process text))))

;;;###autoload
(defun amp--switch ()
  "Switch to an amp buffer."
  (interactive)
  (let ((amp-buffers (cl-remove-if-not
                      (lambda (buf)
                        (string-match "^\\*amp-.*\\*$" (buffer-name buf)))
                      (buffer-list))))
    (cond
     ((null amp-buffers)
      (message "No amp buffers found"))
     ((= 1 (length amp-buffers))
      (switch-to-buffer (car amp-buffers)))
     (t
      (let ((choice (completing-read "Choose amp buffer: "
                                     (mapcar #'buffer-name amp-buffers)
                                     nil t)))
        (switch-to-buffer choice))))))

;;;###autoload
(defun amp--kill ()
  "Kill a selected amp process."
  (interactive)
  (let ((amp-buffers (amp--find-amp-buffers)))
    (cond
     ((null amp-buffers)
      (message "No running amp processes found"))
     ((= 1 (length amp-buffers))
      (let ((buffer (car amp-buffers)))
        (when (yes-or-no-p (format "Kill amp process in %s? " (buffer-name buffer)))
          (condition-case err
              (progn
                (when (get-buffer-process buffer)
                  (delete-process (get-buffer-process buffer)))
                (kill-buffer buffer)
                (message "Killed amp process: %s" (buffer-name buffer)))
            (error (message "Failed to kill amp process: %s" (error-message-string err)))))))
     (t
      (let* ((choice (completing-read "Choose amp process to kill: "
                                      (mapcar #'buffer-name amp-buffers)
                                      nil t))
             (buffer (get-buffer choice)))
        (when (yes-or-no-p (format "Kill amp process in %s? " choice))
          (condition-case err
              (progn
                (when (get-buffer-process buffer)
                  (delete-process (get-buffer-process buffer)))
                (kill-buffer buffer)
                (message "Killed amp process: %s" choice))
            (error (message "Failed to kill amp process: %s" (error-message-string err))))))))))

;;;###autoload
(defun amp ()
  "Start the amp CLI in the current project as a terminal buffer.
If amp CLI is not installed, attempt to install it via npm."
  (interactive)
  (if (amp--check-installation)
      (amp--start-terminal)
    (if (yes-or-no-p "Amp CLI not found. Install via npm? ")
        (progn
          (amp--install-amp)
          (message "Installing amp CLI... Run 'amp' again after installation completes."))
      (message "Amp CLI installation cancelled"))))

(provide 'amp)

;;; amp.el ends here
