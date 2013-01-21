(defgroup textarea nil
  "Edit browser textarea."
  :group 'applications)

(defcustom textarea:dir "~/var/textserver"
  "Directory to read/write textserver files."
  :type 'directory
  :group 'textarea)

(defcustom textarea:text-file-name "text"
  "Name of textserver text file."
  :type 'string
  :group 'textarea)

(defcustom textarea:reset-file-name "reset"
  "Name of textserver reset file."
  :type 'string
  :group 'textarea)

(defcustom textarea:major-mode 'text-mode
  "Major mode to use for textarea buffer."
  :type 'function
  :group 'textarea)

(defcustom textarea:auto-save-timeout 1
  "Seconds to wait before auto-saving textarea buffer."
  :type '(choice number '(const :tag "Don't save" nil))
  :group 'textarea)

(defcustom textarea:no-auto-save-message t
  "t means that no message is shown on auto-saving."
  :type 'boolean
  :group 'textarea)

(defcustom textarea:no-buckup t
  "t means not to make backup file."
  :type 'boolean
  :group 'textarea)

(defconst textarea:buffer "*textarea*")

(defsubst textarea:file (what)
  (unless (file-exists-p textarea:dir) (make-directory textarea:dir t))
  (expand-file-name (concat (file-name-as-directory textarea:dir) what)))

(defsubst textarea:text-file ()
  (textarea:file textarea:text-file-name))

(defsubst textarea:reset-file ()
  (textarea:file textarea:reset-file-name))

(defvar textarea:auto-save-timer nil)

;; minor mode for textarea

(define-minor-mode textarea:local-mode
  "Minor mode for textarea"
  :group 'textarea
  (textarea:stop-auto-save-timer)
  (if textarea:local-mode
      ;; on
      (progn
        (set-visited-file-name (textarea:text-file))
        (set-buffer-modified-p nil)
        (rename-buffer textarea:buffer)
        (if textarea:auto-save-timeout
            (progn
              (set (make-local-variable 'auto-save-visited-file-name) t)
              (set (make-local-variable 'auto-save-timeout) nil)
              (auto-save-mode 1)
              (textarea:start-auto-save-timer))
          (auto-save-mode -1)
          (set (make-local-variable 'auto-save-default) nil))
        (set (make-local-variable 'make-backup-files) (not textarea:no-buckup))
        (add-hook 'after-revert-hook #'textarea:restore-local-mode nil t)
        (add-hook 'after-change-major-mode-hook
                  #'textarea:restore-local-mode nil t)
        (set (make-local-variable 'revert-buffer-function) 'textarea:reset))
    ;; off
    (remove-hook 'after-revert-hook #'textarea:restore-local-mode t)
    (remove-hook 'after-change-major-mode-hook #'textarea:restore-local-mode t)
    (kill-local-variable 'auto-save-visited-file-name)
    (kill-local-variable 'auto-save-timeout)
    (kill-local-variable 'auto-save-default)
    (kill-local-variable 'make-backup-files)
    (kill-local-variable 'revert-buffer-function)))

(defun textarea:restore-local-mode ()
  "Re-enable `textarea:local-mode'."
  (when (or (and (numberp textarea:local-mode)
                 (<= textarea:local-mode 0))
            (null textarea:local-mode))
    (textarea:local-mode 1)))
(put 'textarea:restore-local-mode 'permanent-local-hook t)

(defun textarea:reset (&rest ignore)
  "Reset textarea buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when (file-exists-p (textarea:reset-file))
      (insert-file-contents (textarea:reset-file)))))

;; auto-save timer

(defun textarea:start-auto-save-timer ()
  (unless textarea:auto-save-timer
    (run-with-idle-timer textarea:auto-save-timeout t #'textarea:auto-save)))

(defun textarea:stop-auto-save-timer ()
  (when textarea:auto-save-timer
    (cancel-timer textarea:auto-save-timer)
    (setq textarea:auto-save-timer nil)))

(defun textarea:auto-save ()
  (let ((buffer (get-buffer textarea:buffer)))
    (if (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (do-auto-save textarea:no-auto-save-message t))
      (textarea:stop-auto-save-timer))))

;; commands

;;;###autoload
(defun textarea ()
  "Edit browser textarea."
  (interactive)
  (let ((buffer (get-buffer-create textarea:buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (when (file-exists-p (textarea:text-file))
        (insert-file-contents (textarea:text-file)))
      (when (fboundp textarea:major-mode) (funcall textarea:major-mode))
      (goto-char (point-min))
      (textarea:local-mode 1))
    (switch-to-buffer buffer)))

;; server

(defconst textarea:server-program "textserver.rb")
(defsubst textarea:server-program (&optional bin)
  (let ((program (or bin (executable-find textarea:server-program))))
    (unless (and program (file-executable-p program))
      (error "No such server program '%s'"
             (or program textarea:server-program)))
    program))

(defun textarea:server-running-p (&optional bin)
  "Check if textserver is running."
  (let ((bin (textarea:server-program bin)))
    (= 0 (call-process bin nil nil nil "--status"))))

(defun textarea:start-server (&optional bin)
  "Start textserver."
  (interactive)
  (let* ((bin (textarea:server-program bin))
         (cmd (format "%s --start >/dev/null 2>&1" bin)))
    (unless (textarea:server-running-p bin)
      (call-process "/bin/sh" nil nil nil "-c" cmd))))

(defun textarea:stop-server (&optional bin)
  "Stop textserver."
  (interactive)
  (let ((bin (textarea:server-program bin)))
    (call-process bin nil nil nil "--stop")))

(provide 'textarea)
