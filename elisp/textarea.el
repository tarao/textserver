(defvar textarea-dir "/var/textserver")

(defun textarea ()
  (interactive)
  (find-file (mapconcat 'identity (list textarea-dir "text") "/"))
  (funcall initial-major-mode)
  (make-variable-buffer-local 'make-backup-file)
  (setq make-backup-files nil)
  (auto-save-mode 0)
  (rename-buffer "*textarea*"))

(defun textarea-auto-save-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*textarea*")))
    (when buffer
      (save-excursion
        (set-buffer buffer)
        (if (and buffer-file-name
                 (buffer-modified-p)
                 (not buffer-read-only)
                 (file-writable-p buffer-file-name))
            (save-buffer))))))
(run-with-idle-timer 3 t 'textarea-auto-save-buffer)

(defun textarea-import ()
  (interactive)
  (textarea)
  (kill-region (point-min) (point-max))
  (insert-file-contents (mapconcat
                         'identity (list textarea-dir "reset") "/")))

(defadvice revert-buffer
  (around textarea-revert first activate)
  (if (string= (buffer-name) "*textarea*") (textarea-import) ad-do-it))

(provide 'textarea)
