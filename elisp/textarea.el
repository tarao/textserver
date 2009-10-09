(defvar textarea-dir "/var/textserver")

(defun textarea ()
  (interactive)
  (find-file (mapconcat 'identity (list textarea-dir "text") "/"))
  (funcall initial-major-mode)
  (rename-buffer "*textarea*"))

(defun textarea-auto-save-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*textarea*")))
    (if buffer (auto-save-buffer buffer))))
(run-with-idle-timer 3 t 'textarea-auto-save-buffer)

(defun textarea-import ()
  (interactive)
  (textarea)
  (kill-region (point-min) (point-max))
  (insert-file-contents (join-to-string "/" (list textarea-dir "feedback"))))

(defadvice revert-buffer
  (around textarea-revert first activate)
  (if (string= (buffer-name) "*textarea*") (textarea-import) ad-do-it))

(provide 'textarea)
