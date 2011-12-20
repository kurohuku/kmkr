

(defun kr:kill-word-or-active-region ()
  (interactive)
  (if (and transient-mark-mode mark-active) ;; (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-word)))

(defun kr:copy-word-or-active-region ()
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active) ;; (region-active-p)
	(call-interactively 'kill-ring-save)
      (let ((pos (point)))
	(forward-word)
	(unless (= pos (point))
	  (kill-ring-save pos (point)))))))

(defun kr:other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun kr:move-bol+scroll-down ()
  (interactive)
  (when (and (eq last-command 'kr:move-bol+scroll-down)
	     (= (point) (line-beginning-position)))
    (call-interactively 'scroll-down))
  (call-interactively 'move-beginning-of-line))

(defun kr:move-eol+scroll-up ()
  (interactive)
  (when (and (eq last-command 'move-eol+scroll-up)
	     (= (point) (line-end-position)))
    (call-interactively 'scroll-up))
  (call-interactively 'move-end-of-line))

(provide 'kr-simple)
