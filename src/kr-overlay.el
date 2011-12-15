

(defun kr:overlay-text (o)
  (let ((buf (overlay-buffer o))
	(beg (overlay-start o))
	(end (overlay-end o)))
    (with-current-buffer buf
      (buffer-substring-no-properties beg end))))

(defun kr:goto-overlay (o)
    (let ((buf (overlay-buffer o))
	  (beg (overlay-start o)))
      (switch-to-buffer buf)
      (goto-char beg)))


(provide 'kr-overlay)