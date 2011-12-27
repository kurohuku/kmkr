
(require 'cl)

(defun* kr:buffer-directory-name (&optional (buf (current-buffer)))
  (file-name-directory (buffer-file-name buf)))

(defun* kr:count-lines-buffer (&optional (buf (current-buffer)))
  (with-current-buffer buf
    (count-lines (point-min) (point-max))))

(defun* kr:buffer-read-line (buf beg)
  (save-excursion
    (with-current-buffer buf
      (goto-char beg)
      (let ((eol-pos (line-end-position)))
      (if (= beg eol-pos)
	  (cons "" nil)
	(cons (buffer-substring-no-properties beg eol-pos)
	      (if (= eol-pos (point-max)) nil (1+ eol-pos))))))))
	

(defmacro* kr:do-buffer-lines ((var &optional (buf (current-buffer))) &rest body)
  (let ((gbuf (gensym))
	(gnext (gensym)))
    `(save-excursion
       (loop
	with ,gbuf = ,buf
	for (,var . ,gnext) = (kr:buffer-read-line ,gbuf 1)
	then (kr:buffer-read-line ,gbuf ,gnext)
	while ,gnext
	do (progn ,@body)))))
(put 'kr:do-buffer-lines 'lisp-indent-function 1)

(provide 'kmkr-buffer)
