
(eval-when-compile (require 'cl))

(defmacro kr:with-gensyms (syms &rest body)
  `(let ,(mapcar
	  #'(lambda (s) `(,s (gensym)))
	  syms)
     ,@body))

(defun kr:make-gensym-list (n)
  (loop repeat n collect (gensym)))

(defun kr:symb (expr &rest args)
  (intern
   (apply #'concat
	  (format "%s" expr)
	  (mapcar
	   #'(lambda (s)
	       (format "%s" s))
	   args))))

(provide 'kr-symbol)
