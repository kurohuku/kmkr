
(eval-when-compile (require 'cl))

(defmacro kr:awhen (test &rest body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro kr:aif (test then &rest else)
  `(let ((it ,test))
     (if it
	 ,then
       ,@else)))

(defmacro kr:aprogn (&rest body)
  `(let ((it nil))
     ,@(mapcar
	(lambda (x)
	  `(setq it ,x))
	body)))

(provide 'kr-anaphora)