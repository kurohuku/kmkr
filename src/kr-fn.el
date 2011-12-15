
(eval-when-compile (require 'cl))

(defun kr:compose (fn &rest more-fns)
  (reduce
   (lambda (curr prev)
     (lexical-let ((curr curr)
		   (prev prev))
       (lambda (&rest args)
	 (funcall curr (apply prev args)))))
   more-fns
   :initial-value fn))

(defun kr:curry (fn &rest arguments)
  (lexical-let ((fn fn)
		(arguments arguments))
    (lambda (&rest more)
      (apply fn (append arguments more)))))

(defun kr:rcurry (fn &rest arguments)
  (lexical-let ((fn fn)
		(arguments arguments))
    (lambda (&rest more)
      (apply fn (append more arguments)))))

(defmacro kr:fn (args &rest body)
  `(lambda ,args
     (lexical-let
	 ,(mapcar (lambda (a) `(,a ,a)) args)
       ,@body)))

kr:f
kr:conjoin
kr:disjoin
kr:chain

(defmacro kr:cut (expr &rest rest)
  "SRFI 26 `cut'"
  (when (and rest
	     (or (>= (count '<...> rest) 2)
		 (and (= (count '<...> rest) 1)
		      (not (eq (car (last rest)) '<...>)))))
    (error "invalid arguments"))
  (let* ((syms (loop repeat (count '<> rest) collect (gensym)))
	 (rest-slot (when (eq (car (last rest) '<...>)) (gensym)))
	 (fnsym (gensym)))
    (if (eq expr '<>)
	`(lambda (,fnsym ,@syms ,@(and rest-slot '(&rest ,rest-slot)))
	   (apply ,fnsym
		    ,@(loop
			 for a in rest
			 collect (if (eq a '<>) (pop syms) a))
		    
			   

(provide 'kr-fn)