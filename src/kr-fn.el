
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

(defmacro kr:$ (expr &rest rest)
  "ref: Gauche, Haskell, ets"
  (reduce
   #'(lambda (acc x)
       (if (eq x '$)
	   (list acc)
	 (cons x acc)))
   (reverse (cons expr rest))
   :initial-value nil))

;;kr:f
;;kr:conjoin
;;kr:disjoin
;;kr:chain

(defmacro kr:cut (expr &rest rest)
  "ref: SRFI-26 `cut'"
  (when
      (let ((c (count '<...> rest)))
	(or (> c 1)
	    (and (= c 1)
		 (not
		  (eq '<...> (car (last rest)))))))
    (error "error: <...>"))
  (let ((syms (loop repeat (count '<> rest) collect (gensym)))
	(rest-slot (when (eq (car (last rest)) '<...>) (gensym)))
	(fnsym (gensym)))
    (if (eq expr '<>)
	`(lambda (,fnsym ,@syms ,@(and rest-slot `(&rest ,rest-slot)))
	   (apply ,fnsym
		  ,@(loop
		     for x in rest
		     collect (if (eq x '<>) (pop syms) x))
		  ,rest-slot))
      `(lambda (,@syms ,@(and rest-slot `(&rest ,rest-slot)))
	 (apply ,expr
		,@(loop
		   for x in rest
		   collect (if (eq x '<>) (pop syms) x))
		,rest-slot)))))


(defmacro kr:cute (expr &rest rest)
  "ref: SRFI-26 `cute'"
  (when
      (let ((c (count '<...> rest)))
	(or (> c 1)
	    (and (= c 1)
		 (not
		  (eq '<...> (car (last rest)))))))
    (error "error: <...>"))
  (let ((syms (loop repeat (count '<> rest) collect (gensym)))
	(evalled (loop for x in rest
		       unless (or (eq x '<>) (eq x '<...>))
		       collect (gensym)))
	(rest-slot (when (eq (car (last rest)) '<...>) (gensym)))
	(fnsym (gensym)))
    `(lexical-let
	 ,(loop
	   with s = (copy-list evalled)
	   for x in rest
	   unless (or (eq x '<>) (eq x '<...>))
	   collect `(,(pop s) ,x))
       ,(if (eq expr '<>)
	    `(lambda (,fnsym ,@syms ,@(and rest-slot `(&rest ,rest-slot)))
	       (apply ,fnsym
		      ,@(loop
			 for x in rest
			 unless (eq x '<...>)
			 collect (if (eq x '<>) (pop syms) (pop evalled)))
		      ,rest-slot))
	  `(lexical-let ((,fnsym ,expr))
	     (lambda (,@(copy-list syms) ,@(and rest-slot `(&rest ,rest-slot)))
	       (apply ,fnsym
		      ,@(loop
			 for x in rest
			 unless (eq x '<...>)
			 collect (if (eq x '<>) (pop syms) (pop evalled)))
		      ,rest-slot)))))))


(provide 'kr-fn)
