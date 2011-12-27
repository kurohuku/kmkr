;; kr:destructuring-case

(defmacro* kr:destructuring-case (keyform &body clauses)
  "ref: Alexandria `destructuring-case'"
  (let ((g (gensym)))
    (flet ((expand-destructuring-case (key clauses case)
	     `(let ((,g ,key))
		(if (typep ,g 'cons)
		    (,case (car ,g)
		      ,@(mapcar (lambda (clause)
				  (destructuring-bind ((keys . lambda-list) &body body) clause
				    `(,keys
				      (destructuring-bind ,lambda-list (cdr ,g)
					,@body))))
				clauses))
		    (error "Invalid key to DESTRUCTURING-%s: %s" ',case ,g)))))
      (expand-destructuring-case keyform clauses 'case))))

(defmacro kr:let1 (var expr &rest body)
  `(let ((,var ,expr))
     ,@body))
(put 'kr:let1 'lisp-indent-function 2)

(defmacro kr:if-let (bind then &rest else)
  `(let (,bind)
     (if ,(car bind)
	 ,then
       ,@else)))

(defmacro kr:if-let1 (var expr then &rest else)
  `(let ((,var ,expr))
     (if ,var
	 ,then
       ,@else)))
(put 'kr:if-let1 'lisp-indent-function 2)

(defmacro kr:when-let (bind &rest body)
  `(let (,bind)
     (when ,(car bind)
       ,@body)))
(put 'kr:when-let 'lisp-indent-function 1)

(defmacro kr:and-let* (binds &rest body)
  (if binds
      `(let (,(car binds))
	 (when ,(caar binds)
	   (kr:and-let* ,(cdr binds) ,@body)))
    `(progn ,@body)))
(put 'kr:and-let* 'lisp-indent-function 1)


(defmacro* kr:once-only ((&rest vars) &rest body)
  (let ((syms (loop for v in vars collect (gensym))))
    `(let ,(loop for g in syms collect `(,g (gensym)))
       `(let (,,@(loop for v in vars for g in syms collect (list 'list g v)))
	  ,(let ,(loop for v in vars for g in syms collect `(,v ,g))
	     ,@body)))))
(put 'kr:once-only 'lisp-indent-function 1)


(provide 'kmkr-control)