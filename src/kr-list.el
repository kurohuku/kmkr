
(eval-when-compile (require 'cl))

(defun kr:flatten (lst)
  (let ((rest lst)
	(curr nil)
	(result nil))
    (while (or rest curr)
      (cond
       ((and curr (listp curr))
	(psetf rest (cons (cdr curr) rest)
	       curr (car curr)))
       ((and curr (atom curr))
	(push curr result)
	(setf curr nil))
       ((and rest (listp rest))
	(psetf curr (car rest)
	       rest (cdr rest)))
       ((and rest (atom rest))
	(push rest result)
	(setf rest nil))))
    (nreverse result)))

(defun* kr:iota (count &optional (start 0) (step 1))
  (loop
   repeat count
   for i from start by step
   collect i))

(defun kr:drop (i x)
  (nthcdr i x))
(defun kr:drop-while (predicate lst)
  (loop
   named drop-while
   for rest = lst then (cdr rest)
   while rest
   while (funcall predicate (car rest))
   finally (return-from drop-while rest)))
(defun kr:drop-until (predicate lst)
  (loop
   named drop-until
   for rest = lst then (cdr rest)
   while rest
   until (funcall predicate (car rest))
   finally (return-from drop-until rest)))

(defun kr:take (i x)
  (subseq x 0 i))
(defun kr:take-while (predicate lst)
  (loop
   for elm in lst
   while (funcall predicate elm)
   collect elm))
(defun kr:take-until (predicate lst)
  (loop
   for elm in lst
   until (funcall predicate elm)
   collect elm))

(defmacro* kr:with-collector (collector &body body)
  (let* ((cols (if (listp collector) collector (list collector)))
	 (len (length cols))
	 (results (loop repeat len collect (gensym)))
	 (arg (gensym)))
    (unless (every #'symbolp cols)
      (error "invalid form"))
    `(let ,results
       (macrolet
	   ,(map
	     'list
	     (lambda (c r)
	       `(,c (,arg) `(push ,,arg ,',r)))
	     cols
	     results)
	 ,@body
	 ,(if (listp collector)
	      `(list ,@(mapcar
			(lambda (r)
			  `(nreverse ,r))
			results))
	    `(nreverse ,(car results)))))))

(put 'kr:with-collector 'lisp-indent-function 1)
;; (kr:with-collector a (dotimes (i 5) (a i))) => (0 1 2 3 4)
;; (kr:with-collector (a b) (a 1) (b 2)) => ((1) (2))
    

;;do-plist
;;do-alist

(defun kr:mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defun kr:range (start end)
  (loop for i from start to end collect i))

;;split
;;group


(defalias 'kr:filter #'remove-if-not)
(defalias 'kr:keep #'remove-if-not)

(defun kr:zip (&rest args)
  (apply #'map 'list #'list args))
       
(defun kr:unzip (lst)
  (when (and lst (listp (car lst)))
    (let ((results (make-vector (length (car lst)) nil)))
      (dolist (elms lst)
	(loop
	 for i from 0
	 for e in elms
	 do (push e (svref results i))))
      (map 'vector #'nreverse results))))


(defun kr:alist->plist (alist)
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

(defun kr:plist->alist (plist)
  (let (alist)
    (do ((tail plist (cddr tail)))
	((endp tail) (nreverse alist))
      (push (cons (car tail) (cadr tail)) alist))))

(defun kr:mappend (fun &rest lists)
  (loop for results in (apply #'mapcar fun lists)
       append results))




(provide 'kr-list)