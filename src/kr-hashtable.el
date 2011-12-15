

(defun kr:maphash-keys (fn ht)
  (loop 
   for key being the hash-keys in ht
   collect (funcall fn key)))

(defun kr:maphash-values (fn ht)
  (loop 
   for val being the hash-values in ht
   collect (funcall fn val)))

(defun kr:hash-table-keys (ht)
  (loop for key being the hash-keys in ht
	collect key))

(defun kr:hash-table-values (ht)
  (loop for val being the hash-values in ht
	collect val))


(defun kr:hash-table->alist (ht)
  (loop for val being the hash-values in ht
	using (hash-key key)
	collect (cons key val)))

(defun kr:alist->hash-table (alist &rest hash-table-keywords)
  (loop
   with ht = (apply #'make-hash-table hash-table-keywords)
   for (key . val) in alist
   do (setf (gethash key ht) val)
   finally (return ht)))

	       
(defmacro* kr:do-hash-table ((key sym) ht &rest body)
  `(maphash
    (lambda (,key ,sym)
      ,@body)
    ,ht))

(provide 'kr-hashtable)