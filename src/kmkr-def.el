
(eval-when-compile (require 'cl))

(defmacro kr:defkeys (map &rest clauses)
  (let ((definitions ;; ((key command) ...)
          (loop for rest on clauses by 'cddr
                collect (subseq rest 0 2))))
    `(progn
       ,@(mapcar
          (lambda (def)
            `(define-key ,map ,(car def) ,(cadr def)))
          definitions))))
(put 'kr:defkeys 'lisp-indent-function 1)

(provide 'kmkr-def)