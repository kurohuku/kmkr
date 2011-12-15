
(eval-when-compile (require 'cl))

(defmacro kr:defkeys (map &rest keys)
  (let ((definitions ;; ((key command) ...)
          (loop for rest on clauses by 'cddr
                collect (subseq rest 0 2))))
    `(progn
       ,@(mapcar
          (lambda (def)
            `(define-key ,map ,(car def) ,(cadr def)))
          definitions))))

(provide 'kr-def)