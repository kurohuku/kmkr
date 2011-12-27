;; kr:parse-body
;; kr:parse-ordinary-lambda-list

(defmacro* kr:time (&body body)
  (let ((sym (gensym)))
    `(let ((,sym (current-time)))
       ,@body
       (time-to-seconds
	(time-subtract (current-time) ,sym)))))

(provide 'kmkr-misc)