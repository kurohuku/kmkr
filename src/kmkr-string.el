;; kr:string-trim-left
;; kr:string-trim-right
;; kr:string-trim
;; kr:whitespace-p
;; kr:whitespace?


(defun kr:mkstr (&rest args)
  (loop
   for a in args
   collect (prin1-to-string a) into x
   finally (return (apply #'concat x))))

(provide 'kmkr-string)