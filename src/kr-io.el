
(defun kr:insertf (fmt &rest args)
  (insert (apply #'format fmt args)))

(provide 'kr-io)