
(defun kr:function-argument (sym)
  (let ((argstr (car (help-split-fundoc (documentation sym t) sym))))
    (if argstr
	(read argstr)
      (cons sym (help-function-arglist sym)))))

(defun kr:aliase-list (sym)
  "ref: help.el `where-is'"
  (let ((result nil)
	(func (indirect-function sym)))
    (mapatoms
     (lambda (s)
       (and (fboundp s)
	    (eq func
		(condition-case ()
		    (indirect-function s)
		  (error s)))
	    (push s result))))
    result))

(provide 'kr-info)
    
