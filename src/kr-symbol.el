
(eval-when-compile (require 'cl))

kr:with-gensyms

(defun kr:make-gensym-list (n)
  (loop repeat n collect (gensym)))

kr:do-all-symbols
kr:symb

(provide 'kr-symbol)