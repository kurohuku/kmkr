(add-to-list 'load-path
	     (expand-file-name
	      "src"
	      (file-name-directory
	       (car (last current-load-list)))))

;;(add-to-list 'load-path
;;	     (expand-file-name "src" (file-name-directory (buffer-file-name))))

(require 'kmkr-anaphora)
(require 'kmkr-control)
(require 'kmkr-def)
(require 'kmkr-fn)
(require 'kmkr-hashtable)
(require 'kmkr-info)
(require 'kmkr-io)
(require 'kmkr-list)
(require 'kmkr-misc)
(require 'kmkr-overlay)
(require 'kmkr-queue)
(require 'kmkr-simple)
(require 'kmkr-string)
(require 'kmkr-symbol)
(require 'kmkr-buffer)

(provide 'kmkr)
