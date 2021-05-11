(in-package :portofino-cli)

(defun latest-portofino-version ()
   (let* ((drakma:*text-content-types* '(("application" . "json")))
	  (resp (drakma:http-request "http://search.maven.org/solrsearch/select"
				     :parameters '(("q" . "g:com.manydesigns AND a:portofino")
						   ("start" . "0")
						   ("rows" . "1"))))
	  (json (with-input-from-string (s resp) (json:decode-json s))))
     (cdr (find-if (lambda (x) (and (listp x) (eq (car x) :latest-version)))
		   (cadr
		    (find-if (lambda (x) (and (listp x) (eq (car x) :docs)))
			     (find-if (lambda (x) (eq (car x) :response)) json)))))))

(defparameter *latest-portofino-version* (latest-portofino-version))
