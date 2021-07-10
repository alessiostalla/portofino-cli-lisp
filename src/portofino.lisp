(in-package :portofino)

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

(defparameter *latest-portofino-version* (or (ignore-errors (latest-portofino-version)) "5.2.1"))

(defvar *default-portofino-host* "localhost")
(defvar *default-portofino-port* "8080")
(defvar *default-portofino-path* "/api")
(defvar *default-protocol* "http")

(defun sanitize-path (path)
  (if (> (length path) 0)
      (if (char= (aref path 0) #\/)
	  path
	  (format nil "/~A" path))
      path))

(defun base-url (host port path &key (protocol *default-protocol*))
  (format nil "~A://~A:~A~A" protocol host port (sanitize-path path)))

(defun resource-url (host port base-path path &key (protocol *default-protocol*))
  (format nil "~A/~A" (base-url host port base-path :protocol protocol) path))

(defun login (username password &key (host *default-portofino-host*) (port *default-portofino-port*) (path *default-portofino-path*) (protocol *default-protocol*))
  (let ((drakma:*text-content-types* '(("application" . "json")))
	(url (resource-url host port path ":auth" :protocol protocol)))
    (multiple-value-bind (text status)
	(drakma:http-request url
			     :method :post
			     :parameters `(("username" . ,username)
					   ("password" . ,password)))
      (if (= status 200)
	  (with-input-from-string (s text) (json:decode-json s))
	  (error "Login failed, status: ~S, URL: ~A" status url)))))

(defun resolve-directory (base &rest path)
  (merge-pathnames (make-pathname :directory (append (pathname-directory base) path)) base))

(defun resolve-file (base &rest path)
  (merge-pathnames (make-pathname :name (car (last path)))
		   (apply #'resolve-directory base (butlast path))))

