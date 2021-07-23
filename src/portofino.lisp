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

(defvar *default-connection-timeout* 10)
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

(defmacro with-http-request ((method url (&optional (text (gensym "TEXT")) (status (gensym "STATUS")))
				  &rest args) &body body)
  `(multiple-value-bind (,text ,status)
	(drakma:http-request ,url :method ,method ,@args :connection-timeout *default-connection-timeout*)
      (if (and (>= ,status 200) (< ,status 300))
	  ,@body
	  (cond
	    ((= ,status 401) (error 'authentication-required :url url))
	    ((= ,status 404) (error 'not-found :url url))
	    (t (error "Request failed: ~S, status: ~S, URL: ~A" ,text ,status ,url))))))

(defun login (username password &key (host *default-portofino-host*) (port *default-portofino-port*) (path *default-portofino-path*) (protocol *default-protocol*))
  (unless (and username password)
    (error "Username and password are required."))
  (let ((drakma:*text-content-types* '(("application" . "json")))
	(url (resource-url host port path ":auth" :protocol protocol)))
    (with-http-request (:post url (text)
			      :parameters `(("username" . ,username)
					    ("password" . ,password)))
      (json:decode-json-from-string text))))

(defun resolve-directory (base &rest path)
  (merge-pathnames (make-pathname :directory (append (pathname-directory base) path)) base))

(defun resolve-file (base &rest path)
  (merge-pathnames (make-pathname :name (car (last path)))
		   (apply #'resolve-directory base (butlast path))))

(define-condition http-error (serious-condition)
  ((url :initarg :url :reader http-error-url)))

(define-condition authentication-required (http-error) ())
(define-condition not-found (http-error) ())

(defun action-types (&key (host *default-portofino-host*) (port *default-portofino-port*) (path *default-portofino-path*) (protocol *default-protocol*) token)
  (let ((drakma:*text-content-types* '(("application" . "json")))
	(url (resource-url host port path "portofino-upstairs/actions/:types" :protocol protocol)))
    (with-http-request (:get url (text) :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))))
      (let ((json:*json-identifier-name-to-lisp* #'identity))
	(json:decode-json-from-string text)))))

(defun create-action (type action-path &key (host *default-portofino-host*) (port *default-portofino-port*) (path *default-portofino-path*) (protocol *default-protocol*) token)
  (let ((url (resource-url host port path (format nil "portofino-upstairs/actions/~A" action-path) :protocol protocol)))
    (with-http-request (:post url (text)
			      :content type
			      :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))))
      text)))

(defun delete-action (action-path &key (host *default-portofino-host*) (port *default-portofino-port*) (path *default-portofino-path*) (protocol *default-protocol*) token)
  (let ((url (resource-url host port path (format nil "portofino-upstairs/actions/~A" action-path) :protocol protocol)))
    (with-http-request (:delete url () :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))))
      t)))

(defun synchronize-database (name &key (host *default-portofino-host*) (port *default-portofino-port*) (path *default-portofino-path*) (protocol *default-protocol*) token)
  (let ((url (resource-url host port path
			   (format nil "portofino-upstairs/database/connections/~A/:synchronize" name)
			  :protocol protocol)))
    (with-http-request (:post url () :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))))
      t)))
