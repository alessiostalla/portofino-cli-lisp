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

(defvar *maven-command* "mvn")

(defparameter *latest-portofino-version* (or (ignore-errors (latest-portofino-version)) "5.3.4"))

(defvar *default-connection-timeout* 10)
(defvar *default-portofino-url* "http://localhost:8080")

(defvar *portofino-version-6* (semver:read-version-from-string "6.0.0"))

(defun archetype-name (kw version)
  (let ((version (semver:read-version-from-string version)))
    (cond
      ((eq kw :service) "portofino-service-full-archetype")
      ((eq kw :minimal) "portofino-service-minimal-archetype")
      ((and (version< version *portofino-version-6*) (eq kw :webapp)) "portofino-war-archetype")
      (t (error "Not a known application type: ~A" type)))))

(defun check-maven-installation ()
  (uiop:run-program `(,*maven-command* "-version")))

(defun create-application (name package &key (type :service) (version "1.0.0-SNAPSHOT") (portofino-version *latest-portofino-version*))
  (check-maven-installation)
  (uiop:run-program `(,*maven-command* "archetype:generate"
				       "-DinteractiveMode=false"
				       "-DarchetypeGroupId=com.manydesigns"
				       ,(format nil "-DarchetypeArtifactId=~A" (archetype-name type portofino-version))
				       ,(format nil "-DarchetypeVersion=~A" portofino-version)
				       ,(format nil "-DgroupId=~A" package)
				       ,(format nil "-DartifactId=~A" name)
				       ,(format nil "-Dversion=~A" version)
				       ,(format nil "-Dpackage=~A" package))
		    :output t
		    :error-output t))

(defun sanitize-base-url (url)
  (loop :while (and (> (length url) 0) (char= (aref url (1- (length url))) #\/))
     :do (setf url (subseq url 0 (1- (length url)))))
  url)

(defun resource-url (base-url path)
  (format nil "~A/~A" (sanitize-base-url base-url) path))

(defun maybe-decode (content)
  (typecase content
    (string content)
    (vector (flexi-streams:octets-to-string content))
    (t content)))

(defun decode-severity (sev-str)
  (cond
    ((equal sev-str "info") :info)
    ((equal sev-str "warning") :warning)
    ((equal sev-str "error") :error)
    (t :unknown)))

(defmacro with-http-request ((method url
				     (&optional (text (gensym "TEXT"))
						(status (gensym "STATUS"))
						(headers (gensym "HEADERS")))
				     &rest args)
			     &body body)
  (let ((portofino-messages (gensym "MESSAGES")))
    `(multiple-value-bind (,text ,status ,headers)
	 (drakma:http-request ,url :method ,method ,@args :connection-timeout *default-connection-timeout*)
       (let ((,portofino-messages (drakma:header-value :x-portofino-message ,headers)))
	 (when ,headers
	   (let ((,portofino-messages (split-sequence #\, ,portofino-messages)))
	     (dolist (msg ,portofino-messages)
	       (let ((pos (position #\: msg)))
		 (when pos
		   (signal 'log-message-received
			   :severity (decode-severity (subseq msg 0 pos))
			   :message (string-trim " " (subseq msg (1+ pos)))))))))
	 (if (and (>= ,status 200) (< ,status 300))
	     (progn ,@body)
	     (cond
	       ((= ,status 401) (error 'authentication-required :url url))
	       ((= ,status 403) (error 'not-authorized :url url))
	       ((= ,status 404) (error 'not-found :url url))
	       (t (error "Request failed: ~S, ~A ~A, message: ~S" ,status ,method ,url (maybe-decode ,text)))))))))

(defun login (username password &key (url *default-portofino-url*))
  (unless (and username password)
    (error "Username and password are required."))
  (let ((drakma:*text-content-types* '(("application" . "json")))
	(url (resource-url url ":auth")))
    (with-http-request (:post url (text)
			      :parameters `(("username" . ,username)
					    ("password" . ,password)))
      (json:decode-json-from-string text))))

(defun resolve-directory (base &rest path)
  (merge-pathnames (make-pathname :directory (append (pathname-directory base) path)) base))

(defun resolve-file (base &rest path)
  (merge-pathnames (make-pathname :name (car (last path)))
		   (apply #'resolve-directory base (butlast path))))

(define-condition http-error (error)
  ((url :initarg :url :reader http-error-url)))

(define-condition authentication-required (http-error) ())
(define-condition not-authorized (http-error) ())
(define-condition not-found (http-error) ())

(define-condition log-message-received (condition)
  ((severity :initarg :severity :reader log-message-severity)
   (message :initarg :message :reader log-message)))

(defun authorization-header (token)
  `("Authorization" . ,(format nil "Bearer ~A" token)))

(defun action-types (&key (url *default-portofino-url*) token)
  (let ((drakma:*text-content-types* '(("application" . "json")))
	(url (resource-url url "portofino-upstairs/actions/:types")))
    (with-http-request (:get url (text) :additional-headers (list (authorization-header token)))
      (let ((json:*json-identifier-name-to-lisp* #'identity))
	(json:decode-json-from-string text)))))

(defun create-action (type action-path &key (url *default-portofino-url*) token)
  (let ((url (resource-url url (format nil "portofino-upstairs/actions/~A" action-path))))
    (with-http-request (:post url (text)
			      :content type
			      :additional-headers (list (authorization-header token)))
      text)))

(defun delete-action (action-path &key (url *default-portofino-url*) token)
  (let ((url (resource-url url (format nil "portofino-upstairs/actions/~A" action-path))))
    (with-http-request (:delete url () :additional-headers (list (authorization-header token)))
      t)))

(defun synchronize-database (name &key (url *default-portofino-url*) token)
  (let ((url (resource-url url (format nil "portofino-upstairs/database/connections/~A/:synchronize" name))))
    (with-http-request (:post url () :additional-headers (list (authorization-header token)))
      t)))

(defun db-descriptor (name driver url username password dialect jndi-resource)
  (cl-json:encode-json-to-string
   (remove-if #'null (list
		      (cons 'database-name name)
		      (cons 'driver driver)
		      (cons 'url url)
		      (cons 'username username)
		      (cons 'password password)
		      (cons 'hibernate-dialect dialect)
		      (cons 'jndi-resource jndi-resource))
	      :key #'cdr)))

(defun create-database (name &key
			       (url *default-portofino-url*) token
			       driver jdbc-url username password dialect jndi-resource
			       schemas)
  (unless (and (or jdbc-url jndi-resource) (or (null jdbc-url) (null jndi-resource)))
    (error "Exactly one of jdbc-url and jndi-resource is required"))
  (let ((url (resource-url url "portofino-upstairs/database/connections")))
    (with-http-request (:post url ()
			      :content-type "application/json"
			      :content (db-descriptor name driver jdbc-url username password dialect jndi-resource)
			      :additional-headers (list (authorization-header token)))
      t)))

(defun remove-database (name &key (url *default-portofino-url*) token)
  (let ((url (resource-url url (format nil "portofino-upstairs/database/connections/~A" name))))
    (with-http-request (:delete url () :additional-headers (list (authorization-header token)))
      t)))

(defun update-database (name &key
			       (url *default-portofino-url*) token
			       driver jdbc-url username password dialect jndi-resource
			       schemas)
  (if (and jdbc-url jndi-resource)
    (error "Please provide either a jdbc-url or a jndi-resource, not both"))
  (let ((url (resource-url url (format nil "portofino-upstairs/database/connections/~A" name))))
    (with-http-request (:put url ()
			     :content-type "application/json"
			     :content (db-descriptor name driver jdbc-url username password dialect jndi-resource)
			     :additional-headers (list (authorization-header token)))
      t)))

(defun add-database-schema (db-name schema-name schema-physical-name &key (url *default-portofino-url*) token)
  (let ((url (resource-url url (format nil "portofino-upstairs/database/connections/~A/~A" db-name schema-name))))
    (with-http-request (:post url ()
			      :content schema-physical-name
			      :additional-headers (list (authorization-header token)))
      t)))
