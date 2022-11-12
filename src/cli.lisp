(in-package :portofino-cli)

(defmacro with-conf ((name &optional (file (gensym))) &body body)
  `(let* ((,file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	  (,name (with-open-file (in ,file :direction :input :if-does-not-exist nil)
		   (when in
		     (let ((*read-eval* nil)) (read in))))))
     ,@body))

(defmethod error-info ((c error))
  (values (format nil "Generic error: ~A" c) 1))

(defmethod error-info ((c USOCKET:CONNECTION-REFUSED-ERROR))
  (values "Could not connect to the Portofino application." 2))

(defun main ()
  (let ((cli (portofino/command)))
    (handler-case
	(clingon:run cli)
      (error (e)
	(multiple-value-bind (message code) (error-info e)
	  (format *error-output* "~A~%" message)
	  (clingon:exit code))))))

(defmacro defhandler (name args &body body)
  (let ((command (gensym "COMMAND")))
    `(defun ,name (,command)
       (handler-case
	   (let (,@(mapcar (lambda (arg)
			     `(,arg (clingon:getopt ,command ,(intern (symbol-name arg) :keyword))))
			   args))
	     ,@body)
	 (error (e)
	   (multiple-value-bind (message code) (error-info e)
	     (format *error-output* message)
	     (clingon:exit code)))))))
     

(defun portofino/options ()
  (with-conf (conf)
    (let ((*default-portofino-url* (or (cdr (assoc :url conf)) *default-portofino-url*)))
      (list
       (clingon:make-option
	:string
	:description "URL of the Portofino application"
	:short-name #\U
	:long-name "url"
	:initial-value *default-portofino-url*
	:env-vars '("PORTOFINO_URL")
	:key :url)
       (clingon:make-option
	:string
	:description "username to log in"
	:short-name #\u
	:long-name "username"
	:key :username)
       (clingon:make-option
	:string
	:description "password to log in"
	:short-name #\p
	:long-name "password"
	:key :password)))))

(defun portofino/pre-hook (cmd)
  (cl+ssl:reload))

(defun portofino/handler (command)
  (when (clingon:command-arguments command)
    (format *error-output* "Invalid arguments ~A~%~%" (clingon:command-arguments command)))
  (format t "Usage: ~%~%")
  (clingon:print-usage command t))

(defun portofino/command ()
  (clingon:make-command :name "portofino" :description "The Portofino CLI"
			:authors '("Alessio Stalla <alessiostalla@gmail.com>")
			:options (portofino/options)
			:pre-hook #'portofino/pre-hook
			:handler #'portofino/handler
			:sub-commands (list (new-project/command) (login/command) (logout/command))))

(defun new-project/options ()
  (list
   (clingon:make-option
    :string
    :description "type of application: service or webapp"
    :long-name "type"
    :initial-value "service"
    :key :project-type)
   (clingon:make-option
    :string
    :description "package of the application"
    :long-name "package"
    :key :project-package)
   (clingon:make-option
    :string
    :description "version of the application"
    :long-name "project-version"
    :initial-value "1.0.0-SNAPSHOT"
    :key :project-version)
   (clingon:make-option
    :string
    :description "version of Portofino"
    :short-name #\v
    :long-name "portofino-version"
    :initial-value *latest-portofino-version*
    :env-vars '("PORTOFINO_VERSION")
    :key :portofino-version)))

(defun new-project/command ()
  (clingon:make-command :name "new" :description "Create a new Portofino project"
			:options (new-project/options)
			:handler #'new-project/handler))

(defun new-project/handler (cmd)
  "Create a new Portofino project"
  (let ((type (clingon:getopt cmd :project-type))
	(package (clingon:getopt cmd :project-package))
	(version (clingon:getopt cmd :project-version))
	(portofino-version (clingon:getopt cmd :portofino-version))
	(args (clingon:command-arguments cmd)))
    (let* ((name (car args))
	   (package (or package name)))
      (unless name
	(error "Project name is required"))
      (portofino:create-application name package :type (or (find-symbol (string-upcase type) :keyword)
							   (error "Invalid project type ~A" type))
				    :version version :portofino-version portofino-version))))

(defun login/command ()
  (clingon:make-command :name "login" :description "Login to a running Portofino instance"
			:handler #'login/handler
			:options (portofino/options)))

(defhandler login/handler (username password url)
  "Login to a running Portofino instance"
  (unless (and username password (> (length username) 0) (> (length password) 0))
    (error "Username and password are required."))
  (let ((token (cdr (assoc :jwt (portofino:login username password :portofino-url url)))))
    (with-conf (conf file)
      (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
	(write (acons :token token
		      (acons :url url
			     (remove-if (lambda (x)
					  (or (eq (car x) :token) (eq (car x) url)))
					conf)))
	       :stream out)))))

(defun logout/command ()
  (clingon:make-command :name "logout" :description "Log out of the application, i.e. delete the stored authentication token"
			:handler #'logout/handler))

(defhandler logout/handler ()
  "Log out deleting the stored token"
  (with-conf (conf file)
    (when conf
      (with-open-file (out file :direction :output :if-exists :supersede)
	(write (remove-if (lambda (x)
			    (or (eq (car x) :token) (eq (car x) :url)))
			  conf)
	       :stream out)))))

#|
(define-subcommand-with-login (main action) (&subcommand)
  "Commands for working with resource-actions")

(defun ensure-login-token (host port path protocol username password &key force-login)
  (let* ((file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	 (conf (with-open-file (in file :direction :input :if-does-not-exist nil)
		 (when in
		   (let ((*read-eval* nil))
		     (read in))))))
    (or (and (not force-login) (cdr (assoc :token conf)))
	(if (and username password (> (length username) 0) (> (length password) 0))
	    (let ((token (cdr (assoc :jwt (portofino:login username password :host host :port port :path path :protocol protocol)))))
	      (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
		(write (acons :token token (remove-if (lambda (x) (eq (car x) :token)) conf)) :stream out))
	      token)
	    (error "Username and password needed for authentication")))))

(defmacro with-safe-http-request ((token-var host port path protocol username password) &body body)
  `(handler-case
       (let ((,token-var (ensure-login-token ,host ,port ,path ,protocol ,username ,password)))
	 ,@body)
     (not-found (error)
       (format t "Not found: ~A~%" (http-error-url error))
       (uiop:quit 2))
     (authentication-required ()
       (let ((,token-var (ensure-login-token ,host ,port ,path ,protocol ,username ,password :force-login t)))
	 ,@body))))

(define-subcommand-with-login (action list-types) ()
  "List resource-action types"
  (with-safe-http-request (token host port path protocol username password)
    (dolist (type (portofino:action-types :host host :port port :path path :protocol protocol :token token))
      (format t "~A (~A)~%~A~@[: ~A~]~%~%" (car type) (cdr (assoc "className" (cdr type) :test #'string=))
	      (cdr (assoc "name" (cdr type) :test #'string=)) (cdr (assoc "description" (cdr type) :test #'string=))))))


(define-subcommand-with-login (action portofino-cli-actions:create) (&rest type-and-path)
  "Create a new resource-action"
  (destructuring-bind (type action-path) type-and-path
    (with-safe-http-request (token host port path protocol username password)
      (let* ((action-types (portofino:action-types :host host :port port :path path :protocol protocol :token token))
	     (action-class (cdr (assoc "className"
				       (cdr (or (find (string-downcase type)
						      action-types
						      :key (lambda (x) (string-downcase (car x)))
						      :test #'string=)
						(error "Unknown action type ~S, valid choices are:~{ ~S~}"
						       type
						       (mapcar (lambda (x) (string-downcase (car x))) action-types))))
				       :test #'string=))))
	(portofino:create-action action-class action-path :host host :port port :path path :protocol protocol :token token)))))

(define-subcommand-with-login (action portofino-cli-actions:delete) (&rest args)
  "Delete an action"
  (let ((action-path (or (car args) (error "Usage: action delete <options> <path>"))))
    (with-safe-http-request (token host port path protocol username password)
      (portofino:delete-action action-path :host host :port port :path path :protocol protocol :token token))))

(define-subcommand-with-login (main db) (&subcommand)
  "Commands for working with databases")

(define-subcommand-with-login (db sync) (&rest database-name)
  "Synchronize a database connection"
  (let ((db-name (or (car database-name) (error "Usage: db sync <options> <database-name>"))))
    (with-safe-http-request (token host port path protocol username password)
      (portofino:synchronize-database db-name :host host :port port :path path :protocol protocol :token token))))

(define-subcommand-with-login (db portofino-cli-dbs:create)
    ((driver "JDBC driver" :short nil)
     (url "JDBC connection URL" :short nil)
     (jdbc-user "JDBC username" :short nil)
     (jdbc-pass "JDBC password" :short nil)
     (jndi-resource "JNDI resource name (alternative to JDBC parameters)" :short nil)
     (dialect "Hibernate dialect. If not specified, it'll be computed from the database connection." :short nil)
     &rest database-name)
  "Create a new database connection"
  (let ((db-name (or (car database-name) (error "Usage: db create <options> <database-name>"))))
    (with-safe-http-request (token host port path protocol username password)
      (portofino:create-database db-name
				 :host host :port port :path path :protocol protocol :token token
				 :driver driver :url url :username jdbc-user :password jdbc-pass :dialect dialect
				 :jndi-resource jndi-resource))))
|#
