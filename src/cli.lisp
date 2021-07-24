(in-package :portofino-cli)

(defvar *maven-command* "mvn")

(defun check-maven-installation ()
  (uiop:run-program `(,*maven-command* "-version")))

(defun create-application (name package &key (type :service) (version "1.0.0-SNAPSHOT") (portofino-version *latest-portofino-version*))
  (check-maven-installation)
  (uiop:run-program `(,*maven-command* "archetype:generate"
				       "-DinteractiveMode=false"
				       "-DarchetypeGroupId=com.manydesigns"
				       ,(format nil "-DarchetypeArtifactId=~A" (ecase type
										 (:service "portofino-service-archetype")
										 (:webapp  "portofino-war-archetype")))
				       ,(format nil "-DarchetypeVersion=~A" portofino-version)
				       ,(format nil "-DgroupId=~A" package)
				       ,(format nil "-DartifactId=~A" name)
				       ,(format nil "-Dversion=~A" version)
				       ,(format nil "-Dpackage=~A" package))
		    :output t
		    :error-output t))

(defmain:defmain (main) ((host "host to connect to" :short nil :default *default-portofino-host*)
			 (port "host to connect to" :short nil :default *default-portofino-port*)
			 (path "web path to the Portofino API" :short nil :default *default-portofino-path*)
			 (protocol "protocol (http or https)" :short nil :default *default-protocol*)
			 (username "username to log in" :default "")
			 (password "password to log in" :short nil :default "")
			 &subcommand))

(defmain:defcommand (main new) ((type "type of application: service or webapp" :default "service")
				(package "package of the application")
				(version "version of the application" :default "1.0.0-SNAPSHOT")
				(portofino-version "version of Portofino" :default *latest-portofino-version* :short nil)
				&rest args)
  "Create a new Portofino project"
  (let* ((name (car args))
	 (package (or package name)))
    (unless name
      (print "Project name is required")
      (uiop:quit))
    (create-application name package :type (find-symbol (string-upcase type) :keyword)
			:version version :portofino-version portofino-version)))

#+todo-arguments-after-subcommand
(defmacro defcommand (name (&rest args) &body body)
  (let ((parent-name (if (listp name) (cadr name) name)))
    `(progn
       (defmain:defcommand ,name ,args ,@body)
       (defmain:defcommand (,parent-name exec-gensyn) ,args ,@body)
       ,parent-name)))

(defmacro define-subcommand-with-login ((parent name) (&rest args) &body body)
  `(defmain:defcommand (,parent ,name) ((host "host to connect to" :short nil :default host)
					(port "host to connect to :short nil" :short nil :default port)
					(path "web path to the Portofino API" :short nil :default path)
					(protocol "protocol (http or https)" :short nil :default protocol)
					(username "username to log in" :default username)
					(password "password to log in" :short nil :default password)
					,@args)
     ,@body))

(define-subcommand-with-login (main login) ()
  "Login to a running Portofino instance"
  (unless (and username password (> (length username) 0) (> (length password) 0))
    (error "--username and --password are required."))
  (let* ((token (cdr (assoc :jwt (portofino:login username password :host host :port port :path path :protocol protocol))))
	 (file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	 (conf (with-open-file (in file :direction :input :if-does-not-exist nil)
		 (when in
		   (let ((*read-eval* nil)) (read in))))))
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write (acons :token token (remove-if (lambda (x) (eq (car x) :token)) conf)) :stream out))))

(defmain:defcommand (main logout) ()
  "Log out deleting the stored token"
  (let* ((file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	 (conf (with-open-file (in file :direction :input :if-does-not-exist nil)
		 (when in
		   (let ((*read-eval* nil)) (read in))))))
    (when conf
      (with-open-file (out file :direction :output :if-exists :supersede)
	(write (remove-if (lambda (x) (eq (car x) :token)) conf) :stream out)))))

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
       (uiop:quit))
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
  (let ((db-name (or (car database-name) (error "Usage: db sync <options> <database-name>"))))
    (with-safe-http-request (token host port path protocol username password)
      (portofino:create-database db-name
				 :host host :port port :path path :protocol protocol :token token
				 :driver driver :url url :username jdbc-user :password jdbc-pass :dialect dialect
				 :jndi-resource jndi-resource))))
