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

(defmain:defcommand (main login) ((host "host to connect to" :short nil :default host)
				  (port "host to connect to :short nil" :short nil :default port)
				  (path "web path to the Portofino API" :short nil :default path)
				  (protocol "protocol (http or https)" :short nil :default protocol)
				  (username "the username")
				  (password "the password"))
  "Login to a running Portofino instance"
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

(defmain:defcommand (main action) ((host "host to connect to" :short nil :default host)
				   (port "host to connect to :short nil" :short nil :default port)
				   (path "web path to the Portofino API" :short nil :default path)
				   (protocol "protocol (http or https)" :short nil :default protocol)
				   (username "the username")
				   (password "the password")
				   &subcommand)
  "Commands for working with resource-actions")

(defun ensure-login-token (host port path protocol username password &key force-login)
  (let* ((file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	 (conf (with-open-file (in file :direction :input :if-does-not-exist nil)
		 (when in
		   (let ((*read-eval* nil))
		     (read in))))))
    (or (and (not force-login) (cdr (assoc :token conf)))
	(if (and username password)
	    (let ((token (cdr (assoc :jwt (portofino:login username password :host host :port port :path path :protocol protocol)))))
	      (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
		(write (acons :token token (remove-if (lambda (x) (eq (car x) :token)) conf)) :stream out))
	      token)
	    (error "Username and password needed for authentication")))))

(defmacro with-valid-login-token ((token-var host port path protocol username password) &body body)
  `(handler-case
       (let ((,token-var (ensure-login-token ,host ,port ,path ,protocol ,username ,password)))
	 ,@body)
     (authentication-required ()
       (let ((,token-var (ensure-login-token ,host ,port ,path ,protocol ,username ,password :force-login t)))
	 ,@body))))

(defmain:defcommand (action list-types) ((host "host to connect to" :short nil :default host)
					 (port "host to connect to :short nil" :short nil :default port)
					 (path "web path to the Portofino API" :short nil :default path)
					 (protocol "protocol (http or https)" :short nil :default protocol)
					 (username "the username")
					 (password "the password"))
  "List resource-action types"
  (with-valid-login-token (token host port path protocol username password)
    (dolist (type (portofino:action-types :host host :port port :path path :protocol protocol :token token))
      (format t "~A (~A)~%~A~@[: ~A~]~%~%" (car type) (cdr (assoc "className" (cdr type) :test #'string=))
	      (cdr (assoc "name" (cdr type) :test #'string=)) (cdr (assoc "description" (cdr type) :test #'string=))))))


(defmain:defcommand (action create) ((host "host to connect to" :short nil :default host)
				     (port "host to connect to :short nil" :short nil :default port)
				     (path "web path to the Portofino API" :short nil :default path)
				     (protocol "protocol (http or https)" :short nil :default protocol)
				     (username "the username")
				     (password "the password")
				     &rest type-and-path)
  "List resource-action types"
  (destructuring-bind (type action-path) type-and-path
    (with-valid-login-token (token host port path protocol username password)
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

(defmain:defcommand (action delete) ((host "host to connect to" :short nil :default host)
				     (port "host to connect to :short nil" :short nil :default port)
				     (path "web path to the Portofino API" :short nil :default path)
				     (protocol "protocol (http or https)" :short nil :default protocol)
				     (username "the username")
				     (password "the password")
				     &rest args)
  "List resource-action types"
  (destructuring-bind (action-path) args
    (with-valid-login-token (token host port path protocol username password)
      (portofino:delete-action action-path :host host :port port :path path :protocol protocol :token token))))
