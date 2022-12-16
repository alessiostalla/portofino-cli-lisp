(in-package :portofino-cli)

(defmacro with-conf ((name &optional (file (gensym))) &body body)
  `(let* ((,file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	  (,name (with-open-file (in ,file :direction :input :if-does-not-exist nil)
		   (when in
		     (let ((*read-eval* nil)) (read in))))))
     ,@body))

(defmethod error-info ((c serious-condition))
  (values (format nil "~A" c) 1))

(defmethod error-info ((c USOCKET:CONNECTION-REFUSED-ERROR))
  (values "Could not connect to the Portofino application." 2))

(defmethod error-info ((error not-found))
  (values (format nil "Not found: ~A" (http-error-url error)) 3))

(defmethod error-info ((error authentication-required))
  (values (format nil "Authentication failed. Please retry with the correct user credentials." (http-error-url error)) 4))

(defmethod error-info ((error not-authorized))
  (values (format nil "The user is not authorized to access ~A." (http-error-url error)) 5))

(defun main ()
  (let ((cli (portofino/command)))
    (handler-case
	(clingon:run cli)
      (serious-condition (e)
	(multiple-value-bind (message code) (error-info e)
	  (format *error-output* "~A~%" message)
	  (clingon:exit code)))
      (log-message-received (m)
	(format (case (log-message-severity m)
		  ((or :error :unknown) *error-output*)
		  (t t))
		"[~A] ~A~%"
		(string-upcase (symbol-name (log-message-severity m)))
		(log-message m))))))

(defun getopt (command opt-key)
  (dolist (cmd (clingon:command-lineage command))
    (multiple-value-bind (value exists-p) (clingon:getopt cmd opt-key)
      (when exists-p
        (return-from getopt (values value exists-p)))))
  (values nil nil))

(defmacro defhandler (name args &body body)
  (let ((command (gensym "COMMAND")) bindings rest)
    (dolist (arg args)
      (if (eq arg '&rest)
	  (setf rest t)
	  (push (if rest
		    `(,arg (clingon:command-arguments ,command))
		    `(,arg (getopt ,command ,(intern (symbol-name arg) :keyword))))
		bindings)))
    `(defun ,name (,command)
       (handler-case
	   (let ,(reverse bindings) ,@body)
	 (error (e)
	   (multiple-value-bind (message code) (error-info e)
	     (format *error-output* "~A~%" message)
	     (clingon:exit code)))))))
     
(defun portofino/options ()
  (list
   (clingon:make-option
    :string
    :description "URL of the Portofino application"
    :short-name #\U
    :long-name "url"
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
    :key :password)))

(defun portofino/pre-hook (cmd)
  (cl+ssl:reload))

(defun directory-command/handler (command)
  (when (clingon:command-arguments command)
    (format *error-output* "Invalid arguments ~A~%~%" (clingon:command-arguments command)))
  (format t "Usage: ~%~%")
  (clingon:print-usage command t))

(defun portofino/command ()
  (clingon:make-command :name "portofino" :description "The Portofino CLI"
			:authors '("Alessio Stalla <alessiostalla@gmail.com>")
			:options (portofino/options)
			:pre-hook #'portofino/pre-hook
			:handler #'directory-command/handler
			:sub-commands (list (new-project/command)
					    (action/command)
					    (db/command)
					    (login/command)
					    (logout/command))))

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
  (login-and-store-token username password url))

(defun login-and-store-token (username password url)
  (unless (and username password (> (length username) 0) (> (length password) 0))
    (error "Username and password are required."))
  (let ((token (cdr (assoc :jwt (portofino:login username password :url url)))))
    (with-conf (conf file)
      (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
	(write (acons :token token
		      (acons :url url
			     (remove-if (lambda (x)
					  (or (eq (car x) :token) (eq (car x) url)))
					conf)))
	       :stream out)
	token))))

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

(defun action/command ()
  (clingon:make-command :name "action" :description "Commands for working with resource-actions"
			:handler #'directory-command/handler
			:options (portofino/options)
			:sub-commands (list
				       (list-action-types/command)
				       (create-action/command)
				       (delete-action/command))))

(defun ensure-login-token (username password &key (url *default-portofino-url*) force-login)
  (let* ((file (resolve-file (user-homedir-pathname) ".portofino-cli"))
	 (conf (with-open-file (in file :direction :input :if-does-not-exist nil)
		 (when in
		   (let ((*read-eval* nil))
		     (read in))))))
    (or (and (not force-login) (cdr (assoc :token conf)))
	(login-and-store-token username password url))))

(defmacro with-safe-http-request ((token-var url username password) &body body)
  (let ((conf (gensym "CONF")))
    `(with-conf (,conf)
       (let ((,url (or ,url (cdr (assoc :url ,conf)) *default-portofino-url*)))
	 (handler-case
	     (let ((,token-var (ensure-login-token ,username ,password :url ,url)))
	       ,@body)
	   (authentication-required ()
	     (let ((,token-var (ensure-login-token ,username ,password :url ,url :force-login t)))
	       ,@body)))))))

(defun list-action-types/command ()
  (clingon:make-command :name "list-types" :description "List resource-action types"
			:handler #'list-action-types/handler
			:options (portofino/options)))

(defhandler list-action-types/handler (username password url)
  "List resource-action types"
  (with-safe-http-request (token url username password)
    (dolist (type (portofino:action-types :url url :token token))
      (format t "~A (~A)~%~A~@[: ~A~]~%~%" (car type) (cdr (assoc "className" (cdr type) :test #'string=))
	      (cdr (assoc "name" (cdr type) :test #'string=)) (cdr (assoc "description" (cdr type) :test #'string=))))))

(defun create-action/command ()
  (clingon:make-command :name "create" :description "Create a new resource-action"
			:handler #'create-action/handler
			:options (portofino/options)))

(defhandler create-action/handler (username password url &rest type-and-path)
  "Create a new resource-action"
  (when (< (length type-and-path) 2)
    (error "Type and path required"))
  (destructuring-bind (type action-path) type-and-path
    (with-safe-http-request (token url username password)
       (let* ((action-types (portofino:action-types :url url :token token))
	      (action-class (cdr (assoc "className"
				       (cdr (or (find (string-downcase type)
						      action-types
						      :key (lambda (x) (string-downcase (car x)))
						      :test #'string=)
						(error "Unknown action type ~S, valid choices are:~{ ~S~}"
						       type
						       (mapcar (lambda (x) (string-downcase (car x))) action-types))))
				       :test #'string=))))
	 (portofino:create-action action-class action-path :url url :token token)))))

(defun delete-action/command ()
  (clingon:make-command :name "delete" :description "Delete a resource-action"
			:handler #'delete-action/handler
			:options (portofino/options)))

(defhandler delete-action/handler (username password url &rest args)
  "Delete a resource-action"
  (let ((action-path (or (car args) (error "Usage: action delete <options> <path>"))))
    (with-safe-http-request (token url username password)
      (portofino:delete-action action-path :url url :token token))))

(defun db/command ()
  (clingon:make-command :name "db" :description "Commands for working with databases"
			:handler #'directory-command/handler
			:options (portofino/options)
			:sub-commands (list
				       (db-add/command)
				       (db-schema/command)
				       (db-remove/command)
				       (db-sync/command)
				       (db-update/command))))

(defun db-sync/command ()
  (clingon:make-command :name "sync" :description "Synchronize a database connection"
			:handler #'db-sync/handler
			:options (portofino/options)))

(defhandler db-sync/handler (username password url &rest args)
  "Synchronize a database connection"
  (let ((db-name (or (car args) (error "Usage: db sync <options> <database-name>"))))
    (with-safe-http-request (token url username password)
      (portofino:synchronize-database db-name :url url :token token))))

(defun db-conf/options ()
  (append (portofino/options)
	  (list
	   (clingon:make-option
	    :string
	    :description "JDBC URL of the database connection"
	    :long-name "jdbc-url"
	    :key :jdbc-url)
	   (clingon:make-option
	    :string
	    :description "Database username to log in"
	    :long-name "jdbc-user"
	    :key :jdbc-user)
	   (clingon:make-option
	    :string
	    :description "Database password to log in"
	    :long-name "jdbc-password"
	    :key :jdbc-password)
	   (clingon:make-option
	    :string
	    :description "JDBC driver"
	    :long-name "jdbc-driver"
	    :key :jdbc-driver)
	   (clingon:make-option
	    :string
	    :description "JNDI resource name (alternative to JDBC parameters)"
	    :long-name "jndi-resource"
	    :key :jndi-resource)
	   (clingon:make-option
	    :string
	    :description "Hibernate dialect. If not specified, it'll be computed from the database connection."
	    :long-name "dialect"
	    :key :dialect))))

(defun db-add/command ()
  (clingon:make-command :name "add" :description "Configure a new database connection"
			:handler #'db-add/handler
			:options (db-conf/options)))

(defhandler db-add/handler
    (username password url jdbc-url jdbc-user jdbc-password jdbc-driver jndi-resource dialect &rest args)
  "Configure a new database connection"
  (let ((db-name (or (car args) (error "Usage: db add <options> <database-name>"))))
    (with-safe-http-request (token url username password)
      (portofino:create-database db-name :url url :token token
				 :driver jdbc-driver :jdbc-url jdbc-url :username jdbc-user :password jdbc-password
				 :dialect dialect :jndi-resource jndi-resource))))

(defun db-remove/command ()
  (clingon:make-command :name "remove" :description "Remove a database connection"
			:handler #'db-remove/handler
			:options (portofino/options)))

(defhandler db-remove/handler (username password url &rest args)
  "Remove a database connection"
  (let ((db-name (or (car args) (error "Usage: db remove <options> <database-name>"))))
    (with-safe-http-request (token url username password)
      (portofino:remove-database db-name :url url :token token))))

(defun db-update/command ()
  (clingon:make-command :name "update" :description "Configure an existing database connection"
			:handler #'db-update/handler
			:options (db-conf/options)))

(defhandler db-update/handler
    (username password url jdbc-url jdbc-user jdbc-password jdbc-driver jndi-resource dialect &rest args)
  "Configure an existing database connection"
  (let ((db-name (or (car args) (error "Usage: db update <options> <database-name>"))))
    (with-safe-http-request (token url username password)
      (portofino:update-database db-name :url url :token token
				 :driver jdbc-driver :jdbc-url jdbc-url :username jdbc-user :password jdbc-password
				 :dialect dialect :jndi-resource jndi-resource))))

(defun db-schema/command ()
  (clingon:make-command :name "schema" :description "Commands for working with schemas"
			:handler #'directory-command/handler
			:options (portofino/options)
			:sub-commands (list
				       (db-schema-add/command))))

(defun db-schema-add/command ()
  (clingon:make-command :name "add" :description "Map a new database schema"
			:handler #'db-schema-add/handler
			:options (portofino/options)))

(defhandler db-schema-add/handler
    (username password url &rest args)
  "Map a new database schema"
  (let ((db-name (or (car args) (error "Usage: db add <options> <database-name> <schema-name> [physical-name]")))
	(schema-name (or (cadr args) (error "Usage: db add <options> <database-name> <schema-name> [physical-name]")))
	(schema-physical-name (caddr args)))
    (with-safe-http-request (token url username password)
      (portofino:add-database-schema db-name schema-name schema-physical-name :url url :token token))))
