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
