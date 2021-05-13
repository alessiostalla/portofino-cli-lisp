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

(defmain:defmain (main) ((version "Print the version and exit")
			 &subcommand)
  (print version))

(defmain:defcommand (main new) ((type "type of application: service or webapp" :default "service")
				(package "package of the application")
				(version "version of the application" :default "1.0.0-SNAPSHOT")
				(portofino-version "version of Portofino" :default *latest-portofino-version* :short nil)
				&rest args)
  "Create a new Portofino project"
  (let* ((name (car args))
	 (package (or package name)))
    (unless name (error "Project name is required"))
    (create-application name package :type (find-symbol (string-upcase type) :keyword)
			:version version :portofino-version portofino-version)))



