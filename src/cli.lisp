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
#|
(defun main (&optional (args (net.didierverna.clon:cmdline)))
  (parse
   (create-main-parser (main "Portofino command-line interface")
     (add-flag main :long "portofino-version" :short "V" :help "The version of Portofino to use" :var "portofino-version")
     (add-subparser main
		    (create-sub-parser (sub "this is a sub action parser") (add-optional sub :short "t" :long "test" :var "test!") (add-optional sub :short "t2" :long "test2" :var "test2")
                                        (add-positional sub :name "action" :help "this is an example for a positional"))))
         (list "-g" "-t" "alle" "positional" "sub" "--test" "foo" "theaction")))|#

#|(defclass argument ()
  ((name :accessor argument-name :initarg :name :initform (error "Argument name is required"))
   (help :accessor argument-help :initarg :help)))

(defclass optional-argument (argument)
  ((default-value :accessor argument-default-value :initarg :default)
   (allows-multiple-copies :accessor argument-allows-multiple-copies :initform nil :initarg :allows-multiple-copies)
   (aliases :accessor argument-aliases :initarg :aliases :initform ())))

(defclass command-line-flag (optional-argument))

(defclass positional-argument (argument) ())

(defclass rest-arguments (argument) ())

(defclass subcommand (positional-argument)
  ((function :accessor subcommand-function :initarg :function
	     :initform #'(lambda (&rest _) (declare (ignore _)) (error "Subcommand not implemented")))
   (subcommands :accessor subcommands :initarg :subcommands :initform ())))

(defmethod compute-partial-cli-parser-function ((argument optional-argument) other-arguments)
  (lambda (cont args)
    ( args))

(defun derive-command-line-parser-function (arguments)
  (if arguments
      (compute-partial-cli-parser-function (car arguments) (cdr arguments))
      (lambda (args)
	(when args (error "Unsupported arguments: ~A" args)))))

(defmacro define-cli (name (&rest args) &body body)
  (let ((cmdline (gensym "cmdline")))
    `(defun ,name (&optional (,cmdline (net.didierverna.clon:cmdline)))
       ,@body)))|#

#|
(defcommand main ((optional portofino-version :alias "V" :default *latest-portofino-version* :help "The version of Portofino to use")
		  (flag verbose :help "Print extra information")
		  (subcommand :help "Command to execute"))
  (if verbose ...)
  (cond
    ((equal subcommand "new") (new-project :portofino-version portofino-version :verbose verbose))
    (t (error "Unknown command: ~A" subcommand))))
|#
(defmain:defmain (main) ((test "test" :short nil) &subcommand)
  (print "Portofino CLI. Please provide a subcommand."))

(defmain:defcommand (main new) ((test "test" :short nil :default test)
				(type "type of application: service or webapp" :default "service")
				(package "package of the application")
				(version "version of the application" :default "1.0.0-SNAPSHOT")
				(portofino-version "version of Portofino" :default *latest-portofino-version* :short nil)
				&rest args)
  "Create a new Portofino project"
  (let* ((name (car args))
	 (package (or package name)))
    (print test)
    (unless name
      (print "Project name is required")
      (uiop:quit))
    (create-application name package :type (find-symbol (string-upcase type) :keyword)
			:version version :portofino-version portofino-version)))
