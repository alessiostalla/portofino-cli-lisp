(in-package :asdf)

(defsystem "portofino-cli"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("cl-json" "defmain" "drakma")
  :components ((:module "src"
                :components ((:file "packages") (:file "portofino") (:file "cli"))))
  :description "Command-line interface to a Portofino application"
  :in-order-to ((test-op (test-op "portofino-cli/tests"))))

#+sbcl
(defsystem "portofino-cli/executable"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli")
  :components ((:module "src" :components (#+sbcl (:file "binary-sbcl"))))
  :description "Command-line interface to a Portofino application")

#+sbcl
(defsystem "portofino-cli/deb"
  :defsystem-depends-on (:linux-packaging)
  :class "linux-packaging:deb"
  :build-operation "linux-packaging:build-op"
  :package-name "portofino-cli"
  :build-pathname "portofino-cli"
  :entry-point "portofino-cli:main"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli")
  :description "Portofino CLI Debian package")

(defsystem "portofino-cli/osx-app"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "portofino"
  :entry-point "portofino-cli:main"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli")
  :description "Portofino CLI OSX app")

(defsystem "portofino-cli/tests"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :description "Test system for portofino-cli"
  :perform (test-op (op c) (symbol-call :rove :run c)))

