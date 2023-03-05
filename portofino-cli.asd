(in-package :asdf)

(defsystem "portofino-cli"
  :version "1.0.0-alpha-1"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("cl-json" "cl-semver" "clingon" "drakma" "split-sequence")
  :components ((:module "src"
                :components ((:file "packages") (:file "portofino") (:file "password-prompt") (:file "cli"))))
  :description "Command-line interface to a Portofino application"
  :in-order-to ((test-op (test-op "portofino-cli/tests"))))

#+sbcl
(defsystem "portofino-cli/executable"
  :version (asdf:component-version (asdf:find-system :portofino-cli))
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli")
  :components ((:module "src" :components ((:file "binary-sbcl"))))
  :build-operation "program-op"
  :build-pathname #-win32 "portofino" #+win32 "portofino.exe"
  :entry-point "portofino-cli:main"
  :description "Command-line interface to a Portofino application")

#-sbcl
(error "Only SBCL is supported for now")

(defsystem "portofino-cli/tests"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :description "Test system for portofino-cli"
  :perform (test-op (op c) (symbol-call :rove :run c)))

