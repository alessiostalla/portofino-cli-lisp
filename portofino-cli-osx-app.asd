(asdf:defsystem "portofino-cli-osx-app"
  :defsystem-depends-on (:deploy)
  :build-operation "osx-app-deploy-op"
  :build-pathname "portofino"
  :entry-point "portofino-cli:main"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("portofino-cli")
  :description "Portofino CLI OSX app")

(deploy:define-hook (:build unload-libssl) ()
    (map nil #'cffi:close-foreign-library (cffi:list-foreign-libraries))
    (print *features*))

(deploy:define-hook (:bood reload-cl-ssl) ()
  (cl+ssl:reload)
  (print *features*))
