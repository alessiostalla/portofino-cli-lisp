(asdf:defsystem "portofino-cli/deb"
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
