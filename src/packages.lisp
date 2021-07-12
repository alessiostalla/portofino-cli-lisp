(defpackage :portofino
  (:use :cl)
  (:export
   #:action-types #:authentication-required #:create-action
   #:*default-portofino-host* #:*default-portofino-port* #:*default-portofino-path* #:*default-protocol*
   #:*latest-portofino-version* #:login #:resolve-file #:resolve-directory))

(defpackage :portofino-cli
  (:use :cl :portofino)
  (:shadow #:login))

