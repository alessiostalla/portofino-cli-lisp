(defpackage :portofino
  (:use :cl)
  (:export
   #:action-types #:authentication-required #:create-action
   #:*default-connection-timeout*
   #:*default-portofino-host* #:*default-portofino-port* #:*default-portofino-path* #:*default-protocol*
   #:delete-action #:http-error-url
   #:*latest-portofino-version* #:login
   #:not-found
   #:resolve-file #:resolve-directory #:synchronize-database))

(defpackage :portofino-cli
  (:use :cl :portofino)
  (:shadow #:login #:delete))

