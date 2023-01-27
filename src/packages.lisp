(defpackage :portofino
  (:use :cl :split-sequence)
  (:export
   #:action-types #:add-database-schema #:authentication-required
   #:copy-or-move-action #:create-action #:create-application #:create-database
   #:*default-connection-timeout*
   #:*default-portofino-url*
   #:delete-action
   #:http-error-url
   #:*latest-portofino-version* #:login #:log-message #:log-message-received #:log-message-severity
   #:not-authorized #:not-found
   #:remove-database #:resolve-file #:resolve-directory
   #:synchronize-database
   #:update-database))

(defpackage :portofino-cli
  (:use :cl :portofino)
  (:shadow #:login #:delete)
  (:export #:main))

(defpackage :portofino-cli-actions
  (:export #:create #:delete))

(defpackage :portofino-cli-dbs
  (:export #:create #:delete))
