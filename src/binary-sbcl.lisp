(when cl+ssl::*ssl-global-context*
  (cl+ssl:ssl-ctx-free cl+ssl::*ssl-global-context*)
  (setf cl+ssl::*ssl-global-context* nil))
(map nil #'cffi:close-foreign-library (cffi:list-foreign-libraries))
(save-lisp-and-die "portofino" :executable t :toplevel #'portofino-cli:main)
