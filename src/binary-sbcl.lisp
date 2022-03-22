(map nil #'cffi:close-foreign-library (cffi:list-foreign-libraries))
(print *features*)
(save-lisp-and-die "portofino" :executable t :toplevel #'portofino-cli:main)
