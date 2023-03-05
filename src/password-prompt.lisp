(in-package :portofino-cli)

;; Credits go to Raymond Wiker for this

#+sbcl
(require :sb-posix)

#-capi
(defun prompt-for-value (prompt)
  (format t "~&~a " prompt)
  (force-output)
  (read-line))

#+capi
(defun prompt-for-value (prompt)
  (capi:prompt-for-string prompt))

#+(and sbcl (not win32))
(defun echo-off ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

#+(and sbcl (not win32))
(defun echo-on ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logior (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

#-capi
(defun prompt-for-passphrase (prompt)
  (format t "~&~a " prompt)
  (force-output)
  #+(and sbcl (not win32)) (echo-off)
  (unwind-protect
       (read-line)
    #+(and sbcl (not win32)) (echo-on)))

#+capi
(defun prompt-for-passphrase (prompt)
  (capi:prompt-for-string prompt :pane-class 'capi:password-pane))

