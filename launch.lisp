;;; launch.lisp: starting programs on the user's behalf.

(in-package #:rhizome)

(defun launch-user-program (command &rest keys &key &allow-other-keys)
  "Start COMMAND as uiop:launch-program does, with the user's home
directory as its working directory; return the process-info. KEYS pass
through to uiop:launch-program. A shell or terminal started this way
opens in the home directory, not wherever the WM was started."
  (apply #'uiop:launch-program command
         :directory (user-homedir-pathname)
         keys))
