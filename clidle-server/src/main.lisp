(defpackage clidle-server
  (:use :cl)
  (:export :main))
(in-package :clidle-server)

(defun swank-thread ()
  "Returns a thread that's acting as a Swank server."
  (dolist (thread (bt:all-threads))
    (when (com.google.base:prefixp "Swank" (bt:thread-name thread))
      (return thread))))

(defun wait-for-swank-thread ()
  "Wait for the Swank server thread to exit."
  (let ((swank-thread (swank-thread)))
    (when swank-thread
      (bt:join-thread swank-thread))))

(defun main ()
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t) 
  (swank:create-server :port 7891 :dont-close t)
  (wait-for-swank-thread))
