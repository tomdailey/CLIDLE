(defpackage lidle
  (:use :cl :ltk)
  (:export :main))
(in-package :lidle)


(defun main ()
  (setf *debug-tk* nil)
  (with-ltk ()
    (let ((b (make-instance
              'button
              :text "Hello World!"
              :command (lambda ()
                         (format t "Hello World!~%")
                         (setf *exit-mainloop* t)))))
      (pack b))))
