(defpackage lidle
  (:use :cl :ltk)
  (:export :main))
(in-package :lidle)

;;; Constants
(defvar +TOPLEVEL-WIDTH+ 600)
(defvar +TOPLEVEL-HEIGHT+ 400)

(defun main ()
  (with-ltk ()
    (wm-title *tk* "Common Lisp IDLE")
    (set-geometry *tk* 600 400 0 0)
    (let ((quit-button
           (make-instance 'button
                          :text "Quit"
                          :command (lambda ()
                                     (setf *exit-mainloop* t)))))
      (pack quit-button))))
