(defpackage clidle
  (:use :cl :ltk)
  (:export :main))
(in-package :clidle)

;;; Constants
(defvar +VERSION+ "0.1.0")
(defvar +TOPLEVEL-WIDTH+ 600)
(defvar +TOPLEVEL-HEIGHT+ 400)
(defvar +PROJECT-URL+ "https://github.com/momozor/CLIDLE")

(defun about-window ()
  (with-ltk ()
    (wm-title *tk* "About CLIDLE")
    (set-geometry *tk*
                  380
                  50
                  (/ +TOPLEVEL-WIDTH+ 2)
                  (/ +TOPLEVEL-HEIGHT+ 2))
    (let ((about-text
           (make-instance 'label
                          :text (format nil "Version: ~a~%Project repository: ~a~%"
                                        +VERSION+
                                        +PROJECT-URL+))))
      (pack about-text))))

(defun main ()
  (with-ltk ()
    (wm-title *tk* "Common Lisp IDLE")
    
    (set-geometry *tk*
                  +TOPLEVEL-WIDTH+
                  +TOPLEVEL-HEIGHT+

                  ;; Put the toplevel widget at the
                  ;; center of the viewport/screen
                  (/ +TOPLEVEL-WIDTH+ 2) 
                  (/ +TOPLEVEL-HEIGHT+ 2))
    
    (let* ((menu-bar
            (make-instance 'menubar))
           
           (file-menu
            (make-instance 'menu
                           :master menu-bar
                           :text "File"))

           (new-file-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "New file"
                           :command (lambda ()
                                      nil)))
           
           (open-file-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "Open a file"
                           :command (lambda ()
                                      nil)))

           (quit-file-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "Quit"
                           :command (lambda ()
                                      (setf *exit-mainloop* t))))

           (repl-menu
            (make-instance 'menu
                           :master menu-bar
                           :text "REPL"))

           (restart-repl-menu-button
            (make-instance 'menubutton
                           :master repl-menu
                           :text "Restart REPL"
                           :command (lambda ()
                                      nil)))

           (compile-and-load-menu-button
            (make-instance 'menubutton
                           :master repl-menu
                           :text "Compile and load the file"
                           :command (lambda ()
                                      nil)))

           (about-menu-bar-button
            (make-instance 'menubutton
                           :master menu-bar
                           :text "About"
                           :command (lambda ()
                                      (about-window))))))))
