(defpackage clidle
  (:use :cl :ltk)
  (:import-from :cl-project
                :make-project)
  (:import-from :swank
                :create-server)
  (:import-from :com.google.base
                :prefixp)
  (:import-from :bt
                :all-threads
                :thread-name)
  (:export :main))
(in-package :clidle)

;;; Constants
(defparameter +VERSION+ "0.1.0")
(defparameter +TOPLEVEL-WIDTH+ 600)
(defparameter +TOPLEVEL-HEIGHT+ 400)
(defparameter +PROJECT-URL+ "https://github.com/momozor/CLIDLE")
(defparameter +SWANK-SERVER-PORT+ 7891)

;;; Swank manager
(defun swank-server-thread ()
  (dolist (thread (all-threads))
    (when (prefixp "Swank" (thread-name thread))
      (return thread))))

(defun wait-for-server-thread-exit ()
  (let ((swank-thread (swank-server-thread)))
    (when swank-thread

      ;; TODO: Make use of the portable
      ;; bt thread function instead of
      ;; SBCL's native thread
      (sb-thread:join-thread swank-thread))))

(defun swank-server-launcher ()
  (create-server :port +SWANK-SERVER-PORT+ :dont-close t)
  (wait-for-server-thread-exit))

;;; GUI
(defun new-project-popup ()
  (with-ltk ()
    (wm-title *tk* "Create a new project")
    (set-geometry *tk*
                  150
                  100
                  (/ +TOPLEVEL-WIDTH+ 2)
                  (/ +TOPLEVEL-HEIGHT+ 2))
    (let* ((entry-label
           (make-instance 'label :text "Path to new project"))
          (entry
           (make-instance 'entry :width 15))
          (create-project-button
           (make-instance 'button
                          :text "Create project"
                          :command (lambda ()
                                     (make-project
                                      (pathname (text entry)))
                                     (setf *exit-mainloop* t)))))
      (pack entry-label)
      (pack entry)
      (pack create-project-button)

      (bind entry "<Return>"
            (lambda (event)
              (declare (ignore event))
              (make-project (pathname (text entry)))
              (setf *exit-mainloop* t))))))

(defun about-window-popup ()
  (with-ltk ()
    (wm-title *tk* "About CLIDLE")
    (set-geometry *tk*
                  380
                  50
                  (/ +TOPLEVEL-WIDTH+ 2)
                  (/ +TOPLEVEL-HEIGHT+ 2))
    (let ((about-text
           (make-instance 'label
                          :text (format nil
                                        "Version: ~a~%Project repository: ~a~%"
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
                  ;; FIXME: Need to know the actual
                  ;; host screen resolution/size
                  (/ +TOPLEVEL-WIDTH+ 2) 
                  (/ +TOPLEVEL-HEIGHT+ 2))
    
    (let* ((menu-bar
            (make-instance 'menubar))
           
           (file-menu
            (make-instance 'menu
                           :master menu-bar
                           :text "File"))

           (new-project-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "New project"
                           :command (lambda ()
                                      (new-project-popup))))
           
           (open-project-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "Open a project"
                           :command (lambda ()
                                      nil)))

           (open-file-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "Open a file"
                           :command (lambda ()
                                      nil)))

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

           (reload-the-project-menu-button
            (make-instance 'menubutton
                           :master repl-menu
                           :text "Reload the current project"
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
                                      (about-window-popup))))
           
           (quit-file-menu-button
            (make-instance 'menubutton
                           :master file-menu
                           :text "Quit"
                           :command (lambda ()
                                      (setf *exit-mainloop* t))))
           
           (text-editor
            (make-instance 'text
                           :wrap :word))

           (repl-terminal
            (make-instance 'text
                           :wrap :word)))
      
      (pack text-editor)
      (pack repl-terminal))))
