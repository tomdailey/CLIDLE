(defpackage clidle
  (:use :cl :ltk)
  (:import-from :cl-project
                :make-project)
  (:import-from :swank
                :create-server
                :*globally-redirect-io*
                :*enable-event-history*
                :*log-events*
                :*loopback-interface*)
  (:import-from :swank-client
                :with-slime-connection
                :slime-eval)
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
(defparameter +DEFAULT-POPUP-WIDTH+ 150)
(defparameter +DEFAULT-POPUP-HEIGHT+ 100)
(defparameter +ENTER-KEY-CODE+ "<Return>")
(defparameter +PROJECT-URL+ "https://github.com/momozor/CLIDLE")

(defparameter *current-workspace* "")

;;; GUI

(declaim (ftype (function (ltk:entry) pathname)
                set-current-workspace-path))
(defun set-current-workspace-path (path-entry-widget)
  (setf *current-workspace*
        (uiop:ensure-directory-pathname
         (text path-entry-widget))))

(defun open-existing-project-popup ()
  (with-ltk ()
    (wm-title *tk* "Open existing project")
    (set-geometry *tk*
                  +DEFAULT-POPUP-WIDTH+
                  +DEFAULT-POPUP-HEIGHT+
                  (/ +TOPLEVEL-WIDTH+ 2)
                  (/ +TOPLEVEL-HEIGHT+ 2))
    (let* ((path-entry-label
            (make-instance 'label :text "Path to existing project"))
           (path-entry
            (make-instance 'entry :width 15))
           (open-project-button
            (make-instance 'button
                           :text "Open project"
                           :command (lambda ()
                                      (set-current-workspace-path path-entry)
                                      (setf *exit-mainloop* t)))))
      (pack path-entry-label)
      (pack path-entry)
      (pack open-project-button)

      (bind path-entry +ENTER-KEY-CODE+
            (lambda (event)
              (declare (ignore event))
              (set-current-workspace-path path-entry)
              (setf *exit-mainloop* t))))))

(defun new-project-popup ()
  (with-ltk ()
    (wm-title *tk* "Create a new project")
    (set-geometry *tk*
                  +DEFAULT-POPUP-WIDTH+
                  +DEFAULT-POPUP-HEIGHT+
                  (/ +TOPLEVEL-WIDTH+ 2)
                  (/ +TOPLEVEL-HEIGHT+ 2))
    (let* ((path-entry-label
            (make-instance 'label :text "Path to new project"))
           (path-entry
            (make-instance 'entry :width 15))
           (create-project-button
            (make-instance 'button
                           :text "Create project"
                           :command (lambda ()
                                      (make-project
                                       (pathname (text path-entry)))
                                      (set-current-workspace-path path-entry)
                                      (setf *exit-mainloop* t)))))
      (pack path-entry-label)
      (pack path-entry)
      (pack create-project-button)

      (bind path-entry +ENTER-KEY-CODE+
            (lambda (event)
              (declare (ignore event))
              (make-project (pathname (text path-entry)))
              (set-current-workspace-path path-entry)
              (setf *exit-mainloop* t))))))

(defun about-window-popup ()
  (with-ltk ()
    (wm-title *tk* "About CLIDLE")
    (set-geometry *tk*
                  380
                  (- +DEFAULT-POPUP-HEIGHT+ 50)
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
    
    (let* ((text-editor
            (make-instance 'text
                           :wrap :word))
           (repl-terminal
            (make-instance 'text
                           :wrap :word))
           (menu-bar
            (make-instance 'menubar))    
           (file-menu
            (make-instance 'menu
                           :master menu-bar
                           :text "File"))
           (repl-menu
            (make-instance 'menu
                           :master menu-bar
                           :text "REPL")))
      
      (make-instance 'menubutton
                     :master file-menu
                     :text "New project"
                     :command (lambda ()
                                (new-project-popup)
                                (load-text text-editor
                                           (format nil
                                                   "~a~a"
                                                   *current-workspace*
                                                   "src/main.lisp"))))
      (make-instance 'menubutton
                     :master file-menu
                     :text "Open a project"
                     :command (lambda ()
                                (open-existing-project-popup)
                                (load-text text-editor
                                           (format nil
                                                   "~a~a"
                                                   *current-workspace*
                                                   "src/main.lisp"))))
      (make-instance 'menubutton
                     :master file-menu
                     :text "Save current file"
                     :command (lambda ()
                                (save-text text-editor
                                           (format nil
                                                   "~a~a"
                                                   *current-workspace*
                                                   "src/main.lisp"))))
      (make-instance 'menubutton
                     :master repl-menu
                     :text "Restart REPL"
                     :command (lambda ()
                                nil))
      (make-instance 'menubutton
                     :master repl-menu
                     :text "Reload the current project"
                     :command (lambda ()
                                nil))
      (make-instance 'menubutton
                     :master repl-menu
                     :text "Compile and load the file"
                     :command (lambda ()
                                nil))
      (make-instance 'menubutton
                     :master menu-bar
                     :text "About"
                     :command (lambda ()
                                (about-window-popup)))
      
      (make-instance 'menubutton
                     :master file-menu
                     :text "Quit"
                     :command (lambda ()
                                (setf *exit-mainloop* t)))
      
      (pack text-editor)
      (pack repl-terminal) 
      (append-text repl-terminal
                   (format nil
                           "CLIDLE (~a)> "
                           (cursor-index repl-terminal)))
      (bind repl-terminal +ENTER-KEY-CODE+
            (lambda (event)
              (declare (ignore event))
              
              (append-text repl-terminal
                           (format nil
                                   "CLIDLE (~a)> "
                                   (cursor-index repl-terminal))))))))
