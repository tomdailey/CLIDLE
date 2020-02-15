(defpackage clidle
  (:use :cl :ltk)
  (:import-from :cl-project
                :make-project)
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
(defparameter +PROJECT-URL+ "https://github.com/momozor/CLIDLE")
(defparameter +DEFAULT-SWANK-HOST+ "localhost")
(defparameter +DEFAULT-SWANK-PORT+ 7891)
(defparameter +ENTER-KEY-CODE+ "<Return>")
(defparameter *current-workspace* "")

(defun set-current-workspace-path (path-entry-widget)
  (when (> (length (text path-entry-widget)) 0)
    (setf *current-workspace*
          (uiop:ensure-directory-pathname
           (text path-entry-widget)))))

(defun current-workspace-path-not-empty ()
  (> (length (namestring *current-workspace*)) 0))

(defun combined-path ()
  (when (current-workspace-path-not-empty)
    (format nil
            "~a~a"
            *current-workspace*
            "src/main.lisp")))


(defun swank-load-and-compile-file ()
  (with-slime-connection (connection +DEFAULT-SWANK-HOST+ +DEFAULT-SWANK-PORT+)
    (slime-eval (progn
                  (compile-file (pathname (combined-path)))
                  (load (pathname (combined-path))))
                connection)))

;;; GUI
(defmacro with-popup ((&key title (width 150) (height 100)) &body body)
  `(progn
     (with-ltk ()
       (wm-title *tk* ,title)
       (set-geometry *tk*
                     ,width
                     ,height
                     (/ +TOPLEVEL-WIDTH+ 2)
                     (/ +TOPLEVEL-HEIGHT+ 2))
       ,@body)))

(defun open-project-popup ()
  (with-popup (:title "Open project")
    (let* ((label
            (make-instance 'label :text "Path to existing project"))
           (entry
            (make-instance 'entry :width 15))
           (submit
            (make-instance 'button
                           :text "Open project"
                           :command (lambda ()
                                      (set-current-workspace-path entry)
                                      (setf *exit-mainloop* t)))))
      (pack label)
      (pack entry)
      (pack submit))))

(defun new-project-popup ()
  (with-popup (:title "New project")
    (let* ((label
            (make-instance 'label :text "Path to new project"))
           (entry
            (make-instance 'entry :width 15))
           (create-button
            (make-instance 'button
                           :text "Create project"
                           :command (lambda ()
                                      (set-current-workspace-path entry)
                                      (when (> (length (text entry)) 0)
                                        (make-project
                                         (pathname (text entry))
                                         :without-tests t))
                                      (setf *exit-mainloop* t)))))
      (pack label)
      (pack entry)
      (pack create-button))))

(defun about-popup ()
  (with-popup (:title "About CLIDLE" :width 380 :height 50)
    (pack (make-instance 'label
                         :text (format nil
                                       "Version: ~a~%Project repository: ~a~%"
                                       +VERSION+
                                       +PROJECT-URL+)))))

(defun main ()
  (with-ltk ()
    (wm-title *tk* "Common Lisp IDLE")
    (set-geometry *tk*
                  +TOPLEVEL-WIDTH+
                  +TOPLEVEL-HEIGHT+
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
                                (when (current-workspace-path-not-empty)
                                  (load-text text-editor (combined-path)))))
      (make-instance 'menubutton
                     :master file-menu
                     :text "Open a project"
                     :command (lambda ()
                                (open-project-popup)
                                (when (current-workspace-path-not-empty)
                                  (load-text text-editor (combined-path)))))
      (make-instance 'menubutton
                     :master file-menu
                     :text "Save current file"
                     :command (lambda ()
                                (when (current-workspace-path-not-empty)
                                  (save-text text-editor (combined-path)))))
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
                                (swank-load-and-compile-file)))
      (make-instance 'menubutton
                     :master menu-bar
                     :text "About"
                     :command (lambda ()
                                (about-popup)))      
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

              (with-slime-connection (connection +DEFAULT-SWANK-HOST+ +DEFAULT-SWANK-PORT+ )
                (slime-eval
                 '(cons 1 2)
                 connection))
              
              (append-text repl-terminal
                           (format nil
                                   "CLIDLE (~a)> "
                                   (cursor-index repl-terminal))))))))
