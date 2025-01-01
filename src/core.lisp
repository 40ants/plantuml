(uiop:define-package #:40ants-plantuml
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:alexandria
                #:required-argument)
  (:nicknames #:40ants-plantuml/core)
  (:export #:render
           #:*path-to-jar*
           #:*path-to-graphviz*))
(in-package #:40ants-plantuml)


;; If unsure, use MIT version:
;; https://github.com/plantuml/plantuml/releases/download/v1.2024.8/plantuml-mit-1.2024.8.jar

(defvar *path-to-jar*
  nil
  "Set this variable to a path to the plantuml.jar. Note, there are different builds of plantuml with different licensing.")


(defvar *path-to-graphviz*
  nil
  "If given, should be a path to `dot` binary of Graphviz. Will be passed as -graphvizdot option to the `PlantUML`.")


(define-condition plantuml-error (error)
  ((exit-code :initarg :exit-code
              :type integer
              :initform (required-argument "EXIT-CODE is required argument.")
              :reader plantuml-exit-code)
   (error-message :initarg :error-message
                  :type string
                  :initform (required-argument "ERROR-MESSAGE is required argument.")
                  :reader plantuml-error-message))
  (:report (lambda (condition stream)
             (format stream "`PlantUML` exited with code ~A and this error output:~2%~A"
                     (plantuml-exit-code condition)
                     (plantuml-error-message condition)))))


(-> render (string (or string pathname))
    (values &optional))


(defun render (diagram-code output-filename)
  (unless *path-to-jar*
    (error "Please, set 40ants-plantuml:*path-to-jar* variable."))

  (unless (probe-file *path-to-jar*)
    (error "Variable 40ants-plantuml:*path-to-jar* points to ~S which can't be found."
           *path-to-jar*))

  (when (and *path-to-graphviz*
             (not (probe-file *path-to-graphviz*)))
    (error "Variable 40ants-plantuml:*path-to-graphviz* points to ~S which can't be found."
           *path-to-graphviz*))
  
  (with-input-from-string (input-stream diagram-code)
    (uiop:with-output-file (output-stream output-filename
                                          :if-exists :supersede
                                          :element-type '(unsigned-byte 8))
      (let ((error-stream (make-string-output-stream)))
        (handler-bind ((uiop:subprocess-error
                         (lambda (err)
                           (error 'plantuml-error
                                  :exit-code (uiop:subprocess-error-code err)
                                  :error-message (string-right-trim '(#\Space #\Newline)
                                                                    (get-output-stream-string error-stream))))))
          (uiop:run-program (append
                             (list "java"
                                   "-jar"
                                   (namestring
                                    (probe-file *path-to-jar*))
                                   "-pipe")
                             (when *path-to-graphviz*
                               (list "-graphvizdot"
                                     *path-to-graphviz*)))
                            :input input-stream
                            :output output-stream
                            :error-output error-stream)))
      ;; (values)
      )))
