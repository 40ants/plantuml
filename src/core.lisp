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
  (let ((path-to-jar
          (cond
            ((null *path-to-jar*)
             (let ((system-jar
                     (probe-file "/usr/share/plantuml/plantuml.jar")))
               (cond
                 (system-jar
                  system-jar)
                 (t
                  (error "Please, set 40ants-plantuml:*path-to-jar* variable or do \"apt install plantuml\".")))))
            ((probe-file *path-to-jar*)
             (probe-file *path-to-jar*))
            (t
             (error "Variable 40ants-plantuml:*path-to-jar* points to ~S which can't be found."
                    *path-to-jar*))))
        (path-to-graphviz
          (when *path-to-graphviz*
            (cond
              ((probe-file *path-to-graphviz*)
               ;; We should not use result of PROBE-FILE
               ;; here because it can resolve /usr/bin/dot
               ;; to /usr/sbin/libgvc6-config-update and
               ;; graphviz will work incorrectly when used like this.
               *path-to-graphviz*)
              (t
               (error "Variable 40ants-plantuml:*path-to-graphviz* points to ~S which can't be found."
                      *path-to-graphviz*))))))
  
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
            (let ((command-line
                    (append
                     (list "java"
                           "-jar"
                           (namestring
                            (probe-file path-to-jar))
                           "-pipe")
                     (when path-to-graphviz
                       (list "-graphvizdot"
                             (namestring
                              path-to-graphviz))))))
              (uiop:run-program command-line
                                :input input-stream
                                :output output-stream
                                :error-output error-stream))))
        (values)))))
