(uiop:define-package #:40ants-plantuml-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-plantuml-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "PlantUML"
                              "HTTP"))
  (0.2.0 2025-01-05
         "* Use a `PlantUML` installed by package manager, if it is available.")
  (0.1.0 2025-01-04
         "* Initial version."))
