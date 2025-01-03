(uiop:define-package #:40ants-plantuml-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:40ants-plantuml-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-plantuml-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-plantuml-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "40ants-plantuml - Wrapper around PlantUML jar library"
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "PNG"
                                   "JAR"
                                   "TODO"
                                   "Unlicense"
                                   "PlantUML"
                                   "UML"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "API"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "GIT"))
  (40ants-plantuml system)
  "
[![](https://github-actions.40ants.com/40ants/plantuml/matrix.svg?only=ci.run-tests)](https://github.com/40ants/plantuml/actions)

![Quicklisp](http://quickdocs.org/badge/40ants-plantuml.svg)
"
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :40ants-plantuml)
```
""")


(defsection @usage (:title "Usage")
  """
To make a diagram, first you need to download JAR file with
Java program `PlantUML` from official site https://plantuml.com/.

Then you'll have to set a path to this JAR file like this:

```
CL-USER> (setf 40ants-plantuml:*path-to-jar*
               "~/plantuml-mit-1.2024.8.jar")

```

And of cause you will need some Java implementation suitable
for running this JAR file.

Here is how to render a sequence diagram to PNG file:

```
CL-USER> (40ants-plantuml:render "
  @startuml
  CommoLisp -> PlantUML : render
  activate PlantUML
  return PNG file
  @enduml
  "

#P"/tmp/diagram.png")
; No values
```

It will render an image like this:

![](docs/images/diagram.png)

""")


(defautodoc @api (:system "40ants-plantuml"))
