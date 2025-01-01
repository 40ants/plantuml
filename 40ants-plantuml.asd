#-asdf3.1 (error "40ants-plantuml requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "40ants-plantuml"
  :description "Wrapper around PlantUML jar library"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/plantuml"
  :source-control (:git "https://github.com/40ants/plantuml")
  :bug-tracker "https://github.com/40ants/plantuml/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("40ants-plantuml/core")
  :in-order-to ((test-op (test-op "40ants-plantuml-tests"))))
