(defsystem "40ants-plantuml-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/plantuml"
  :class :package-inferred-system
  :description "Provides documentation for 40ants-plantuml."
  :source-control (:git "https://github.com/40ants/plantuml")
  :bug-tracker "https://github.com/40ants/plantuml/issues"
  :pathname "docs"
  :depends-on ("40ants-plantuml"
               "40ants-plantuml-docs/index"))
