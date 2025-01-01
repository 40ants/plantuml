(defsystem "40ants-plantuml-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/plantuml"
  :class :package-inferred-system
  :description "Provides CI settings for 40ants-plantuml."
  :source-control (:git "https://github.com/40ants/plantuml")
  :bug-tracker "https://github.com/40ants/plantuml/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "40ants-plantuml-ci/ci"))
