(defsystem commondoc-markdown-docs
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :licence "Unlicense"
  :description "A documentation for CommonDoc-Markdown system"
  :homepage "https://40ants.com/commondoc-markdown/"
  :source-control (:git "https://github.com/40ants/commondoc-markdown")
  :pathname "docs"
  :depends-on ("commondoc-markdown-docs/index"
               "commondoc-markdown-docs/changelog"))
