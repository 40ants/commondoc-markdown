(defsystem "commondoc-markdown"
  :author "Alexander Artemenko"
  :license "Unlicense"
  :homepage "https://40ants.com/commondoc-markdown"
  :bug-tracker "https://github.com/40ants/commondoc-markdown/issues"
  :source-control (:git "https://github.com/40ants/commondoc-markdown")
  :description "Converter from Markdown to CommonDoc."
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("commondoc-markdown/core")
  :in-order-to ((test-op (test-op commondoc-markdown-test))))