(defsystem "commondoc-markdown-test"
  :author "Alexander Artemenko"
  :license "Unlicense"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               "rove"
               "commondoc-markdown-test/core")
  :description "Test system for COMMONDOC-MARKDOWN system."

  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))

(asdf:register-system-packages "common-doc" '("COMMON-DOC.OPS"))
