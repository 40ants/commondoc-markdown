(uiop:define-package #:commondoc-markdown-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:commondoc-markdown-docs/changelog)

(in-readtable pythonic-string-syntax)


(defchangelog ()
  (0.1.0 2022-01-25
         """
Initial version with minimal docs.
"""))
