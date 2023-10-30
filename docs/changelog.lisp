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


(defchangelog (:ignore-words ("HASH-LINK"
                              "COMMON-DOC:STRIKETHROUGH"))
  (0.4.0 2023-10-30
         "* Fixed work with latest 3bmd Markdown parser.
          * Fixed the way how bullet lists are rendered into Markdown. Now there is no blank lines between list items.")
  (0.3.0 2022-03-10
         "* Parser was extended to produce a COMMON-DOC:STRIKETHROUGH objects
            when you enter `--text surrounded by two minus signs--`.")
  (0.2.0 2022-03-07
         "* Variable COMMONDOC-MARKDOWN/EMITTER:*GENERATE-SHORT-LINK-REFERENCES*
            was introduced. By default it is `T`, but you can bind it to NIL,
            to prevent short link references generation.
          * HASH-LINK function now is not exported from emitter package.")
  (0.1.0 2022-01-25
         """
Initial version with minimal docs.
"""))
