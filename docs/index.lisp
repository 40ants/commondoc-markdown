(uiop:define-package #:commondoc-markdown-docs/index
  (:use #:cl)
  (:import-from #:docs-config)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:export
   #:@index
   #:@readme))
(in-package #:commondoc-markdown-docs/index)


(in-readtable pythonic-string-syntax)


(defmethod docs-config:docs-config ((system (eql (asdf:find-system "commondoc-markdown-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "CommonDoc-Markdown")
  """
This is a wrapper around [3BMD](https://github.com/3b/3bmd) markdown parser which produces
documents in [CommonDoc](http://commondoc.github.io/) format. Also, it is able to render
CommonDoc documents into the Markdown.

It is a proof of the concept, but I'm already using it in the documentation builder
[40ANTS-DOC](https://40ants.com/doc/).

> **Note**. This library is not compatible with [CommonMark](https://commonmark.org/) yet.
"""
  (@installation section)
  (@parsing section)
  (@formatting section)
  (@roadmap section))


(40ants-doc:defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
This library available only at Ultralisp.org now. To install it using Quicklisp client, do:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)

(ql:quickload :commondoc-markdown)
```
""")


(defsection @parsing (:title "Parsing Markdown to CommonDoc")
  """
```
CL-USER> (common-doc.format:parse-document
          (make-instance 'commondoc-markdown:markdown)
          "
# Hello World

* First item
* Second item
")
#<COMMON-DOC:SECTION title: Hello World, ref: NIL>
```
""")


(defsection @formatting (:title "Writing CommonDoc to Markdown")
  """
Now we can render our document back to Markdown:

```
CL-USER> (common-doc.format:emit-to-string
          (make-instance 'commondoc-markdown:markdown)
          *document*)
"# Hello World

* First item

* Second item


"
```
""")


(defsection @roadmap (:title "Roadmap")
  """
* Make commondoc-markdown compatible with [CommonMark](https://commonmark.org/) syntax.

""")

