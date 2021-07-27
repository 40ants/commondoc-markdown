(defpackage #:commondoc-markdown/format
  (:use #:cl)
  (:export
   #:markdown))
(in-package commondoc-markdown/format)


(defclass markdown (common-doc.format:document-format)
  ()
  (:documentation "The Markdown format."))
