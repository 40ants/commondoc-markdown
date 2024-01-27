(defpackage #:commondoc-markdown/format
  (:use #:cl)
  (:import-from #:common-doc.format)
  (:export #:markdown))
(in-package #:commondoc-markdown/format)


(defclass markdown (common-doc.format:document-format)
  ()
  (:documentation "The Markdown format."))
