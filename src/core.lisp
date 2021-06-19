(defpackage #:commondoc-markdown
  (:use #:cl)
  (:nicknames #:commondoc-markdown/core)
  (:import-from #:3bmd)
  (:import-from #:common-doc)
  (:export
   #:markdown))
(in-package commondoc-markdown)


(defclass markdown (common-doc.format:document-format)
  ()
  (:documentation "The Markdown format."))


(defun make-text-node (pieces)
  (common-doc:make-text
   (apply 'concatenate 'string
          pieces)))


(defun create-node (3bmd-node)
  (let ((node-type (car 3bmd-node))
        (content (cdr 3bmd-node)))
    (ecase node-type
      (:plain
       (make-text-node content))
      (:paragraph
       (common-doc:make-paragraph
        (list (make-text-node content)))))))


(defmethod common-doc.format:parse-document ((format markdown) (string string))
  (let* ((parsed-tree (3bmd-grammar:parse-doc string))
         (nodes (mapcar #'create-node parsed-tree)))
    (if (= (length nodes) 1)
        (first nodes)
        (common-doc:make-content nodes))))
