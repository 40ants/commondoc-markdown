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


(defun make-inline-nodes (pieces)
  "PIECES argument may contain a strings
   or lists like (:CODE \"foo\").

   Consecutive strings are concatenated into a
   text nodes, for lists a CREATE-NODE function
   is applied"
  (let ((strings nil)
        (results nil))
    (labels ((collect-text-node-if-needed ()
               (when strings
                 (collect-node
                  (common-doc:make-text
                   (apply 'concatenate 'string
                          (nreverse strings))))
                 (setf strings nil)))
             (collect-node (node)
               (push node results)))
      (loop for piece in pieces
            do (typecase piece
                 (string (push piece strings))
                 (t
                  (collect-text-node-if-needed)
                  (collect-node
                   (create-node piece))))
            finally
               (collect-text-node-if-needed)
               (return (nreverse results))))))


(defun create-node (3bmd-node)
  (let ((node-type (car 3bmd-node))
        (content (cdr 3bmd-node)))
    (ecase node-type
      (:plain
       ;; Not sure if it is a good idea to
       ;; make :PLAIN a paragraph,
       ;; but seems it can contain a multiple
       ;; inline nodes. So I think it should be
       ;; ok for now.
       (common-doc:make-paragraph
        (make-inline-nodes content)))
      (:paragraph
       (common-doc:make-paragraph
        (make-inline-nodes content)))
      (:code
       (common-doc:make-code
        (make-inline-nodes content)))
      (3bmd-code-blocks::code-block
       (let* ((lang (getf content :lang))
              (code (getf content :content)))
         (common-doc:make-code-block lang
                                     (common-doc:make-text code)))))))


(defmethod common-doc.format:parse-document ((format markdown) (string string))
  (let* ((parsed-tree (let ((3bmd-code-blocks:*code-blocks* t))
                        (3bmd-grammar:parse-doc string)))
         (nodes (mapcar #'create-node parsed-tree)))
    (if (= (length nodes) 1)
        (first nodes)
        (common-doc:make-content nodes))))
