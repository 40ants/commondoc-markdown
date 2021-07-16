(defpackage #:commondoc-markdown/emitter
  (:use #:cl)
  (:import-from #:commondoc-markdown/core
                #:markdown))
(in-package commondoc-markdown/emitter)


(defvar *header-level*)


(defun write-header-prefix (stream)
  (loop repeat *header-level*
        do (write-char #\# stream))
  (write-char #\Space stream))


(defun write-header (format title stream)
  (write-header-prefix stream)
  (loop for piece in (uiop:ensure-list title)
        do (common-doc.format:emit-document format piece stream))
  (format stream "~2&"))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:document)
                                            stream)
  "Render a document to HTML stream."
  (let ((*header-level* (or (and (boundp '*header-level*)
                                 (1+ *header-level*))
                            1)))
    (write-header format
                  (common-doc:title node)
                  stream)

    (loop for child in (common-doc:children node)
          do (common-doc.format:emit-document format child stream))))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:document-node)
                                            stream)
  "Render a node to HTML stream."
  (format stream "Node of type ~S is not supported yet.~%"
          (type-of node)))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:section)
                                            stream)
  (let ((*header-level* (or (and (boundp '*header-level*)
                                 (1+ *header-level*))
                            1)))
    (write-header format
                  (common-doc:title node)
                  stream)

    (call-next-method)
    ;; (loop for child in (common-doc:children node)
    ;;       do (common-doc.format:emit-document format child stream))
    ))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:text-node)
                                            stream)
  (common-doc.format:emit-document format (common-doc:text node) stream))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (text string)
                                            stream)
  (write-string text
                stream))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:web-link)
                                            stream)
  (write-char #\[ stream)
  (call-next-method)
  (write-char #\] stream)

  (format stream "(~A)"
          (common-doc:uri node)))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:content-node)
                                            stream)
  (loop for child in (common-doc:children node)
        do ;; (format stream "~A: "
           ;;         child)
           (common-doc.format:emit-document format child stream)))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:paragraph)
                                            stream)
  (call-next-method)
  (format stream "~2&"))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:code-block)
                                            stream)
  (if (common-doc:language node)
      (format stream
              "```~A~%"
              (common-doc:language node))
      (format stream
              "```~%"))
  (call-next-method)
  (format stream
          "~&```~%"))

;;; Lists

;;; Definition lists are supported by some markdown parsers,
;;; but syntax may be different. 

(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:unordered-list)
                                            stream)
  
  (loop for item in (common-doc:children node)
        do (write-string "* " stream)
           (common-doc.format:emit-document format item stream))

  (fresh-line stream))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:ordered-list)
                                            stream)
  
  (loop for item in (common-doc:children node)
        for idx upfrom 1
        do (format stream "~A. " idx)
           (common-doc.format:emit-document format item stream))

  (fresh-line stream))

;;; Markup


;; TODO: support bold, italic, underline, strikethrough, superscript and subscript

(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:code)
                                            stream)
  (write-char #\` stream)
  (call-next-method)
  (write-char #\` stream))

