(defpackage #:commondoc-markdown/emitter
  (:use #:cl)
  (:import-from #:commondoc-markdown/core
                #:markdown)
  (:export #:*emit-section-anchors*))
(in-package commondoc-markdown/emitter)


(defvar *header-level*)

(defvar *emit-section-anchors* t)


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


;; Links

(defmethod common-doc.format:emit-document :before ((format markdown)
                                                    (node common-doc:document-node)
                                                    stream)
  (when (and (common-doc:reference node)
             *emit-section-anchors*)
    (format stream "<a id=\"~A\"></a>~2&"
            (common-doc:reference node))))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:web-link)
                                            stream)
  (write-char #\[ stream)
  (call-next-method)
  (write-char #\] stream)

  (format stream "(~A)"
          (common-doc:uri node)))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:document-link)
                                            stream)
  (write-char #\[ stream)
  (call-next-method)
  (write-char #\] stream)

  (let ((uri (format nil "~A#~A"
                     (common-doc:document-reference node)
                     (common-doc:node-reference node))))
    (format stream "(~A)"
            uri)))


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


(defun get-line-backticks-count (line)
  (let* ((backticks-count (count #\` line)))
    ;; We only interested in lines having
    ;; only backticks:
    (if (= (length line)
           backticks-count)
        backticks-count
        0)))


(defun get-num-required-backticks (code-text)
  "If code includes examples of markdown code blocks, then we should
   select higher number of backticks.

   This function returns a number of backticks, required to
   wrap given CODE-TEXT into a Markdown code-block."

  (loop with max-backticks-count = 0
        for line in (str:split #\Newline code-text)
        do (setf max-backticks-count
                 (max max-backticks-count
                      (get-line-backticks-count line)))
        finally (return (max 3
                             (1+ max-backticks-count)))))


(defun make-fence (num-backticks)
  (with-output-to-string (s)
    (loop repeat num-backticks
          do (write-char  #\` s))))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:code-block)
                                            stream)
  (let* ((content (with-output-to-string (s)
                    (call-next-method format node s)))
         (fence (make-fence (get-num-required-backticks content))))
    (if (common-doc:language node)
        (format stream
                "~A~A~%"
                fence
                (common-doc:language node))
        (format stream
                "~A~%"
                fence))
    (write-string content stream)
    (format stream
            "~&~A~%"
            fence)))

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

