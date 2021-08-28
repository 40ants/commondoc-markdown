(defpackage #:commondoc-markdown/emitter
  (:use #:cl)
  (:import-from #:commondoc-markdown/core
                #:markdown)
  (:export #:*emit-section-anchors*
           #:*min-link-hash-length*
           #:hash-link))
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

(defvar *min-link-hash-length* 4
  "Minumum length of the hash for generated markdown links.")

(defvar *link->hash*)
(defvar *hash->link*)


(defmethod common-doc.format:emit-document :around ((format markdown)
                                                    node
                                                    stream)
  (let* ((toplevel (not (boundp '*link->hash*)))
         (*link->hash* (if toplevel
                           (make-hash-table :test 'equal)
                           *link->hash*))
         (*hash->link* (if toplevel
                           (make-hash-table :test 'equal)
                           *hash->link*)))
    (call-next-method)

    (when (and toplevel
               (hash-table-count *hash->link*))
      (format stream "~2&")
      (loop for (link . hash) in (sort (alexandria:hash-table-alist *link->hash*)
                                       #'string<
                                       :key #'first)
            do (format stream "~&[~A]: ~A"
                       hash link)))))


(defun hash-link (url
                  &key (min-n-chars *min-link-hash-length*))
  (unless (boundp '*link->hash*)
    (error "Function HASH-LINK should be called from COMMON-DOC.FORMAT:EMIT-DOCUMENT"))

  (let ((found (gethash url *link->hash*)))
    (when found
      (return-from hash-link found)))
  
  (let ((hex (ironclad:byte-array-to-hex-string
              (ironclad:digest-sequence 'ironclad:md5
                                        (babel:string-to-octets url)))))
    (loop for i upfrom min-n-chars below (length hex)
          do (let ((hash (subseq hex 0 (min (length hex) i))))
               (unless (gethash hash *hash->link*)
                 (setf (gethash hash *hash->link*)
                       url)
                 (setf (gethash url *link->hash*)
                       hash)
                 (return-from hash-link hash))))
    (assert nil () "MD5 collision collision detected.")))


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

  (format stream "[~A]"
          (hash-link
           
           (quri:render-uri
            (common-doc:uri node)))))


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
            (hash-link uri))))


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

