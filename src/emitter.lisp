(uiop:define-package #:commondoc-markdown/emitter
  (:use #:cl)
  (:import-from #:commondoc-markdown/core
                #:markdown
                #:line-break)
  (:import-from #:alexandria
                #:hash-table-alist)
  (:import-from #:common-html.emitter)
  (:import-from #:str)
  (:import-from #:ironclad)
  (:import-from #:babel)
  (:import-from #:quri)
  (:export #:*emit-section-anchors*
           #:*min-link-hash-length*
           #:*generate-short-link-references*))
(in-package #:commondoc-markdown/emitter)


(defvar *header-level*)
(defvar *inhibit-paragraph-breaks* nil)

(defvar *emit-section-anchors* t
  "When this variable is `T` (default), emitter outputs
   a raw html `<a name=\"some-id\"></a>` before each
   Markdown section.")

(defvar *generate-short-link-references* t
  "By default it is `T`, but you can bind it to NIL,
   to prevent short link references generation.")


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

    (call-next-method)))


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
  "Minumum length of the hash for generated markdown links.
   This works only when *GENERATE-SHORT-LINK-REFERENCES* variable
   is set to `T`.")

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

    ;; We only need to output hashes and links when there
    ;; is something to output:
    (when (and toplevel
               (> (hash-table-count *hash->link*)
                  0))
      (format stream "~2&")
      (loop for (link . hash) in (sort (hash-table-alist *link->hash*)
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

  (let ((url (quri:render-uri
              (common-doc:uri node))))
    (if *generate-short-link-references*
        (format stream "[~A]"
                (hash-link url))
        (format stream "(~A)"
                url))))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:document-link)
                                            stream)
  (write-char #\[ stream)
  (call-next-method)
  (write-char #\] stream)

  (let ((uri (format nil "~A#~A"
                     (common-doc:document-reference node)
                     (common-doc:node-reference node))))
    (if *generate-short-link-references*
        (format stream "[~A]"
                (hash-link uri))
        (format stream "(~A)"
                uri))))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:content-node)
                                            stream)
  (loop for child in (common-doc:children node)
        do (common-doc.format:emit-document format child stream)))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:paragraph)
                                            stream)
  (call-next-method)
  (if *inhibit-paragraph-breaks*
      (format stream "~1&")
      (format stream "~2&")))


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
           (let ((*inhibit-paragraph-breaks* t))
             (common-doc.format:emit-document format item stream)))

  (format stream "~%"))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:ordered-list)
                                            stream)
  
  (loop for item in (common-doc:children node)
        for idx upfrom 1
        do (format stream "~A. " idx)
           (let ((*inhibit-paragraph-breaks* t))
             (common-doc.format:emit-document format item stream)))

  (format stream "~%"))

;;; Markup


;; TODO: support underline, strikethrough, superscript and subscript

(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:bold)
                                            stream)
  (write-string "**" stream)
  (call-next-method)
  (write-string "**" stream))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:italic)
                                            stream)
  (write-string "*" stream)
  (call-next-method)
  (write-string "*" stream))

(defmethod common-doc.format:emit-document ((format markdown)
                                            (node line-break)
                                            stream)
  (format stream "  ~%"))

(common-html.emitter::define-emitter (line-break line-break)
  (write-string "<br/>"
                common-html.emitter::*output-stream*))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:code)
                                            stream)
  (write-char #\` stream)
  (call-next-method)
  (write-char #\` stream))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:image)
                                            stream)
  (format stream "![~A](~A)"
          (or (common-doc:description node)
              "")
          (common-doc:source node)))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node common-doc:block-quote)
                                            stream)
  (let* ((content (with-output-to-string (s)
                    (call-next-method format node s)))
         (lines (str:split #\Newline content)))
    
    (format stream "~&")
    (loop for line in lines
          do (format stream "> ~A~%"
                     line))
    (format stream "~%")))
