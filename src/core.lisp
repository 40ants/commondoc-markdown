(defpackage #:commondoc-markdown
  (:use #:cl)
  (:nicknames #:commondoc-markdown/core)
  (:import-from #:3bmd)
  (:import-from #:common-doc
                #:get-meta
                #:make-meta)
  (:import-from #:commondoc-markdown/format
                #:markdown)
  (:import-from #:commondoc-markdown/raw-html
                #:make-raw-html-block
                #:make-raw-inline-html)
  (:export
   #:markdown
   #:make-markdown-link
   #:markdown-link
   #:markdown-link-definition))
(in-package commondoc-markdown)


(common-doc:define-node markdown-link (common-doc:link)
  ((definition :accessor markdown-link-definition
               :initarg :definition
               :initform nil
               :type (or null string)
               :documentation "A definition slug for the linked document."))
  (:documentation "A named link like [Some text][the-id].

                   These links can be replaced with a web-link if id is
                   defined somewhere in the markdown text."))


(common-doc:define-node line-break (common-doc:markup)
  ()
  (:documentation "Explicit line break. In Markdown you have to add two or more spaces at the end of the line.

                   When rendered to HTML, this node will be replaced with <br/>.
                   When rendered back to markdown - with two spaces and a new-line."))


(defun make-markdown-link (children &key definition)
  (make-instance 'markdown-link
                 :children (uiop:ensure-list children)
                 :definition definition))


(defmethod common-doc.ops:node-specific-equal ((link-a markdown-link)
                                               (link-b markdown-link))
  (equal (markdown-link-definition link-a)
         (markdown-link-definition link-b)))


(defvar *create-node-recursive-call* nil)

(defmacro go-deeper (&body body)
  `(let ((*create-node-recursive-call* t))
     ,@body))


(defun make-inline-nodes (pieces)
  "PIECES argument may contain a strings
   or lists like (:CODE \"foo\").

   Consecutive strings are concatenated into a
   text nodes, for lists a CREATE-NODE function
   is applied"
  (go-deeper
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
                 (return (nreverse results)))))))


;; This var will be bound during with-collected-references macro body execution
(defvar *link-references*)


(defun call-with-collected-references (nodes func)
  (let ((*link-references* (make-hash-table :test 'equal)))
    (labels ((process (node)
               (typecase node
                 (list
                  (cond
                    ((and (> (length node) 1)
                          (eql (car node) :reference))
                     (let* ((content (cdr node))
                            (definition (apply 'concatenate
                                               'string
                                               ;; If label is a space separated
                                               ;; string, then 3bmd will make it
                                               ;; a list of multiple strings:
                                               (getf content :label)))
                            (url (getf content :source)))
                       (setf (gethash
                              ;; Making reference searching case insensitive
                              (string-downcase definition)
                              *link-references* )
                             url))))))))
      (mapc #'process nodes))
    (funcall func)))


(defmacro with-collected-references ((nodes) &body body)
  `(call-with-collected-references ,nodes
                                   (lambda ()
                                     ,@body)))


(defun find-url (definition)
  (unless (boundp '*link-references*)
    (error "Function FIND-URL should be called inside WITH-COLLECTED-REFERENCES macro."))
  (gethash (string-downcase definition)
           *link-references*))


(defvar *sections-stack*)


(defun create-node (3bmd-node)
  (let* ((node-type (car 3bmd-node))
         (content (cdr 3bmd-node))
         (node
           (ecase node-type
             ;; 3bmd produces :raw-html for lines like this:
             ;; <s>Refactor code and make a core package with only a few dependencies.</s>
             (:raw-html
              (make-raw-inline-html (first content)))
             ;; and :html nodes are created for a multiline html code
             (:html
              (make-raw-html-block (first content)))
             (:line-break
              (make-instance 'line-break))
             (:emph
              (common-doc:make-italic (make-inline-nodes content)))
             (:strong
              (common-doc:make-bold (make-inline-nodes content)))
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
             (:block-quote
              (common-doc:make-block-quote
               (go-deeper
                 (mapcar #'create-node
                         content))))
             ;; We ignore references, because they are used
             ;; only for making weblinks
             (:reference
              nil)
             (:reference-link
              (let* ((label (getf content :label))
                     (label-nodes (make-inline-nodes label))
                     (definition (getf content :definition))
                     (url (find-url definition)))
                (if url
                    (common-doc:make-web-link url
                                              label-nodes)
                    (make-markdown-link label-nodes
                                        :definition definition))))
             (:explicit-link
              (let ((url (getf content :source))
                    (label (getf content :label)))
                (common-doc:make-web-link url
                                          (make-inline-nodes label))))
             (:link
              ;; Link in the form <https://40ants.com>
              (let ((url (first content)))
                (common-doc:make-web-link url
                                          (common-doc:make-text url))))
             (:code
              (common-doc:make-code
               (make-inline-nodes content)))
             (:verbatim ;; Code indented by 4 spaces
              (common-doc:make-code-block "text"
                                          (make-inline-nodes content)))
             (3bmd-code-blocks::code-block
              (let* ((lang (getf content :lang))
                     (code (getf content :content)))
                (common-doc:make-code-block lang
                                            (common-doc:make-text code))))
             (:bullet-list
              (common-doc:make-unordered-list
               (go-deeper
                 (mapcar #'create-node
                         content))))
             (:counted-list
              (common-doc:make-ordered-list
               (go-deeper
                 (mapcar #'create-node
                         content))))
             (:list-item
              (common-doc:make-list-item
               (go-deeper
                 (mapcar #'create-node
                         content))))
             (:heading
              (let* ((level (getf content :level))
                     (title-content (getf content :contents))
                     (title (make-inline-nodes title-content))
                     (metadata (make-meta
                                (list (cons "original-level" level)))))
                (common-doc:make-section title
                                         :metadata metadata))
              (let* ((level (getf content :level))
                     (title-content (getf content :contents))
                     (title (make-inline-nodes title-content))
                     (metadata (make-meta (list (cons "original-level" level)))))
                (common-doc:make-section title
                                         :metadata metadata)))
             (:image
              (let* ((options (cdar content))
                     (url (getf options :source))
                     (desc (getf options :title)))
                (common-doc:make-image url
                                       :description desc)))
             (:entity
              (let* ((entity (car content))
                     (plain-text (plump:decode-entities entity)))
                (common-doc:make-text plain-text))))))
    (typecase node
      (common-doc:section
       (cond
         ;; If prev section exists, then we have to chose
         ;; action according to relative levels.
         ;; 
         ;; When new section's level the same or larger, then we need
         ;; to pop prev-section from the stack and if it is the last one,
         ;; then to return it as result of the function.
         ;; 
         ;; Otherwise, a new section should be added to the previos and pushed
         ;; to the stack.
         (*sections-stack*
          (let* ((new-level (get-meta node "original-level")))
            (loop for prev-section = (first *sections-stack*)
                  for prev-level = (and *sections-stack*
                                        (get-meta prev-section "original-level"))
                  while (and prev-level
                             (<= new-level prev-level))
                  do (pop *sections-stack*))
            
            (cond
              (*sections-stack*
               ;; Adding node to the top of the stack and to the childrens of
               ;; the parent section. In this case we shouldn't return the node
               ;; from the function, because it is the part of the nodes tree.
               (let ((prev-section (first *sections-stack*)))
                 (setf (common-doc:children prev-section)
                       (nconc (common-doc:children prev-section)
                              (list node))))
               
               (push node *sections-stack*)
               (values nil))
              (t
               ;; Addint 
               (push node *sections-stack*)
               (values node)))))
         (t
          (push node *sections-stack*)
          (values node))))
      (t ;; for all other types of nodes
       (cond
         ;; If prev section exists, then we need to add node to it's content
         ;; and return NIL because this node will be the part of the tree.
         ;; 
         ;; Otherwise, node should be returned as is.
         ((and *sections-stack*
               ;; When node is a reference which shouldn't be rendered:
               ;; [SomeRef]: https://40ants.com
               ;; it will be NIL and we don't need to add it to the children.
               node
               (not *create-node-recursive-call*))
          (let* ((current-section (first *sections-stack*)))
            (setf (common-doc:children current-section)
                  (nconc (common-doc:children current-section)
                         (list node)))
            (values nil)))
         (t
          (values node)))))))

(defun parse-markdown (string)
  "This is just a helper to reuse in tests"
  (let ((3bmd-code-blocks:*code-blocks* t))
    (3bmd-grammar:parse-doc string)))


(defmethod common-doc.format:parse-document ((format markdown) (string string))
  (let* ((parsed-tree (parse-markdown string))
         (*sections-stack* nil)
         (nodes (with-collected-references (parsed-tree)
                  (remove
                   nil ;; create-node can return NIL if node should be skipped
                   (mapcar #'create-node parsed-tree)))))
    (if (= (length nodes) 1)
        (first nodes)
        (common-doc:make-content nodes))))
