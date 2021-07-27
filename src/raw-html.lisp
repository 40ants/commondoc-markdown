(uiop:define-package #:commondoc-markdown/raw-html
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:commondoc-markdown/format
                #:markdown)
  (:import-from #:common-html.emitter)
  (:export
   #:raw-html
   #:raw-inline-html
   #:raw-html-block
   #:make-raw-html-block
   #:html
   #:make-raw-inline-html))
(in-package commondoc-markdown/raw-html)


(defclass raw-html (common-doc:document-node)
  ((html :initarg :html
         :type string
         :reader html)))


(defclass raw-html-block (raw-html)
  ())


(defclass raw-inline-html (raw-html)
  ())


(defun make-raw-html-block (html-string)
  (check-type html-string string)
  (make-instance 'raw-html-block
                 :html html-string))


(defun make-raw-inline-html (html-string)
  (check-type html-string string)
  (make-instance 'raw-inline-html
                 :html html-string))



(defmethod common-doc.format:emit-document ((format markdown)
                                            (node raw-html-block)
                                            stream)
  
  (format stream "~2&~A~2&"
          (html node)))


(common-html.emitter::define-emitter (obj raw-html-block)
  (write-string (html obj)
                common-html.emitter::*output-stream*))


(defmethod common-doc.format:emit-document ((format markdown)
                                            (node raw-inline-html)
                                            stream)
  
  (write-string (html node)
                stream))


(common-html.emitter::define-emitter (obj raw-inline-html)
  (write-string (html obj)
                common-html.emitter::*output-stream*))

