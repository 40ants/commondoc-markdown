(uiop:define-package #:commondoc-markdown/utils
  (:use #:cl)
  (:import-from #:str
                #:starts-with-p))
(in-package #:commondoc-markdown/utils)


(declaim (inline parse-tree-p))
(defun parse-tree-p (parse-tree tag)
  (and (listp parse-tree)
       (eq (first parse-tree) tag)))


(defun parse-tree-to-text (parse-tree &key deemph)
  ;; This function was taken from:
  ;; https://github.com/melisgl/mgl-pax/blob/f7a6c51b187b10dc39470b77fac05aaeb7b4e781/src/document/markdown.lisp#L170
  (labels
      ((recurse (e)
         (cond ((stringp e)
                ;; "S" -> "S"
                e)
               ((and (listp e)
                     (or (stringp (first e))
                         (listp (first e))))
                ;; ("mgl-pax-test:" (:EMPH "test-variable")) =>
                ;; "mgl-pax-test:*test-variable*"
                (apply #'concatenate 'string (mapcar #'recurse e)))
               ;; Recurse into (:PLAIN ...)
               ((parse-tree-p e :plain)
                (format nil "~A" (recurse (rest e))))
               ;; (:EMPH "S") -> "*S*"
               ((and deemph (parse-tree-p e :emph))
                (format nil "*~A*" (recurse (rest e))))
               ;; (:CODE "S") -> "S"
               ((parse-tree-p e :code)
                (let ((string (second e)))
                  (cond ((starts-with-p "\\\\" string)
                         (recurse (subseq string 2)))
                        ((starts-with-p "\\" string)
                         (recurse (subseq string 1)))
                        (t
                         (recurse string)))))
               (t
                (return-from parse-tree-to-text nil)))))
    (recurse parse-tree)))
