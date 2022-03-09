(uiop:define-package #:commondoc-markdown/addons
  (:use #:cl)
  (:import-from #:esrap
                #:defrule
                #:!))
(in-package #:commondoc-markdown/addons)


(defrule strike-open (and "--"
                          (! 3bmd-grammar::space-char)
                          (! 3bmd-grammar::newline)))

(defrule strike-close (and (! 3bmd-grammar::space-char)
                           (! 3bmd-grammar::newline)
                           inline
                           "--")
  (:destructure (s n inline s2)
    (declare (ignore s n s2))
    inline))


(3bmd-ext:define-extension-inline *strikethrough* strike
    (and strike-open
         (* (and (! strike-close)
                 inline))
         strike-close)
  (:destructure (o (&rest inlines) e)
                (declare (ignore o))
                (cons :strikethrough
                      (append (mapcar #'second inlines)
                              (list e))))
  (:after 3bmd-grammar:emph)
  (:character-rule strike-extended-chars #\-))
