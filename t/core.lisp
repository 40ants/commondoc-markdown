(defpackage #:commondoc-markdown-test/core
  (:use #:cl)
  (:import-from #:rove
                #:testing
                #:deftest)
  (:import-from #:common-doc
                #:make-content
                #:make-section
                #:make-text
                #:make-paragraph))
(in-package commondoc-markdown-test/core)


(defun p (text)
  (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                    text))

(defmacro compare (left right)
  `(rove:ok (common-doc.ops:node-equal ,left ,right)))


(deftest test-plain-text-parsing
  (testing "Single word"
    (compare (p "foo")
             (make-text "foo")))
  
  (testing "A few words in line"
    (compare (p "foo bar")
             (make-text "foo bar")))
  
  (testing "A few words in multiple lines"
    (compare (p "foo bar
blah minor")
             (make-text "foo bar
blah minor"))))


(deftest test-paragraph-parsing
  (testing "A single paragraph with one word"
    (compare (p "foo

")
             (make-paragraph (list (make-text "foo")))))
  
  (testing "A single paragraph with a few word"
    (compare (p "a few words

")
             (make-paragraph (list (make-text "a few words")))))
  
  (testing "Multiple paragraphs should be grouped into a content node"
    (compare (p "A few words
in first paragraph.

And some words
in the second.

")
             (make-content (list (make-paragraph (list (make-text "A few words
in first paragraph.")))
                                 (make-paragraph (list (make-text "And some words
in the second."))))))))
