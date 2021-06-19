(uiop:define-package #:commondoc-markdown-test/core
  (:use #:cl)
  (:import-from #:rove
                #:testing
                #:deftest)
  (:import-from #:common-doc
                #:make-code-block
                #:make-code
                #:make-content
                #:make-section
                #:make-text
                #:make-paragraph))
(in-package commondoc-markdown-test/core)


(defun p (text)
  (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                    text))

;; For debugging it is useful to render node to HTML
(defun r (node)
  (common-doc.format:emit-document (make-instance 'common-html:html) node t))


(defmacro compare (left right)
  `(rove:ok (common-doc.ops:node-equal ,left ,right)))


(deftest test-plain-text-parsing
  (testing "Single word"
    (compare (p "foo")
             (make-paragraph (make-text "foo"))))
  
  (testing "A few words in line"
    (compare (p "foo bar")
             (make-paragraph (make-text "foo bar"))))
  
  (testing "A few words in multiple lines"
    (compare (p "foo bar
blah minor")
             (make-paragraph (make-text "foo bar
blah minor")))))


(deftest test-paragraph-parsing
  (testing "A single paragraph with one word"
    (compare (p "foo

")
             (make-paragraph (make-text "foo"))))
  
  (testing "A single paragraph with a few word"
    (compare (p "a few words

")
             (make-paragraph (make-text "a few words"))))
  
  (testing "Multiple paragraphs should be grouped into a content node"
    (compare (p "A few words
in first paragraph.

And some words
in the second.

")
             (make-content (list (make-paragraph (make-text "A few words
in first paragraph."))
                                 (make-paragraph (make-text "And some words
in the second.")))))))


(deftest test-inline-code
  (testing "Simple code"
    (compare (p "`foo`")
             (make-paragraph (make-code (make-text "foo")))))
  (testing "Code with a few words"
    (compare (p "`foo bar`")
             (make-paragraph (make-code (make-text "foo bar")))))
  (testing "Code inside a text"
    (compare (p "This is a `foo bar` function.")
             (make-paragraph (list (make-text "This is a ")
                                   (make-code (make-text "foo bar"))
                                   (make-text " function.")))))
  (testing "Code inside a paragraph in a larger text"
    (compare (p "Ok!

This is a `foo bar` function.

The third paragraph!

")
             (make-content
              (list
               (make-paragraph (make-text "Ok!"))
               (make-paragraph (list (make-text "This is a ")
                                     (make-code (make-text "foo bar"))
                                     (make-text " function.")))
               (make-paragraph (make-text "The third paragraph!")))))))


(deftest test-multiline-code
  (testing "Simple code"
    (compare (p "```
Hello

World
```")
             (make-code-block ""
                              (make-text "Hello

World"))))
  
  (testing "Code with language"
    (compare (p "```lisp
Hello

World
```")
             (make-code-block "lisp"
                              (make-text "Hello

World"))))
  
  (testing "Code surrounded with paragraphs"
    (compare (p "Here is an example:

```lisp
Hello

World
```

Now you know everything!

")
             (make-content (list
                            (make-paragraph (make-text "Here is an example:"))
                            (make-code-block "lisp"
                                             (make-text "Hello

World"))
                            (make-paragraph (make-text "Now you know everything!")))))))
