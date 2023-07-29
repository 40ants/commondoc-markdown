(uiop:define-package #:commondoc-markdown-test/core
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:testing
                #:deftest)
  (:import-from #:common-doc
                #:make-web-link
                #:make-code-block
                #:make-code
                #:make-content
                #:make-section
                #:make-text
                #:make-list-item
                #:make-ordered-list
                #:make-paragraph
                #:text
                #:title)
  (:import-from #:commondoc-markdown
                #:make-markdown-link)
  (:import-from #:common-doc.ops
                #:collect-all-text))
(in-package commondoc-markdown-test/core)


(defun p (text)
  (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                    text))

(defun pp (text)
  (commondoc-markdown::parse-markdown text))

;; For debugging it is useful to render node to HTML
(defun r (node)
  (common-doc.format:emit-document (make-instance 'common-html:html) node t))

(defun rr (node)
  (with-output-to-string (stream)
    (common-doc.format:emit-document (make-instance 'common-html:html) node stream)))

(defun mm (node)
  "Renders document into a markdown string"
  (with-output-to-string (stream)
    (common-doc.format:emit-document (make-instance 'commondoc-markdown:markdown)
                                     node stream)))


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


(deftest test-bullet-parsing
  (testing "Parsing a bullet list"
    (compare (p "- test 1
- test 2
- test 3

")
             (make-unordered-list
              (list
               (make-list-item
                (make-paragraph
                 (make-text "test 1")))
               (make-list-item
                (make-paragraph
                 (make-text "test 2")))
               (make-list-item
                (make-paragraph
                 (make-text "test 3")))))))

  (testing "Round tripping a bullet list"
    (let ((md-string "* test 1
* test 2
* test 3

"))
      (ok (string-equal (mm (p md-string)) md-string)))))


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


(deftest test-links
  (testing "External link"
    (compare (p "A text with [a link](https://40ants.com).")
             (make-paragraph (list
                              (make-text "A text with ")
                              (make-web-link "https://40ants.com"
                                             (make-text "a link"))
                              (make-text ".")))))
  
  (testing "Link with empty definition"
    (compare (p "A text with [a link][].")
             (make-paragraph (list
                              (make-text "A text with ")
                              (make-markdown-link (make-text "a link"))
                              (make-text ".")))))

  (testing "Link with not defined definition"
    (compare (p "A text with [a link][function].")
             (make-paragraph (list
                              (make-text "A text with ")
                              (make-markdown-link (make-text "a link")
                                                  :definition "function")
                              (make-text ".")))))

  (testing "Link with defined definition"
    (compare (p "A text with [a link][function].

[function]: https://40ants.com/some-func")
             (make-paragraph (list
                              (make-text "A text with ")
                              (make-web-link "https://40ants.com/some-func"
                                             (make-text "a link"))
                              (make-text "."))))))


(deftest test-code-links
  (testing "External link"
    (compare (p "A text with [`a link`][function].")
             (make-paragraph (list
                              (make-text "A text with ")
                              (make-markdown-link (make-code (make-text "a link"))
                                                  :definition "function")
                              (make-text "."))))))


(deftest test-link-references
  (testing "External link"
    (let ((result (mm
                   (make-paragraph (list
                                    (make-text "A text with ")
                                    (make-web-link "https://40ants.com"
                                                   (make-text "a link 1"))
                                    (make-text " and ")
                                    (make-web-link "https://40ants.com"
                                                   (make-text "a link 2"))
                                    (make-text "."))))))
      (ok (string=
           "A text with [a link 1][9bf8] and [a link 2][9bf8].


[9bf8]: https://40ants.com"
           result)))))


(deftest test-section-parsing
  (testing "A single section"
    (let ((doc (p "
The Header
==========

Content, line one.

- first item
- second item

Second line.")))
      (ok (typep doc 'common-doc:section))
      (let ((children (common-doc:children doc)))
        (ok (= (length children) 3))
        (ok (typep (first children) 'common-doc:paragraph))
        (ok (typep (second children) 'common-doc:unordered-list))
        (ok (typep (third children) 'common-doc:paragraph)))))
  
  (testing "Two sections on the same level"
    (let ((doc (p "
First Header
============

First paragraph.

Second Header
=============

Second paragraph.
")))
      (ok (typep doc 'common-doc:content-node))
      (let ((children (common-doc:children doc)))
        (ok (typep (first children) 'common-doc:section))
        (ok (string= (text (first (title (first children)))) "First Header"))
        
        (ok (typep (second children) 'common-doc:section))
        (ok (string= (text (first (title (second children)))) "Second Header")))))

  (testing "Two nested sections"
    (let ((doc (p "
First Header
============

First paragraph.

Second Header
-------------

Second paragraph.
")))
      (ok (typep doc 'common-doc:section))
      (let ((children (common-doc:children doc)))
        (ok (typep (first children) 'common-doc:paragraph))
        (ok (string= (collect-all-text (first children)) "First paragraph."))
        
        (let* ((inner-section (second children))
               (children (common-doc:children inner-section)))
          (ok (typep inner-section 'common-doc:section))
          (ok (string= (text (first (title inner-section))) "Second Header"))

          (ok (= (length children) 1))
          (ok (typep (first children) 'common-doc:paragraph))
          (ok (string= (collect-all-text (first children)) "Second paragraph."))))))

  (testing "References should not be presented as NIL children"
    (let ((doc (p "
The Header
==========

Content.

[TheRef]: https://40ants.com
")))
      (ok (typep doc 'common-doc:section))
      (let ((children (common-doc:children doc)))
        (ok (= (length children) 1))
        (ok (typep (first children) 'common-doc:paragraph))))))


(deftest line-breaks
  (testing "Rendering back to Markdown and HTML"
    (let* ((doc (p "
This is  
the breaked line.
"))
           (html (rr doc))
           (markdown (mm doc)))

      (ok (equal markdown
                 "This is  
the breaked line.

"))
      (ok (equal html
                 "<p>This is<br/>the breaked line.</p>")))))
