(ns ssgr.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [ssgr.parser :as p]
            [ssgr.doc :as d]
            [hiccup.compiler :as h.c]))

(deftest thematic-break-line
  (is (p/thematic-break? "---"))
  (is (p/thematic-break? "___"))
  (is (p/thematic-break? "***"))
  (is (p/thematic-break? " ---"))
  (is (p/thematic-break? "   ---"))
  (is (p/thematic-break? "   -------      \t\t\t\t"))
  (is (not (p/thematic-break? "    ---")))
  (is (not (p/thematic-break? "    ---*")))
  (is (not (p/thematic-break? "    ----- ***"))))

(deftest atx-heading-line
  (is (p/atx-heading? "#"))
  (is (p/atx-heading? "##"))
  (is (p/atx-heading? "###"))
  (is (p/atx-heading? "####"))
  (is (p/atx-heading? "#####"))
  (is (p/atx-heading? "######"))
  (is (not (p/atx-heading? "    #")))
  (is (not (p/atx-heading? "#Richo")))
  (is (not (p/atx-heading? "    ----- ***"))))

(deftest setext-heading-underline-line
  (is (p/setext-heading-underline? "-"))
  (is (p/setext-heading-underline? "="))
  (is (p/setext-heading-underline? "------   "))
  (is (p/setext-heading-underline? "   -"))
  (is (not (p/setext-heading-underline? "    -")))
  (is (not (p/setext-heading-underline? "-="))))

(deftest indented-code-block-line
  (is (p/indented-code-block? "    a"))
  (is (p/indented-code-block? "    a "))
  (is (p/indented-code-block? "     Richo capo    "))
  (is (not (p/indented-code-block? "     ")))
  (is (not (p/indented-code-block? "   Richo"))))

(deftest code-fence-line
  (is (p/code-fence? "```"))
  (is (p/code-fence? "````"))
  (is (p/code-fence? "~~~"))
  (is (p/code-fence? "~~~```"))
  (is (p/code-fence? "```python    [[[]]]```"))
  (is (p/code-fence? "   ```python"))
  (is (not (p/code-fence? "    ```python"))))

(deftest blank-line
  (is (p/blank? "\n"))
  (is (p/blank? "\t\n"))
  (is (p/blank? "   \n"))
  (is (p/blank? "    \n"))
  (is (not (p/blank? "asdf\n")))
  (is (not (p/blank? "  asdf\n")))
  (is (not (p/blank? "    asdf\n"))))

(deftest paragraph-line
  (is (p/paragraph? "Richo"))
  (is (p/paragraph? "Richo  "))
  (is (p/paragraph? "  Richo"))
  (is (not (p/paragraph? "    Richo"))))

(deftest regular-text
  (is (= (p/parse "Texto normal")
         (d/document
          (d/paragraph
           (d/text "Texto normal"))))))

(deftest paragraph
  (is (= (p/parse "P1. L1\nP1. L2\n\nP2. L1")
         (d/document
          (d/paragraph (d/text "P1. L1")
                       (d/text "P1. L2"))
          (d/paragraph (d/text "P2. L1"))))))

(deftest atx-heading
  (is (= (p/parse "# Heading 1")
         (d/document
          (d/heading 1
                     (d/text "Heading 1")))))
  (is (= (p/parse "  ## Heading 2    \n")
         (d/document
          (d/heading 2
                     (d/text "Heading 2"))))))

(deftest atx-heading-with-trailing-hashtags
  (is (= (p/parse "# Heading#")
         (d/document
          (d/heading 1
                     (d/text "Heading#")))))
  (is (= (p/parse "# Heading #")
         (d/document
          (d/heading 1
                     (d/text "Heading")))))
  (is (= (p/parse "# Heading  ##")
         (d/document
          (d/heading 1
                     (d/text "Heading"))))))

(deftest thematic-breaks
  (is (= (p/parse "Texto 1\n***\nTexto 2\n___")
         (d/document
          (d/paragraph (d/text "Texto 1"))
          (d/thematic-break)
          (d/paragraph (d/text "Texto 2"))
          (d/thematic-break)))))

(deftest setext-headings
  (is (= (p/parse "Texto 1\n=\nTexto 2\n---")
         (d/document
          (d/heading 1 (d/text "Texto 1"))
          (d/heading 2 (d/text "Texto 2"))))))

(deftest fenced-code-blocks
  (is (= (p/parse "``` python \n    3 + 4\n```")
         (d/document
          (d/code-block
           "python"
           "    3 + 4\n")))))

(deftest indented-code-blocks
  (is (= (p/parse "    var a := 3 + 4;\n    print(a);")
         (d/document
          (d/code-block ""
           "var a := 3 + 4;\nprint(a);")))))

(deftest code-spans
  (is (= (p/parse "`foo`")
         (d/document
          (d/paragraph (d/code-span "foo")))))
  (is (= (p/parse "``foo``")
         (d/document
          (d/paragraph (d/code-span "foo")))))
  (is (= (p/parse "``foo`bar``")
         (d/document
          (d/paragraph (d/code-span "foo`bar")))))
  (is (= (p/parse "``foo`") ; The backticks don't match so it's just text
         (d/document
          (d/paragraph (d/text "``")
                       (d/text "foo")
                       (d/text "`")))))
  (is (= (p/parse "`` foo ` bar ``")
         (d/document
          (d/paragraph (d/code-span "foo ` bar")))))
  (is (= (p/parse "`  ``  `")
         (d/document
          (d/paragraph (d/code-span " `` "))))))

(deftest link
  (is (= (p/parse "[test](http://url.com)")
         (d/document
          (d/paragraph
           (d/link [(d/text "test")] "http://url.com")))))
  (is (= (p/parse "[link con `código` adentro](http://url.com)")
         (d/document
          (d/paragraph
           (d/link [(d/text "link con ")
                    (d/code-span "código")
                    (d/text " adentro")] 
                   "http://url.com")))))
  (is (= (p/parse "Probando un texto con un link en la misma línea: [test](http://url.com)")
         (d/document
          (d/paragraph
           (d/text "Probando un texto con un link en la misma línea: ")
           (d/link [(d/text "test")] "http://url.com")))))
  (is (= (p/parse "# Link in heading [test](http://url.com) #######")
         (d/document
          (d/heading
           1
           (d/text "Link in heading ")
           (d/link [(d/text "test")] "http://url.com")))))
  (is (= (p/parse "[invalid[link](test)")
         (d/document
          (d/paragraph
           (d/text "[invalid")
           (d/link [(d/text "link")] "test"))))))

(deftest paragraph-with-code
  (is (= (p/parse "(println 3 4)\n(+ 3 4)\n\nTest")
         (d/document
          (d/paragraph (d/clojure '(println 3 4) nil)
                       (d/clojure '(+ 3 4) 7))
          (d/paragraph (d/text "Test")))))
  (is (= (p/parse "[:div (+ 3 4)]\n\nTest")
         (d/document
          (d/paragraph (d/clojure '[:div (+ 3 4)]
                                  (h.c/normalize-element [:div 7])))
          (d/paragraph (d/text "Test"))))))

(deftest code-inside-heading
  (is (= (p/parse "# Richo (+ 3 4) capo")
         (d/document
          (d/heading 1
                     (d/text "Richo ")
                     (d/clojure '(+ 3 4) 7)
                     (d/text " capo")))))
  (is (= (p/parse "(def test (atom 42))\n# Richo (do @test) capo")
         (d/document
          (d/paragraph (d/clojure '(def test (atom 42)) nil))
          (d/heading 1
                     (d/text "Richo ")
                     (d/clojure '(do @test) 42)
                     (d/text " capo"))))))

(deftest code-inside-text
  (is (= (p/parse "Richo (+ 3 4) capo")
         (d/document
          (d/paragraph
           (d/text "Richo ")
           (d/clojure '(+ 3 4) 7)
           (d/text " capo")))))
  (is (= (p/parse "(def test (atom 42))\n(loop [] (when (pos? @test) (swap! test dec) (recur)))\n\nRicho (do @test) [1 2 3] capo")
         (d/document
          (d/paragraph
           (d/clojure '(def test (atom 42)) nil)
           (d/clojure '(loop [] (when (pos? @test) (swap! test dec) (recur))) nil))
          (d/paragraph
           (d/text "Richo ")
           (d/clojure '(do @test) 0)
           (d/text " [1 2 3] capo"))))))

(deftest clojure-with-blank-lines-in-between
  (is (= (p/parse "(+ 3\n\n\n4)")
         (d/document
          (d/paragraph (d/clojure '(+ 3 4) 7))))))

(deftest code-blocks-with-many-lines
  (is (= (p/parse "```python\nwheelL.setVelocity(MAX_VEL)\nwheelR.setVelocity(MAX_VEL)\n```")
         (d/document
          (d/code-block "python"
                        "wheelL.setVelocity(MAX_VEL)\nwheelR.setVelocity(MAX_VEL)\n")))))

(deftest clojure-code-can-be-escaped
  (is (= (p/parse "Esto no es clojure: \\(+ 3 4)")
         (d/document
          (d/paragraph (d/text "Esto no es clojure: ")
                       (d/text "(+ 3 4)"))))))

(deftest code-spans-should-not-extend-beyond-paragraph
  (is (= (p/parse "había una vez ``asdf\n\n\nghi``")
         (d/document
          (d/paragraph (d/text "había una vez ")
                       (d/text "``")
                       (d/text "asdf"))
          (d/paragraph (d/text "ghi")
                       (d/text "``"))))))

(deftest code-spans-edge-cases
  (is (= (p/parse "``foo\n---``")
         (d/document
          (d/paragraph (d/code-span "foo ---")))))
  (is (= (p/parse "``foo\n---\n``")
         (d/document
          (d/heading 2 
                     (d/text "``")
                     (d/text "foo"))
          (d/paragraph (d/text "``")))))
  (is (= (p/parse "``foo\n---\n``bar``")
         (d/document
          (d/heading 2
                     (d/text "``")
                     (d/text "foo"))
          (d/paragraph (d/code-span "bar")))))
  (is (= (p/parse "``\nfoo\nbar  \nbaz\n``")
         (d/document
          (d/paragraph (d/code-span "foo bar   baz")))))
  (is (= (p/parse "`foo\n    bar`")
         (d/document
          (d/paragraph (d/code-span "foo bar"))))))

(comment
  (p/parse "(+ 3 4)")
  
  (tap> *1)

  (def test (atom 42))
  (loop [] (when (pos? @test) (swap! test dec) (recur)))
  @test
  

  )