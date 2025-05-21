(ns ssgr.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [ssgr.parser.core :as p]
            [ssgr.doc :as d]))

(deftest thematic-break
  (is (p/thematic-break? "---"))
  (is (p/thematic-break? "___"))
  (is (p/thematic-break? "***"))
  (is (p/thematic-break? " ---"))
  (is (p/thematic-break? "   ---"))
  (is (p/thematic-break? "   -------      \t\t\t\t"))
  (is (not (p/thematic-break? "    ---")))
  (is (not (p/thematic-break? "    ---*"))) 
  (is (not (p/thematic-break? "    ----- ***"))))

(deftest atx-heading
  (is (p/atx-heading? "#"))
  (is (p/atx-heading? "##"))
  (is (p/atx-heading? "###"))
  (is (p/atx-heading? "####"))
  (is (p/atx-heading? "#####"))
  (is (p/atx-heading? "######"))
  (is (not (p/atx-heading? "    #")))
  (is (not (p/atx-heading? "#Richo")))
  (is (not (p/atx-heading? "    ----- ***"))))

(deftest setext-heading-underline 
  (is (p/setext-heading-underline? "-"))
  (is (p/setext-heading-underline? "="))
  (is (p/setext-heading-underline? "------   "))
  (is (p/setext-heading-underline? "   -"))
  (is (not (p/setext-heading-underline? "    -")))
  (is (not (p/setext-heading-underline? "-="))))

(deftest indented-code-block
  (is (p/indented-code-block? "    a"))
  (is (p/indented-code-block? "    a "))
  (is (p/indented-code-block? "     Richo capo    "))
  (is (not (p/indented-code-block? "     ")))
  (is (not (p/indented-code-block? "   Richo"))))

(deftest code-fence
  (is (p/code-fence? "```"))
  (is (p/code-fence? "````"))
  (is (p/code-fence? "~~~"))
  (is (p/code-fence? "~~~```"))
  (is (p/code-fence? "```python    [[[]]]```"))
  (is (p/code-fence? "   ```python"))
  (is (not (p/code-fence? "    ```python"))))

(deftest blank 
  (is (p/blank? ""))
  (is (p/blank? "\t"))
  (is (p/blank? "   "))
  (is (p/blank? "    "))
  (is (not (p/blank? "asdf")))
  (is (not (p/blank? "  asdf")))
  (is (not (p/blank? "    asdf"))))

(deftest paragraph
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


(comment

  (deftest link
    (is (= (p/parse "[test](http://url.com)")
           (d/document
            (d/paragraph
             (d/link "test" "http://url.com")))))
    (is (= (p/parse "Probando un texto con un link en la misma línea: [test](http://url.com)")
           (d/document
            (d/paragraph
             (d/text "Probando un texto con un link en la misma línea: ")
             (d/link "test" "http://url.com")))))
    (is (= (p/parse "# Link in heading [test](http://url.com) #######")
           (d/document
            (d/heading
             1
             (d/text "Link in heading ")
             (d/link "test" "http://url.com"))))))

  (deftest code-blocks
    (is (= (p/parse "``` python \n    3 + 4\n```")
           (d/document
            (d/code-block
             "python"
             "    3 + 4")))))

  (deftest paragraph-with-code
    (is (= (p/parse "(println 3 4)\n(+ 3 4)\n\nTest")
           (d/document
            (d/paragraph (d/clojure '(println 3 4))
                         (d/clojure '(+ 3 4)))
            (d/paragraph (d/text "Test")))))
    (is (= (p/parse "(do @counter)\n[:div (+ a b)]\n\nTest")
           (d/document
            (d/paragraph (d/clojure '(do @counter))
                         (d/clojure '[:div (+ a b)]))
            (d/paragraph (d/text "Test"))))))

  (deftest code-inside-heading
    (is (= (p/parse "# Richo (+ 3 4) capo")
           (d/document
            (d/heading 1
                       (d/text "Richo ")
                       (d/clojure '(+ 3 4))
                       (d/text " capo")))))
    (is (= (p/parse "# Richo (do @test) [1 2 3] capo")
           (d/document
            (d/heading 1
                       (d/text "Richo ")
                       (d/clojure '(do @test))
                       (d/text " ")
                       (d/clojure '[1 2 3])
                       (d/text " capo"))))))

  (deftest code-inside-text
    (is (= (p/parse "Richo (+ 3 4) capo")
           (d/document
            (d/paragraph
             (d/text "Richo ")
             (d/clojure '(+ 3 4))
             (d/text " capo")))))
    (is (= (p/parse "Richo (do @test) [1 2 3] capo")
           (d/document
            (d/paragraph
             (d/text "Richo ")
             (d/clojure '(do @test))
             (d/text " ")
             (d/clojure '[1 2 3])
             (d/text " capo"))))))

  )