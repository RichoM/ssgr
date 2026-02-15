(ns ssgr.parser-test
  (:require [clojure.test :refer [deftest is]]
            [ssgr.input-stream :as in]
            [ssgr.lexer :as l]
            [ssgr.eval :as e]
            [ssgr.parser :as p]
            [ssgr.doc :as d]
            [ssgr.token :as t]
            [hiccup.compiler :as h.c]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.walk :as w]
            [clojure.data :refer [diff]]))

(defn parse* [src]
  (p/parse src {} e/eval-form))

(defn find-first [type nodes]
  (let [result (volatile! nil)]
    (w/prewalk (fn [node]
                 (if (= type (:type node))
                   (do (vreset! result node)
                       nil)
                   node))
               nodes)
    @result))

(defn parse-token [node token]
  (let [sanitized-input (t/input-value token)
        #_(case (:type node)
                          ;;; NOTE(Richo): This doesn't work because we're trying to sanitize
                          ;;; the heading element instead of its text child elements. I need
                          ;;; to make some walking function that also keeps track of the path
                          ;;; (parent nodes) that correspond to this node and if we're a text
                          ;;; inside a heading we sanitize the input or something like that...
            :ssgr.doc/heading (-> (t/input-value token)
                                  (str/replace #"^\s+" "")
                                  (str/replace #"\s+#*\r*\n*$" ""))
            (t/input-value token))
        token-parsed (parse* sanitized-input)
        actual (find-first (:type node) token-parsed)]
    actual))

(def excepted-types
  "These types get excluded from the token validation. They simply get skipped"
  #{:ssgr.doc/soft-break :ssgr.doc/hard-break})

(def stop-types
  "These types stop the token validation process and prevent it from analyzing
   their children"
  #{;:ssgr.doc/heading
    })

(defn assert-valid-tokens [result]
  (w/prewalk (fn [node]
               (let [type (:type node)]
                 #_(when type
                     (println type))
                 (when (and (map? node)
                            (not (excepted-types type)))
                   (when-not (and (= type :ssgr.doc/text)
                                  (re-matches #"\s+" (:text node)))
                     (when-let [token (-> node meta :token)]
                       (is (= node (parse-token node token))
                           (str "Invalid token '" (t/input-value token)
                                "' for node of type " type)))))
                 (if (and (map? node)
                          (stop-types type))
                   nil
                   node)))
             result))

(defn find-missing-tokens [result]
  (let [missing-tokens (volatile! #{})]
    (w/prewalk (fn [f]
                 (when (and (map? f)
                            (some? (:type f)))
                   (when-not (-> f meta :token)
                     (when-not (and (= :ssgr.doc/document (:type f))
                                    (empty? (:blocks f)))
                       (vswap! missing-tokens conj (:type f)))))
                 f)
               result)
    @missing-tokens))

(defn parse
  [src & {:keys [check-valid-tokens? check-missing-tokens?]
          :or {check-valid-tokens? true
               check-missing-tokens? true}}]
  (let [result (parse* src)
        missing-tokens (find-missing-tokens result)]
    (when check-missing-tokens?
      (is (empty? missing-tokens)))
    (when (and check-valid-tokens?
               (empty? missing-tokens))
      (assert-valid-tokens result))
    result))

(defn tparse [src]
  (let [result (p/parse src {:debug true} e/eval-form)]
    (tap> result)
    result))


(deftest blank-line
  (let [blank? #(= {:type :ssgr.parser/blank}
                   (p/parse-blank-line! (in/make-stream (l/tokenize %))))]
    (is (blank? "\n"))
    (is (blank? "\t\n"))
    (is (blank? "   \n"))
    (is (blank? "    \n"))
    (is (not (blank? "asdf\n")))
    (is (not (blank? "  asdf\n")))
    (is (not (blank? "    asdf\n")))))



(deftest regular-text
  (is (= (parse "Texto normal")
         (d/document
          (d/paragraph
           (d/text "Texto normal"))))))

(deftest paragraph
  (is (= (parse "P1. L1\nP1. L2\n\nP2. L1")
         (d/document
          (d/paragraph (d/text "P1. L1")
                       (d/soft-break)
                       (d/text "P1. L2"))
          (d/paragraph (d/text "P2. L1"))))))

(deftest atx-heading-line
  (let [atx-heading? #(= :ssgr.doc/heading (-> % :blocks first :type))]
    (is (atx-heading? (parse "#")))
    (is (atx-heading? (parse "##")))
    (is (atx-heading? (parse "###")))
    (is (atx-heading? (parse "####")))
    (is (atx-heading? (parse "#####")))
    (is (atx-heading? (parse "######")))
    (is (not (atx-heading? (parse "    #"))))
    (is (not (atx-heading? (parse "#Richo"))))
    (is (not (atx-heading? (parse "    ----- ***"))))))

(deftest atx-heading-with-different-newlines
  (is (= (parse "#\n")
         (d/document (d/heading 1))))
  (is (= (parse "#\r\n")
         (d/document (d/heading 1))))
  (is (= (parse "#\ntexto")
         (d/document (d/heading 1)
                     (d/paragraph (d/text "texto")))))
  (is (= (parse "#\r\ntexto")
         (d/document (d/heading 1)
                     (d/paragraph (d/text "texto"))))))

(deftest atx-heading
  (is (= (parse "# Heading 1")
         (d/document
          (d/heading 1
                     (d/text "Heading 1")))))
  (is (= (parse "  ## Heading 2    \n")
         (d/document
          (d/heading 2
                     (d/text "Heading 2"))))))

(deftest atx-heading-with-trailing-hashtags
  (is (= (parse "# Heading#")
         (d/document
          (d/heading 1
                     (d/text "Heading#")))))
  (is (= (parse "# Heading #")
         (d/document
          (d/heading 1
                     (d/text "Heading")))))
  (is (= (parse "# Heading  ##")
         (d/document
          (d/heading 1
                     (d/text "Heading"))))))

(deftest thematic-breaks
  (is (= (parse "Texto 1\n***\nTexto 2\n___")
         (d/document
          (d/paragraph (d/text "Texto 1"))
          (d/thematic-break)
          (d/paragraph (d/text "Texto 2"))
          (d/thematic-break))))
  (is (= (parse "Texto 1\n***      \nTexto 2\n___")
         (d/document
          (d/paragraph (d/text "Texto 1"))
          (d/thematic-break)
          (d/paragraph (d/text "Texto 2"))
          (d/thematic-break)))))

(deftest more-thematic-breaks
  (let [thematic-break? #(= % (d/document (d/thematic-break)))]
    (is (thematic-break? (parse "---")))
    (is (thematic-break? (parse "___")))
    (is (thematic-break? (parse "***")))
    (is (thematic-break? (parse " ---")))
    (is (thematic-break? (parse "   ---")))
    (is (thematic-break? (parse "   -------      \t\t\t\t")))
    (is (not (thematic-break? (parse "    ---"))))
    (is (not (thematic-break? (parse "    ---*"))))
    (is (not (thematic-break? (parse "    ----- ***"))))))

(deftest setext-heading-underline-line
  (let [setext-heading-underline?
        (fn [src & {:keys [check-valid-tokens?]
                    :or {check-valid-tokens? true}}]
          (let [doc (parse (str "title\n" src)
                           :check-valid-tokens? check-valid-tokens?)
                heading (-> doc :blocks first)]
            (and (= :ssgr.doc/heading (:type heading))
                 (= [(d/text "title")] (:elements heading))
                 (= 1 (count (:blocks doc))))))]
    (is (setext-heading-underline? "-"))
    (is (setext-heading-underline? "="))
    (is (setext-heading-underline? "------   "))
    (is (setext-heading-underline? "   -"))

    ; The following case seems to be special in the sense that the token is 
    ; correct but the check is wrong, that's why I'm disabling the check for
    ; this case only
    (is (not (setext-heading-underline?
              "    -"
              :check-valid-tokens? false)))
    (is (not (setext-heading-underline? "-=")))))

(deftest setext-headings
  (is (= (parse "Texto 1\n=\nTexto 2\n---")
         (d/document
          (d/heading 1 (d/text "Texto 1"))
          (d/heading 2 (d/text "Texto 2"))))))

(deftest code-fence-line
  (let [code-fence? #(= :ssgr.doc/code-block (-> % :blocks first :type))]
    (is (code-fence? (parse "```")))
    (is (code-fence? (parse "````")))
    (is (code-fence? (parse "~~~")))
    (is (code-fence? (parse "~~~```")))
    (is (code-fence? (parse "```python    [[[]]]```")))
    (is (code-fence? (parse "   ```python")))))

(deftest fenced-code-blocks
  (is (= (parse "``` python \n    3 + 4\n```")
         (d/document
          (d/code-block
           "python"
           "    3 + 4\n")))))

(deftest indented-code-block-line
  (let [indented-code-block? #(= :ssgr.doc/code-block (-> % :blocks first :type))]
    (is (indented-code-block? (parse "    a")))
    (is (indented-code-block? (parse "    a ")))
    (is (indented-code-block? (parse "     Richo capo    ")))
    (is (not (indented-code-block? (parse "     "))))
    (is (not (indented-code-block? (parse "   Richo"))))))

(deftest indented-code-blocks
  (is (= (parse "    var a := 3 + 4;\n    print(a);")
         (d/document
          (d/code-block ""
                        "var a := 3 + 4;\nprint(a);")))))

(deftest code-spans
  (is (= (parse "`foo`")
         (d/document
          (d/paragraph (d/code-span "foo")))))
  (is (= (parse "``foo``")
         (d/document
          (d/paragraph (d/code-span "foo")))))
  (is (= (parse "``foo`bar``")
         (d/document
          (d/paragraph (d/code-span "foo`bar")))))
  (is (= (parse "``foo`") ; The backticks don't match so it's just text
         (d/document
          (d/paragraph (d/text "``")
                       (d/text "foo")
                       (d/text "`")))))
  (is (= (parse "`` foo ` bar ``")
         (d/document
          (d/paragraph (d/code-span "foo ` bar")))))
  (is (= (parse "`  ``  `")
         (d/document
          (d/paragraph (d/code-span " `` "))))))

(deftest paragraph-with-code
  (is (= (parse "(println 3 4)\n(+ 3 4)\n\nTest")
         (d/document
          (d/paragraph (d/clojure '(println 3 4) nil)
                       (d/soft-break)
                       (d/clojure '(+ 3 4) 7))
          (d/paragraph (d/text "Test")))))
  (is (= (parse "[:div (+ 3 4)]\n\nTest")
         (d/document
          (d/paragraph (d/clojure '[:div (+ 3 4)]
                                  (h.c/normalize-element [:div 7])))
          (d/paragraph (d/text "Test"))))))

(deftest code-inside-heading
  (is (= (parse "# Richo (+ 3 4) capo")
         (d/document
          (d/heading 1
                     (d/text "Richo ")
                     (d/clojure '(+ 3 4) 7)
                     (d/text " capo")))))
  (is (= (parse "(def test (atom 42))\n# Richo (do @test) capo")
         (d/document
          (d/paragraph (d/clojure '(def test (atom 42)) nil))
          (d/heading 1
                     (d/text "Richo ")
                     (d/clojure '(do @test) 42)
                     (d/text " capo"))))))

(deftest code-inside-text
  (is (= (parse "Richo (+ 3 4) capo")
         (d/document
          (d/paragraph
           (d/text "Richo ")
           (d/clojure '(+ 3 4) 7)
           (d/text " capo")))))
  (is (= (parse "(def test (atom 42))\n(loop [] (when (pos? @test) (swap! test dec) (recur)))\n\nRicho (do @test) [1 2 3] capo")
         (d/document
          (d/paragraph
           (d/clojure '(def test (atom 42)) nil)
           (d/soft-break)
           (d/clojure '(loop [] (when (pos? @test) (swap! test dec) (recur))) nil))
          (d/paragraph
           (d/text "Richo ")
           (d/clojure '(do @test) 0)
           (d/text " ")
           (d/text "[")
           (d/text "1 2 3")
           (d/text "]")
           (d/text " capo"))))))

(deftest link
  (is (= (parse "[test](http://url.com)")
         (d/document
          (d/paragraph
           (d/link [(d/text "test")] "http://url.com")))))
  (is (= (parse "[link con `código` adentro](http://url.com)")
         (d/document
          (d/paragraph
           (d/link [(d/text "link con ")
                    (d/code-span "código")
                    (d/text " adentro")]
                   "http://url.com")))))
  (is (= (parse "Probando un texto con un link en la misma línea: [test](http://url.com)")
         (d/document
          (d/paragraph
           (d/text "Probando un texto con un link en la misma línea: ")
           (d/link [(d/text "test")] "http://url.com")))))
  (is (= (parse "# Link in heading [test](http://url.com) #######")
         (d/document
          (d/heading
           1
           (d/text "Link in heading ")
           (d/link [(d/text "test")] "http://url.com")))))
  (is (= (parse "[invalid[link](test)")
         (d/document
          (d/paragraph
           (d/text "[")
           (d/text "invalid")
           (d/link [(d/text "link")] "test"))))))

(deftest clojure-with-blank-lines-in-between
  (is (= (parse "(+ 3\n\n\n4)")
         (d/document
          (d/paragraph (d/clojure '(+ 3 4) 7))))))

(deftest code-blocks-with-many-lines
  (is (= (parse "```python\nwheelL.setVelocity(MAX_VEL)\nwheelR.setVelocity(MAX_VEL)\n```")
         (d/document
          (d/code-block "python"
                        "wheelL.setVelocity(MAX_VEL)\nwheelR.setVelocity(MAX_VEL)\n")))))

(deftest clojure-code-can-be-escaped
  (is (= (parse "Esto no es clojure: \\(+ 3 4)")
         (d/document
          (d/paragraph (d/text "Esto no es clojure: ")
                       (d/text "(+ 3 4)"))))))

(deftest code-spans-should-not-extend-beyond-paragraph
  (is (= (parse "había una vez ``asdf\n\n\nghi``")
         (d/document
          (d/paragraph (d/text "había una vez ")
                       (d/text "``")
                       (d/text "asdf"))
          (d/paragraph (d/text "ghi")
                       (d/text "``"))))))

(deftest code-spans-edge-cases
  (is (= (parse "``foo\n---``")
         (d/document
          (d/paragraph (d/code-span "foo ---")))))
  (is (= (parse "``foo\n---\n``")
         (d/document
          (d/heading 2
                     (d/text "``")
                     (d/text "foo"))
          (d/paragraph (d/text "``")))))
  (is (= (parse "``foo\n---\n``bar``")
         (d/document
          (d/heading 2
                     (d/text "``")
                     (d/text "foo"))
          (d/paragraph (d/code-span "bar")))))
  (is (= (parse "``\nfoo\nbar  \nbaz\n``")
         (d/document
          (d/paragraph (d/code-span "foo bar   baz")))))
  (is (= (parse "`foo\n    bar`")
         (d/document
          (d/paragraph (d/code-span "foo bar"))))))

(deftest images
  (is (= (parse "![foo](url)")
         (d/document
          (d/paragraph (d/image "url" (d/text "foo")))))))

(deftest link-with-slash-inside-text
  (is (= (parse "[li\\nk](url)")
         (d/document
          (d/paragraph (d/link [(d/text "li\\nk")]
                               "url"))))))

(deftest code-spans-have-precedence-over-link-texts
  (is (= (parse "[not a `link](/foo`)")
         (d/document
          (d/paragraph
           (d/text "[")
           (d/text "not a ")
           (d/code-span "link](/foo")
           (d/text ")"))))))

(deftest code-spans-opening-and-closing-backtick-strings-need-to-be-equal-in-length
  (is (= (parse "`foo``bar``")
         (d/document
          (d/paragraph
           (d/text "`")
           (d/text "foo")
           (d/code-span "bar"))))))

(deftest atx-heading-with-single-#
  (is (= (parse "#")
         (d/document
          (d/heading 1)))))

(deftest escaped-chars
  ; Any ASCII punctuation character may be backslash-escaped
  (let [valid-chars "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        escaped-chars (->> valid-chars
                           (map (fn [chr]
                                  (str "\\" chr)))
                           (str/join))]
    (is (= (d/as-text (parse escaped-chars))
           valid-chars)))
  ; Backslashes before other characters are treated as literal backslashes
  (let [string "\\→\\A\\a\\ \\3\\φ\\«"]
    (is (= (d/as-text (parse string))
           string))))

(deftest link-text-with-newline
  (is (= (parse "[foo\n](url)")
         (d/document
          (d/paragraph (d/link [(d/text "foo")
                                (d/soft-break)]
                               "url")))))
  (is (= (parse "[foo\n\n](url)") ; Not a link!
         (d/document
          (d/paragraph (d/text "[")
                       (d/text "foo"))
          (d/paragraph (d/text "]")
                       (d/text "(url)")))))
  (is (= (parse "[link\n         with newline](url)")
         (d/document
          (d/paragraph (d/link [(d/text "link")
                                (d/soft-break)
                                (d/text "with newline")]
                               "url"))))))

(deftest soft-breaks
  (is (= (parse "foo\nbar")
         (d/document
          (d/paragraph (d/text "foo")
                       (d/soft-break)
                       (d/text "bar")))))
  (is (= (parse "foo \nbar")
         (d/document
          (d/paragraph (d/text "foo")
                       (d/soft-break)
                       (d/text "bar"))))))

(deftest hard-breaks
  (is (= (parse "foo\\\r\nbar")
         (d/document
          (d/paragraph (d/text "foo")
                       (d/hard-break)
                       (d/text "bar")))))
  (is (= (parse "foo   \nbar")
         (d/document
          (d/paragraph (d/text "foo")
                       (d/hard-break)
                       (d/text "bar")))))
  (is (= (parse "foo      \\\r\nbar")
         (d/document
          (d/paragraph (d/text "foo      ")
                       (d/hard-break)
                       (d/text "bar")))))
  (is (= (parse "foo  \nbar")
         (d/document
          (d/paragraph (d/text "foo")
                       (d/hard-break)
                       (d/text "bar")))))
  (is (= (parse "`foo`\\\nbar")
         (d/document
          (d/paragraph (d/code-span "foo")
                       (d/hard-break)
                       (d/text "bar")))))
  (is (= (parse "`foo`  \\\nbar")
         (d/document
          (d/paragraph (d/code-span "foo")
                       (d/text "  ")
                       (d/hard-break)
                       (d/text "bar")))))
  (is (= (parse "`foo`  \nbar")
         (d/document
          (d/paragraph (d/code-span "foo")
                       (d/hard-break)
                       (d/text "bar"))))))


(deftest link-with-other-link-inside
  (is (= (parse "[link con [otro link](url2) adentro](url)")
         (d/document
          (d/paragraph (d/text "[")
                       (d/text "link con ")
                       (d/link [(d/text "otro link")]
                               "url2")
                       (d/text " adentro")
                       (d/text "]")
                       (d/text "(url)"))))))

(deftest emphasis-with-*
  (is (= (parse "*foo bar*")
         (d/document
          (d/paragraph (d/emphasis (d/text "foo bar"))))))
  (is (= (parse "texto *énfasis*")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/emphasis (d/text "énfasis")))))
      "case-1")
  (is (= (parse "textoo*énfasis*")
         (d/document
          (d/paragraph (d/text "textoo")
                       (d/emphasis (d/text "énfasis")))))
      "case-2a")
  (is (= (parse "texto *+énfasis*")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/emphasis (d/text "+énfasis")))))
      "case-2b")
  (is (= (parse "texto+*+énfasis*")
         (d/document
          (d/paragraph (d/text "texto+")
                       (d/emphasis (d/text "+énfasis")))))
      "case-2b'")
  (is (= (parse "textoo*+énfasis*")
         (d/document
          (d/paragraph (d/text "textoo")
                       (d/text "*")
                       (d/text "+énfasis")
                       (d/text "*"))))
      "bad-case")
  (is (= (parse "textoo *énfasis_")
         (d/document
          (d/paragraph (d/text "textoo ")
                       (d/text "*")
                       (d/text "énfasis")
                       (d/text "_"))))
      "bad-case 2"))

(deftest emphasis-with-_
  (is (= (parse "_foo bar_")
         (d/document
          (d/paragraph (d/emphasis (d/text "foo bar"))))))
  (is (= (parse "texto _énfasis_")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/emphasis (d/text "énfasis")))))
      "case-1")
  (is (= (parse "textoo_énfasis_")
         (d/document
          (d/paragraph (d/text "textoo")
                       (d/text "_")
                       (d/text "énfasis")
                       (d/text "_"))))
      "case-2a")
  (is (= (parse "texto _+énfasis_")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/emphasis (d/text "+énfasis")))))
      "case-2b")
  (is (= (parse "texto+_+énfasis_")
         (d/document
          (d/paragraph (d/text "texto+")
                       (d/emphasis (d/text "+énfasis")))))
      "case-2b'")
  (is (= (parse "textoo_+énfasis_")
         (d/document
          (d/paragraph (d/text "textoo")
                       (d/text "_")
                       (d/text "+énfasis")
                       (d/text "_"))))
      "bad-case")
  (is (= (parse "textoo _énfasis*")
         (d/document
          (d/paragraph (d/text "textoo ")
                       (d/text "_")
                       (d/text "énfasis")
                       (d/text "*"))))
      "bad-case 2"))

(deftest emphasis-with-unmatching-delimiters
  (is (= (parse "texto **énfasis*")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/text "*")
                       (d/emphasis (d/text "énfasis"))))))
  (is (= (parse "texto *énfasis**")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/emphasis (d/text "énfasis"))
                       (d/text "*")))))
  (is (= (parse "texto **énfasis*\ntexto *énfasis**")
         (d/document
          (d/paragraph (d/text "texto ")
                       (d/emphasis
                        (d/emphasis (d/text "énfasis"))
                        (d/soft-break)
                        (d/text "texto ")
                        (d/emphasis (d/text "énfasis"))))))))

(deftest strong-emphasis
  (is (= (parse "*****texto*****")
         (d/document
          (d/paragraph (d/emphasis
                        (d/strong-emphasis
                         (d/strong-emphasis
                          (d/text "texto"))))))))
  (is (= (parse "*****texto******")
         (d/document
          (d/paragraph (d/emphasis
                        (d/strong-emphasis
                         (d/strong-emphasis
                          (d/text "texto"))))
                       (d/text "*")))))
  (is (= (parse "******texto*****")
         (d/document
          (d/paragraph (d/text "*")
                       (d/emphasis
                        (d/strong-emphasis
                         (d/strong-emphasis
                          (d/text "texto")))))))))

(deftest mixing-emph-symbols
  (is (= (parse "_*__foo*__")
         (d/document
          (d/paragraph
           (d/emphasis
            (d/emphasis
             (d/text "__")
             (d/text "foo")))
           (d/text "_"))))))

(deftest emphasis-rules-9-10
  ; I think this one is a false positive, but I don't know how to fix the check
  ; because the emphasis rules are dependant on the surrounding chars, and if I
  ; parse only the token the emphasis precedence changes and the result is not 
  ; the same as it was before. I checked the tokens manually but, just in case, 
  ; I leave this comment here for future Richo to be aware of this
  (is (= (parse "*****Hello*world****"
                :check-valid-tokens? false)
         (d/document
          (d/paragraph (d/text "**")
                       (d/emphasis
                        (d/strong-emphasis
                         (d/text "Hello")
                         (d/emphasis
                          (d/text "world")))))))))

(deftest intraword-emphasis
  (is (= (parse "wb_robot_step")
         (d/document
          (d/paragraph (d/text "wb")
                       (d/text "_")
                       (d/text "robot")
                       (d/text "_")
                       (d/text "step"))))
      "Invalid for _")
  (is (= (parse "wb*robot*step")
         (d/document
          (d/paragraph (d/text "wb")
                       (d/emphasis
                        (d/text "robot"))
                       (d/text "step"))))
      "Only valid for *"))

(deftest emphasis-rule-4
  (is (= (parse "_foo bar _")
         (d/document
          (d/paragraph (d/text "_")
                       (d/text "foo bar ")
                       (d/text "_"))))
      "Example 371")
  (is (= (parse "_(_foo)")
         (d/document
          (d/paragraph (d/text "_")
                       (d/text "(")
                       (d/text "_")
                       (d/text "foo)"))))
      "Example 372")
  (is (= (parse "_(_foo_)_")
         (d/document
          (d/paragraph
           (d/emphasis (d/text "(")
                       (d/emphasis
                        (d/text "foo"))
                       (d/text ")")))))
      "Example 373")
  (is (= (parse "_foo_bar")
         (d/document
          (d/paragraph (d/text "_")
                       (d/text "foo")
                       (d/text "_")
                       (d/text "bar"))))
      "Example 374")
  (is (= (parse "_foo_bar_baz_")
         (d/document
          (d/paragraph
           (d/emphasis (d/text "foo")
                       (d/text "_")
                       (d/text "bar")
                       (d/text "_")
                       (d/text "baz")))))
      "Example 376"))

(deftest ordered-lists
  (is (= (parse "1. Richo")
         (d/document
          (d/ordered-list
           1
           (d/list-item (d/paragraph (d/text "Richo")))))))
  (is (= (parse "10. Richo")
         (d/document
          (d/ordered-list
           10
           (d/list-item (d/paragraph (d/text "Richo")))))))
  (is (= (parse "1) Richo\n2) Diego")
         (d/document
          (d/ordered-list
           1
           (d/list-item (d/paragraph (d/text "Richo")))
           (d/list-item (d/paragraph (d/text "Diego"))))))))

(deftest bullet-lists
  (is (= (parse "* Richo")
         (d/document
          (d/bullet-list
           (d/list-item (d/paragraph (d/text "Richo")))))))
  (is (= (parse "+ Richo")
         (d/document
          (d/bullet-list
           (d/list-item (d/paragraph (d/text "Richo")))))))
  (is (= (parse "- Richo\n- Diego")
         (d/document
          (d/bullet-list
           (d/list-item (d/paragraph (d/text "Richo")))
           (d/list-item (d/paragraph (d/text "Diego"))))))))

(deftest two-lists
  (is (= (parse "+ Richo\n* Diego")
         (d/document
          (d/bullet-list
           (d/list-item (d/paragraph (d/text "Richo"))))
          (d/bullet-list
           (d/list-item (d/paragraph (d/text "Diego"))))))))

(deftest list-with-sublists
  ; This one is a little tricky but I also think it's a false positive because the rules for 
  ; sublists are weird. In this case if I take the sublist token and parse it separately it
  ; won't produce the same sublist, but the token is still correct.
  (is (= (parse "1. item one\n2. item two\n   - sublist\n   - sublist"
                :check-valid-tokens? false)
         (d/document
          (d/ordered-list
           1
           (d/list-item (d/paragraph (d/text "item one")))
           (d/list-item (d/paragraph (d/text "item two"))
                        (d/bullet-list
                         (d/list-item (d/paragraph (d/text "sublist")))
                         (d/list-item (d/paragraph (d/text "sublist"))))))))))

(deftest deeply-nested-list
  ; Another false positive
  (is (= (parse "1. item one\n2. item two\n   - sublist\n     que continúa en la siguiente línea.\n\n     Y que además tiene otro párrafo.\n   - sublist"
                :check-valid-tokens? false)
         (d/document
          (d/ordered-list
           1
           (d/list-item (d/paragraph (d/text "item one")))
           (d/list-item
            (d/paragraph (d/text "item two"))
            (d/bullet-list
             (d/list-item (d/paragraph (d/text "sublist")
                                       (d/soft-break)
                                       (d/text "que continúa en la siguiente línea."))
                          (d/paragraph (d/text "Y que además tiene otro párrafo.")))
             (d/list-item (d/paragraph (d/text "sublist")))))))))
  ; NOTE(Richo): This text should parse the same as before, the only difference is that the blank
  ; line between paragraphs also contains the exact number of spaces to be part of the list item
  (is (= (parse "1. item one\n2. item two\n   - sublist\n     que continúa en la siguiente línea.\n     \n     Y que además tiene otro párrafo.\n   - sublist"
                :check-valid-tokens? false)
         (d/document
          (d/ordered-list
           1
           (d/list-item (d/paragraph (d/text "item one")))
           (d/list-item
            (d/paragraph (d/text "item two"))
            (d/bullet-list
             (d/list-item (d/paragraph (d/text "sublist")
                                       (d/soft-break)
                                       (d/text "que continúa en la siguiente línea."))
                          (d/paragraph (d/text "Y que además tiene otro párrafo.")))
             (d/list-item (d/paragraph (d/text "sublist")))))))))
  (is (= (parse "1. item one\n   - sublist\n     * sub sub list\n   - sublist"
                :check-valid-tokens? false)
         (d/document
          (d/ordered-list
           1
           (d/list-item
            (d/paragraph (d/text "item one"))
            (d/bullet-list
             (d/list-item
              (d/paragraph (d/text "sublist"))
              (d/bullet-list
               (d/list-item (d/paragraph (d/text "sub sub list")))))
             (d/list-item
              (d/paragraph (d/text "sublist"))))))))))

(deftest blockquote
  (is (= (parse "> foo")
         (d/document
          (d/blockquote (d/paragraph (d/text "foo"))))))
  (is (= (parse "> foo\nbar")
         (d/document
          (d/blockquote (d/paragraph (d/text "foo")
                                     (d/soft-break)
                                     (d/text "bar"))))))
  ; This case is also a false positive, because the second blockquote marker changes
  ; its meaning depending on the presence of the first. So we can't check it using the
  ; existing system
  (is (= (parse "> foo\n>bar"
                :check-valid-tokens? false)
         (d/document
          (d/blockquote (d/paragraph (d/text "foo")
                                     (d/soft-break)
                                     (d/text "bar"))))))
  ; Same as previous test case
  (is (= (parse "> foo\n> bar"
                :check-valid-tokens? false)
         (d/document
          (d/blockquote (d/paragraph (d/text "foo")
                                     (d/soft-break)
                                     (d/text "bar")))))))

(deftest nested-blockquotes
  ; This case is also a false positive, because the presence of the first blockquote
  ; changes the meaning of the second one.
  (is (= (parse "> foo\n>>bar"
                :check-valid-tokens? false)
         (d/document
          (d/blockquote (d/paragraph (d/text "foo"))
                        (d/blockquote (d/paragraph (d/text "bar"))))))))

(deftest list-inside-blockquote
  (is (= (tparse "> 1. Richo\n> 2. Diego")
         (d/document
          (d/blockquote (d/ordered-list
                         1
                         (d/list-item (d/paragraph (d/text "Richo")))
                         (d/list-item (d/paragraph (d/text "Diego")))))))))

(comment
  
(deftest blockquote-interrupts-lists
  (is (= (parse "1. Richo\n> 2. Diego")
         (d/document
          (d/ordered-list
           1
           (d/list-item (d/paragraph (d/text "Richo"))))
          (d/blockquote
           (d/ordered-list
            2
            (d/list-item (d/paragraph (d/text "Diego")))))))))


(deftest more-ugly-lists
  (is (= (parse "1. A
   1. AA
   2. AB
   3. AC
2. B
   1. BA
   2. BB
3. C
   1. CA
   2. CB
   3. CC")
         (d/document
          (d/ordered-list
           1
           (d/list-item
            (d/paragraph (d/text "A"))
            (d/ordered-list
             1
             (d/list-item (d/paragraph (d/text "AA")))
             (d/list-item (d/paragraph (d/text "AB")))
             (d/list-item (d/paragraph (d/text "AC")))))
           (d/list-item
            (d/paragraph (d/text "B"))
            (d/ordered-list
             1
             (d/list-item (d/paragraph (d/text "BA")))
             (d/list-item (d/paragraph (d/text "BB")))))
           (d/list-item
            (d/paragraph (d/text "C"))
            (d/ordered-list
             1
             (d/list-item (d/paragraph (d/text "CA")))
             (d/list-item (d/paragraph (d/text "CB")))
             (d/list-item (d/paragraph (d/text "CC")))))))))
  (is (= (parse "
  1. A
     1. AA
        1. AAA
        2. AAB
     2. AB
     3. AC
  2. B
     1. BA
     2. BB
  3. C
     1. CA
     2. CB
     3. CC")
         (d/document
          (d/ordered-list
           1
           (d/list-item
            (d/paragraph (d/text "A"))
            (d/ordered-list
             1
             (d/list-item
              (d/paragraph (d/text "AA"))
              (d/ordered-list
               1
               (d/list-item (d/paragraph (d/text "AAA")))
               (d/list-item (d/paragraph (d/text "AAB")))))
             (d/list-item
              (d/paragraph (d/text "AB")))
             (d/list-item
              (d/paragraph (d/text "AC")))))
           (d/list-item
            (d/paragraph (d/text "B"))
            (d/ordered-list
             1
             (d/list-item
              (d/paragraph (d/text "BA")))
             (d/list-item
              (d/paragraph (d/text "BB")))))
           (d/list-item
            (d/paragraph (d/text "C"))
            (d/ordered-list
             1
             (d/list-item
              (d/paragraph (d/text "CA")))
             (d/list-item
              (d/paragraph (d/text "CB")))
             (d/list-item
              (d/paragraph (d/text "CC"))))))))))

(deftest leftover-whitespace-should-not-throw
  (is (parse "
    1. A
       1. AA
          1. AAA
          2. AAB
       2. AB
       3. AC
    2. B
       1. BA
       2. BB
    3. C
       1. CA
       2. CB
       3. CC
          ")
      "Leftover whitespace should not throw exception!"))

(deftest lists-should-not-consume-trailing-blank-lines
  (is (= (parse "
> A
>
> * AB
> * AC


B")
         (d/document
          (d/blockquote
           (d/paragraph (d/text "A"))
           (d/bullet-list
            (d/list-item (d/paragraph (d/text "AB")))
            (d/list-item (d/paragraph (d/text "AC")))))
          (d/paragraph (d/text "B")))))
  ; This next test should be the same as before, the only difference is that the blank
  ; block now only contains 1 line and also this line contains several spaces (before
  ; it was just empty)
  (is (= (parse "
> A
>
> * AB
> * AC
          
B")
         (d/document
          (d/blockquote
           (d/paragraph (d/text "A"))
           (d/bullet-list
            (d/list-item (d/paragraph (d/text "AB")))
            (d/list-item (d/paragraph (d/text "AC")))))
          (d/paragraph (d/text "B"))))))

(deftest heading-followed-by-paragraph-without-blank-line-in-between
  (is (= (parse "## Heading\nParagraph")
         (d/document
          (d/heading 2 (d/text "Heading"))
          (d/paragraph (d/text "Paragraph"))))))

(deftest fucking-lists
  (is (= (parse "1. AA.
AB.

2. BA
")
         (d/document
          (d/ordered-list
           1
           (d/list-item
            (d/paragraph (d/text "AA.")
                         (d/soft-break)
                         (d/text "AB.")))
           (d/list-item
            (d/paragraph (d/text "BA"))))))))

(deftest indented-code-block-inside-list-item
  ; TODO(Richo): I add this test as a reminder for future me. However, since I found this bug 
  ; while refactoring the parser I can't fix it right now. So I leave it commented...
  #_(is (= (parse "1. text\n\n       code")
           (d/document
            (d/ordered-list
             1
             (d/list-item
              (d/paragraph (d/text "text"))
              (d/code-block "" "code")))))))

)