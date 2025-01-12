(ns ssgr.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [ssgr.parser :as p]))

(deftest atx-heading
  (is (= (p/parse "# Heading 1")
         (p/document
          (p/heading 1
                     (p/text "Heading 1")))))
  (is (= (p/parse "  ## Heading 2    \n")
         (p/document
          (p/heading 2
                     (p/text "Heading 2"))))))

(deftest regular-text 
  (is (= (p/parse "Texto normal")
         (p/document
          (p/paragraph
           (p/text-line "Texto normal"))))))

(deftest paragraph
  (is (= (p/parse "P1. L1\nP1. L2\n\nP2. L1")
         (p/document
          (p/paragraph (p/text-line "P1. L1")
                       (p/text-line "P1. L2"))
          (p/paragraph (p/text-line "P2. L1"))))))

(deftest paragraph-with-code
  (is (= (p/parse "(println 3 4)\n(+ 3 4)\n\nTest")
         (p/document
          (p/paragraph (p/code-line '(println 3 4))
                       (p/code-line '(+ 3 4)))
          (p/paragraph (p/text-line "Test"))))))

(deftest code-inside-heading
  (is (= (p/parse "# Richo (+ 3 4) capo")
         (p/document
          (p/heading 1 
                     (p/text "Richo")
                     (p/code '(+ 3 4))
                     (p/text "capo"))))))

(deftest code-inside-text
  (is (= (p/parse "Richo (+ 3 4) capo")
         (p/document
          (p/paragraph
           (p/line (p/text "Richo")
                   (p/code '(+ 3 4))
                   (p/text "capo")))))))