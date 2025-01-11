(ns ssgr.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [ssgr.parser :as p]))

(deftest atx-heading
  (is (= (p/parse "# Heading 1")
         [(p/heading 1 [(p/text "Heading 1")])]))
  (is (= (p/parse "  ## Heading 2    \n")
         [(p/heading 2 [(p/text "Heading 2")])])))

(deftest regular-text 
  (is (= (p/parse "Texto normal")
         [(p/paragraph [(p/text "Texto normal")])])))

(deftest paragraph
  (is (= (p/parse "P1. L1\nP1. L2\n\nP2. L1")
         [(p/paragraph [(p/text "P1. L1")
                        (p/text "P1. L2")])
          (p/paragraph [(p/text "P2. L1")])])))