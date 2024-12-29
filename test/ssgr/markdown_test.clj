(ns ssgr.markdown-test
  (:require [clojure.test :refer [deftest testing is]]
            [ssgr.markdown :as md]))

(deftest atx-heading
  (is (= (md/parse "# Heading 1")
         [(md/heading 1 "Heading 1")]))
  (is (= (md/parse "  ## Heading 2    \n")
         [(md/heading 2 "Heading 2")])))
