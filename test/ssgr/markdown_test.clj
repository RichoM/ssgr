(ns ssgr.markdown-test
  (:require [clojure.test :refer [deftest testing is]]
            [ssgr.parser :as p]))

(deftest atx-heading
  (is (= (p/parse "# Heading 1")
         [(p/heading 1 "Heading 1")]))
  (is (= (p/parse "  ## Heading 2    \n")
         [(p/heading 2 "Heading 2")])))
