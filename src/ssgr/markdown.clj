(ns ssgr.markdown
  (:require [petitparser.core :as pp]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn heading [level text]
  {:type ::heading
   :level level
   :text text})

(defn text-line [text]
  {:type ::text-line
   :text text})

(defn empty-line [_]
  {:type ::empty-line})

(do
  (def grammar
    {:start (pp/star :line)
     :line (pp/or :empty-line :atx-heading :text-line)
     :atx-heading [:ws? (pp/plus \#) :text (pp/optional :newline)]
     :text (pp/flatten
            (pp/plus (pp/predicate #(not (or (= \newline %)
                                             (= \return %)))
                                   "Any char except newline")))
     :text-line [:text (pp/optional :newline)]
     ;:text-line (pp/flatten
     ;            (pp/or [(pp/plus-lazy pp/any :newline) :newline]
     ;                   (pp/end (pp/plus pp/any))))
     :empty-line [:ws? :newline]
     :ws? (pp/star (pp/or "\t" " "))
     :newline (pp/or "\n" "\n\r")})

  (def transformations
    {:atx-heading (fn [[_ atx text]]
                    (heading (count atx) text))
     :text str/trim
     :text-line (fn [[text _]] (text-line text))
     :empty-line empty-line})

  (def parser (pp/compose grammar transformations)))

(defn parse [src] (pp/parse parser src))

(comment
  (pp/parse (-> parser :parsers :text)
            "asdf\n")
  
  (parse "# Richo capo\nTest.\nSegunda línea\n\nSegundo párrafo.")

  (doseq  [line (pp/parse parser (slurp "README.md"))]
    (println line))
  

  
  )