(ns ssgr.parser
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
            (pp/plus (pp/predicate #(and (not= % \return)
                                         (not= % \newline))
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
  (require '[portal.api :as p])

  (def p (p/open))
  (add-tap #'p/submit) ; Add portal as a tap> target
  \n

  (tap> {:richo "CAPO"})
  (tap> (parse "# Richo capo\nTest.\nSegunda línea\n\nSegundo párrafo.\nPrueba\nFin."))

  (doseq  [line (pp/parse parser (slurp "README.md"))]
    (println line))



  )