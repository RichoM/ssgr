(ns ssgr.parser
  (:require [petitparser.core :as pp]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn heading [level content]
  {:type ::heading
   :level level
   :content content})

(defn text [text]
  {:type ::text
   :text text})


(defn paragraph [lines]
  {:type ::paragraph
   :lines lines})

(def empty-line {:type ::empty-line})

(do
  (def grammar
    {:start :lines
     :lines (pp/separated-by (pp/optional :line) 
                             :newline)
     :line (pp/or :atx-heading :inline)
     :atx-heading [(pp/star :ws) 
                   (pp/plus \#) 
                   (pp/plus :ws) 
                   (pp/star :inline)
                   (pp/star :ws)]
     :inline (pp/or :text)
     :text (pp/flatten (pp/plus :char))
     :ws (pp/flatten (pp/plus (pp/or \tab \space)))
     :char (pp/predicate #(and (not= % \return)
                               (not= % \newline))
                         "Any char except newline")
     :newline (pp/or "\r\n" \return \newline )})

  (def transformations
    {:lines (fn [lines]
              (->> lines
                   (take-nth 2)
                   (mapv #(if % % empty-line))
                   (partition-by :type)
                   (mapcat (fn [group]
                           (when-let [{:keys [type]} (first group)]
                             (case type
                               ::heading group
                               ::text [(paragraph (vec group))]
                               []))))
                   (vec)))
     :atx-heading (fn [[_ atx _ content]]
                    (heading (count atx) content))
     :text (comp text str/trim)
     :newline (constantly \newline)})

  (def parser (pp/compose grammar transformations)))

(defn parse [src] (pp/parse parser src))

(comment
  
  (pp/parse (pp/star (:inline (:parsers parser))) "Richo (+ 3 4)")
  (parse "# Richo (+ 3 4)")
  (parse "# Richo capo")
  (parse "# Richo\n# (+ 3 4)")

  (partition-by :type [{:type :ssgr.parser/empty-line}
                       {:content [{:text "Richo", :type :ssgr.parser/text}], :level 1, :type :ssgr.parser/heading}
                       {:text "Text", :type :ssgr.parser/text}
                       {:text "Text 2", :type :ssgr.parser/text}
                       {:type :ssgr.parser/empty-line}
                       {:type :ssgr.parser/empty-line}
                       {:text "Text3", :type :ssgr.parser/text}])
  (tap> (->> (parse "\n# Richo  \nText\nText 2\n\n\nText3")
             (partition-all 2)
             (map (fn [[a b]]
                    (if (nil? a)
                      empty-line
                      a)))))
  
  (pp/parse (pp/seq (-> parser :parsers :atx-heading)
                    (pp/optional pp/any))
            "\n# Richo  \n")

  (require '[portal.api :as p])

  (def p (p/open {:launcher :vs-code}))
  (add-tap #'p/submit)

  (tap> (pp/parse (-> parser :parsers :paragraph)
                  ""))
  (tap> {:richo "CAPO"})
  (tap> (parse "# Richo capo\nTest.\nSegunda línea\n\nSegundo párrafo.\nPrueba\nFin."))

  (doseq  [line (parse (slurp "README.md"))]
    (println line))

  (p/close p)


  (pp/parse (pp/as-parser [(pp/separated-by pp/digit pp/letter) pp/letter])
            "1a2b3")
  )