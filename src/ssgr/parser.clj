(ns ssgr.parser
  (:require [clojure.string :as str]
            [petitparser.core :as pp]
            [petitparser.input-stream :as in]
            [petitparser.results :as r]
            [edamame.core :as e]))


(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn line-indices [string]
  (loop [[line & rest] (str/split string #"\n")
         start 0
         indices (transient [])]
    (if line
      (let [count (count line)
            stop (+ start count)]
        (recur
         rest
         (inc stop)
         (conj! indices [start stop])))
      (persistent! indices))))

(defn advance-stream-to-match! [stream reader source]
  (let [^long line-number (e/get-line-number reader)
        ^long column-number (e/get-column-number reader)
        [line-position] (nth (line-indices source)
                             (dec line-number))
        ^long position (+ (in/position stream)
                          line-position
                          (dec column-number))]
    (in/reset-position! stream position)))

(defrecord ClojureParser []
  petitparser.parsers.Parser
  (parse-on [_ stream]
    (if (= \( (in/peek stream))
      (try
        (let [src (subs (in/source stream) (in/position stream))
              reader (e/source-reader src)
              result (e/parse-next reader #_(e/normalize-opts {:all false}))]
          (advance-stream-to-match! stream reader src)
          (r/success result))
        (catch Exception _
          (r/failure (in/position stream)
                     (str "Clojure code expected"))))
      (r/failure (in/position stream)
                 (str "Literal '(' expected")))))

(def clojure-parser (ClojureParser.))

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements elements})

(defn text [text]
  {:type ::text
   :text text})

(defn code [form]
  {:type ::code
   :form form})

(defn paragraph [& lines]
  {:type ::paragraph
   :lines lines})

(defn line [& elements]
  {:type ::line
   :elements elements})

(defn text-line [text-content]
  (line (text text-content)))

(defn code-line [form]
  (line (code form)))

(def empty-line {:type ::empty-line})

(defn document [& blocks]
  {:type ::document
   :blocks blocks})

(defn lines->blocks [lines]
  (->> lines
       (partition-by :type)
       (mapcat (fn [group]
                 (when-let [{:keys [type]} (first group)]
                   (case type
                     ; Headings are blocks already
                     ::heading group
                     ; Lines should be grouped into paragraphs
                     ::line [(apply paragraph group)]
                     []))))))

(do
  (def grammar
    {:start :document
     :document :lines
     :lines (pp/separated-by (pp/optional :line) 
                             :newline)
     :line (pp/or :atx-heading 
                  (pp/plus :inline))
     :atx-heading [(pp/star :ws) 
                   (pp/plus \#) 
                   (pp/plus :ws) 
                   (pp/star :inline)
                   (pp/star :ws)]
     :inline (pp/or :code :text)
     :text (pp/flatten (pp/or (pp/plus-lazy :char :code)
                              (pp/plus :char)))
     :code clojure-parser
     :ws (pp/flatten (pp/plus (pp/or \tab \space)))
     :char (pp/predicate #(and (not= % \return)
                               (not= % \newline))
                         "Any char except newline")
     :newline (pp/or "\r\n" \return \newline )})

  (def transformations
    {:document (fn [lines]
                 (apply document (lines->blocks lines)))
     :lines (fn [lines]
              (->> lines
                   (take-nth 2)
                   (map #(or % empty-line))))
     :line (fn [heading-or-inline]
             (if (vector? heading-or-inline)
               (apply line heading-or-inline)
               heading-or-inline))
     :atx-heading (fn [[_ atx _ content]]
                    (apply heading (count atx) content))
     :text (comp text str/trim)
     :code code
     :newline (constantly \newline)})

  (def parser (pp/compose grammar transformations)))

(defn parse [src] (pp/parse parser src))

(comment
  (parse "# Richo (+ a b)")
  (parse "# Richo capo")
  (parse "# Richo\n(+ 3 4)")
  (def lines (pp/parse (-> parser :parsers :lines) "# Title\n\nRicho\nCapo\n\n123"))


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