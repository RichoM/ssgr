(ns ssgr.parser
  (:require [clojure.string :as str]
            [petitparser.core :as pp]
            [petitparser.input-stream :as in]
            [petitparser.results :as r]
            [edamame.core :as e]
            [ssgr.doc :as doc]))

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

(defn lines->blocks [lines]
  (->> lines
       (partition-by :type)
       (mapcat (fn [group]
                 (when-let [{:keys [type]} (first group)]
                   (case type
                     ; Headings are blocks already
                     ::doc/heading group
                     ; Lines should be grouped into paragraphs
                     ::doc/line [(apply doc/paragraph group)]
                     []))))))

(defn any-character-except [exception-set]
  (apply pp/or
         (conj (mapv #(pp/seq \\ %) exception-set)
               (pp/predicate #(not (contains? exception-set %))
                             (str "Expected any character except " exception-set)))))

(do
  (def grammar
    {:start :document
     :document :lines
     :lines (pp/separated-by (pp/optional :line)
                             :newline)
     :line (pp/or :atx-heading
                  (pp/plus :inline))
     :atx-heading [(pp/star :ws)
                   (pp/times \# 1 6)
                   (pp/plus :ws)
                   (pp/star :inline)
                   (pp/star :ws)]
     :inline (pp/or :inline-but-text :text)
     :inline-but-text (pp/or :image :link :code)
     :image [\! :link]
     :link [\[
            (pp/flatten
             (pp/star (any-character-except #{\[ \]})))
            \]
            \(
            (pp/flatten
             (pp/star (any-character-except #{\( \)})))
            \)]
     :code (pp/token clojure-parser)
     :text (pp/flatten (pp/or (pp/plus-lazy :char
                                            :inline-but-text)
                              (pp/plus :char)))
     :ws (pp/flatten (pp/plus (pp/or \tab \space)))
     :char (pp/predicate #(and (not= % \return)
                               (not= % \newline))
                         "Any char except newline")
     :newline (pp/or "\r\n" \return \newline)})

  (def transformations
    {:document (fn [lines]
                 (apply doc/document (lines->blocks lines)))
     :lines (fn [lines]
              (->> lines
                   (take-nth 2)
                   (map #(or % doc/empty-line))))
     :line (fn [heading-or-inline]
             (if (vector? heading-or-inline)
               (apply doc/line heading-or-inline)
               heading-or-inline))
     :atx-heading (fn [[_ atx _ content]]
                    (apply doc/heading (count atx) content))
     :image (fn [[_ {:keys [text destination]}]]
              (doc/image destination text))
     :link (fn [[_ text _ _ dest]]
             (doc/link text dest))
     :code (fn [{:keys [parsed-value] :as token}]
             (vary-meta (doc/code parsed-value)
                        assoc :token token))
     :text doc/text
     :newline (constantly \newline)})

  (def parser (pp/compose grammar transformations)))

(defn parse [src] (pp/parse parser src))

(comment

  (parse "Prueba [a\\[\\]bc](def)")

  (pp/parse (-> parser :parsers :line) "####### Title")
  (tap> (parse "###### Title"))
  )