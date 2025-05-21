(ns ssgr.parser
  (:require [clojure.string :as str]
            [petitparser.input-stream :as in]
            [petitparser.core :as pp]
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
    (if (#{\( \[} (in/peek stream))
      (try
        (let [src (subs (in/source stream) (in/position stream))
              reader (e/source-reader src)
              result (e/parse-next reader (e/normalize-opts {:all true}))]
          (advance-stream-to-match! stream reader src)
          (r/success result))
        (catch Exception _
          (r/failure (in/position stream)
                     "Clojure code expected")))
      (r/failure (in/position stream)
                 "Literal '(' expected"))))

(def clojure-parser (ClojureParser.))

(def regexes
  {:atx-heading #"^\s{0,3}(#{1,6})\s(.*?)(\s#*)?$"})

(defn try-parse-atx-heading [line]
  (when-let [[_ level text] (re-matches (regexes :atx-heading) line)]
    (doc/heading (count level)
                 (doc/text (str/trim text)))))

(defn try-parse-text-line [line]
  (doc/paragraph (doc/text-line line)))

(defn parse-line [line]
  (or (try-parse-atx-heading line)
      (try-parse-text-line line))
  )

(defn parse [src]
  (apply doc/document (map parse-line (str/split-lines src))))
