(ns ssgr.doc 
  (:require [clojure.string :as str]))

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements (vec elements)})

(defn text [text]
  {:type ::text
   :text text})

(defn clojure [form]
  {:type ::clojure
   :form form})

(defn link [text destination]
  {:type ::link
   :text text
   :destination destination})

(defn image [src alt]
  {:type ::image
   :src src
   :alt alt})

(defn paragraph [& lines]
  {:type ::paragraph
   :lines (vec lines)})

(defn code-block [info text]
  {:type ::code-block
   :info info
   :text text})

(defn thematic-break []
  {:type ::thematic-break})

(defn code-span [text]
  {:type ::code-span
   :text text})

(defn document [& blocks]
  {:type ::document
   :blocks (vec blocks)})