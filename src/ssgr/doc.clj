(ns ssgr.doc 
  (:require [clojure.string :as str]))

(defn text? [{:keys [type]}]
  (= type ::text))

(let [elements [1 2 3]]
  (conj (pop elements)
        (inc (peek elements))))

(defn- trim-heading [elements]
  (let [last (peek elements)]
    (conj (pop elements)
          (if (text? last)
            (update last :text
                    #(str/replace % #"\s+#*$" ""))
            last))))

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements (trim-heading (vec elements))})

(defn text [text]
  {:type ::text
   :text text})

(defn code [form]
  {:type ::code
   :form form})

(defn paragraph [& lines]
  {:type ::paragraph
   :lines (vec lines)})

(defn line [& elements]
  {:type ::line
   :elements (vec elements)})

(defn text-line [text-content]
  (line (text text-content)))

(defn code-line [form]
  (line (code form)))

(def empty-line {:type ::empty-line})

(defn document [& blocks]
  {:type ::document
   :blocks (vec blocks)})