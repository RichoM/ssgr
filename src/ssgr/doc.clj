(ns ssgr.doc 
  (:require [clojure.string :as str]))

(defn trim-heading [elements]
  (if (seq elements) ; Make sure it's not empty
    (->> (-> (vec elements)
             (update 0 (fn [element]
                         (if (= ::text (:type element))
                           (update element :text #(str/replace % #"^\s+" ""))
                           element)))
             (update (dec (count elements))
                     (fn [element]
                       (if (= ::text (:type element))
                         (update element :text #(str/replace % #"\s+#*\r*\n*$" ""))
                         element))))
         (filterv (fn [{:keys [type] :as element}]
                    (or (not= ::text type)
                        (not= "" (:text element))))))
    elements))

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements (trim-heading elements)})

(defn text [text]
  {:type ::text
   :text text})

(defn clojure [form result]
  {:type ::clojure
   :form form
   :result result})

(defn link [text destination]
  {:type ::link
   :text text
   :destination destination})

(defn image [src & description]
  {:type ::image
   :src src
   :description description})

(defn paragraph [& elements]
  {:type ::paragraph
   :elements (vec elements)})

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