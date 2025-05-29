(ns ssgr.doc 
  (:require [clojure.string :as str]))

(defn- trim-heading [elements]
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

(defn- remove-trailing-breaks [elements]
  (if (#{::soft-break ::hard-break}
       (:type (last elements)))
    (drop-last 1 elements)
    elements))

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements (trim-heading (remove-trailing-breaks elements))})

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
   :description (vec description)})

(defn paragraph [& elements]
  {:type ::paragraph
   :elements (vec (remove-trailing-breaks elements))})

(defn code-block [info text]
  {:type ::code-block
   :info info
   :text text})

(defn thematic-break []
  {:type ::thematic-break})

(defn soft-break []
  {:type ::soft-break})

(defn hard-break []
  {:type ::hard-break})

(defn code-span [text]
  {:type ::code-span
   :text text})

(defn emphasis [& text]
  {:type ::emphasis
   :text (vec text)})

(defn strong-emphasis [& text]
  {:type ::strong-emphasis
   :text (vec text)})

(defn document [& blocks]
  {:type ::document
   :blocks (vec blocks)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti as-text :type)

(defmethod as-text ::text [{:keys [text]}]
  text)

(defmethod as-text ::heading [{:keys [elements]}]
  (str/join (keep as-text elements)))

(defmethod as-text ::clojure [{:keys [result]}]
  (str result))

(defmethod as-text ::link [{:keys [text]}]
  (str/join (keep as-text text)))

(defmethod as-text ::image [{:keys [description]}]
  (str/join (keep as-text description)))

(defmethod as-text ::paragraph [{:keys [elements]}]
  (str/join (keep as-text elements)))

(defmethod as-text ::code-block [{:keys [text]}]
  text)

(defmethod as-text ::code-span [{:keys [text]}]
  text)

(defmethod as-text ::document [{:keys [blocks]}]
  (str/join (keep as-text blocks)))

(defmethod as-text :default [_] "")