(ns ssgr.doc 
  (:require [clojure.string :as str]
            [ssgr.token :as t]))

(defn remove-leading-spaces [t]
  (if-let [spaces (re-find #"^\s+" (:text t))]
    (-> t
        (update :text #(subs % (count spaces)))
        (vary-meta update :token #(t/remove-leading % (count spaces))))
    t))

(defn remove-trailing-spaces [t]
  (if-let [spaces (re-find #"\s+#*\r*\n*$" (:text t))]
         (-> t
             (update :text #(subs % 0 (- (count %)
                                         (count spaces))))
             (vary-meta update :token #(t/remove-trailing % (count spaces))))
         t))

(defn- trim-heading [elements]
  (if (seq elements) ; Make sure it's not empty
    (->> (-> (vec elements)
             (update 0 (fn [element]
                         (if (= ::text (:type element))
                           (remove-leading-spaces element)
                           element)))
             (update (dec (count elements))
                     (fn [element]
                       (if (= ::text (:type element))
                         (remove-trailing-spaces element)
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

(declare text)

(defn- merge-text [t1 t2]
  (t/with-token
    (text (str (:text t1)
               (:text t2)))
    (t/merge-tokens [t1 t2])))

(defn- compact-text-elements [elements]
  (reduce (fn [acc next]
            (if-let [last (peek acc)]
              (if (and (= ::text (:type last))
                       (= ::text (:type next)))
                (-> acc
                    (pop)
                    (conj (merge-text last next)))
                (conj acc next))
              (conj acc next)))
          []
          elements))

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements (compact-text-elements
              (trim-heading (remove-trailing-breaks elements)))})

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
   :description (compact-text-elements description)})

(defn paragraph [& elements]
  {:type ::paragraph
   :elements (compact-text-elements 
              (remove-trailing-breaks elements))})

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
   :text (compact-text-elements text)})

(defn strong-emphasis [& text]
  {:type ::strong-emphasis
   :text (compact-text-elements text)})

(defn list-item [& blocks]
  {:type ::list-item
   :blocks (vec blocks)})

(defn ordered-list [start & items]
  {:type ::ordered-list
   :start start
   :items (vec items)})

(defn bullet-list [& items]
  {:type ::bullet-list
   :items (vec items)})

(defn blockquote [& blocks]
  {:type ::blockquote
   :blocks (vec blocks)})

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
  
(defmethod as-text ::list-item [{:keys [blocks]}]
  (str/join (keep as-text blocks)))

(defmethod as-text ::ordered-list [{:keys [^long start items]}]
  (str/join "\n"
            (map-indexed (fn [^long idx item]
                           (str (+ start idx) ". " (as-text item)))
                         items)))

(defmethod as-text ::bullet-list [{:keys [items]}]
  (str/join "\n" (map #(str "- " (as-text %)) items)))

(defmethod as-text ::document [{:keys [blocks]}]
  (str/join (keep as-text blocks)))

(defmethod as-text :default [el] (str " " (:type el) " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-printer []
  {:lines (atom [])
   :indent-level (atom 0)})

(defn append-line! [{:keys [lines indent-level] :as printer} line]
  (swap! lines conj (str (str/join (repeat (* 2 ^long @indent-level) " ")) line))
  printer)

(defn indent-while! [{:keys [indent-level] :as p} f]
  (swap! indent-level inc)
  (f)
  (swap! indent-level dec)
  p)

(defn contents [{:keys [lines]}]
  (str/join "\n" @lines))

(defmulti pretty-print* :type)

(defmethod pretty-print* ::document [{:keys [blocks]} printer]
  (doto printer
    (append-line! "DOCUMENT")
    (indent-while!
     #(doseq [block blocks]
        (pretty-print* block printer)))))

(defmethod pretty-print* ::blockquote [{:keys [blocks]} printer]
  (doto printer
    (append-line! "BLOCKQUOTE")
    (indent-while! #(doseq [block blocks]
                      (pretty-print* block printer)))))

(defmethod pretty-print* ::ordered-list [{:keys [items]} printer]
  (doto printer
    (append-line! "ORDERED-LIST")
    (indent-while! #(doseq [item items]
                      (pretty-print* item printer)))))

(defmethod pretty-print* ::bullet-list [{:keys [items]} printer]
  (doto printer
    (append-line! "BULLET-LIST")
    (indent-while! #(doseq [item items]
                      (pretty-print* item printer)))))

(defmethod pretty-print* ::list-item [{:keys [blocks]} printer]
  (doto printer
    (append-line! "ITEM")
    (indent-while! #(doseq [block blocks]
                      (pretty-print* block printer)))))

(defmethod pretty-print* ::code-block [{:keys [info text]} printer]
  (doto printer
    (append-line! (str "CODE-BLOCK(" text ")"))))

(defmethod pretty-print* :default [el printer] 
  (doto printer
    (append-line! (as-text el))))

(defn pretty-print [element]
  (let [printer (make-printer)]
    (pretty-print* element printer)
    (println (contents printer))))
