(ns ssgr.renderer
  (:require [ssgr.doc :as doc]
            [ssgr.eval :as e]
            [hiccup.core :as h]
            [hiccup.compiler :as h.c]
            [clojure.string :as str]
            [petitparser.token :as t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti as-text :type)

(defmethod as-text ::doc/text [{:keys [text]}]
  text)

(defmethod as-text ::doc/heading [{:keys [elements]}]
  (str/join (keep as-text elements)))

(defmethod as-text ::doc/clojure [{:keys [result]}]
  (str result))

(defmethod as-text ::doc/link [{:keys [text]}]
  (str/join (keep as-text text)))

(defmethod as-text ::doc/image [{:keys [description]}]
  (str/join (keep as-text description)))

(defmethod as-text ::doc/paragraph [{:keys [elements]}]
  (str/join (keep as-text elements)))

(defmethod as-text ::doc/code-block [{:keys [text]}]
  text)

(defmethod as-text ::doc/code-span [{:keys [text]}]
  text)

(defmethod as-text ::doc/thematic-break [_]
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare render)

(defmulti render* :type)

(defmethod render* ::doc/text [{:keys [text]}]
  [:span text])

(defmethod render* ::doc/heading [{:keys [level elements]}]
  ; TODO(Richo): If all elements are text we should avoid the spans
  (vec (concat [(keyword (str \h level))]
               (map render elements))))

(defmethod render* ::doc/clojure [{:keys [result]}]
  result)

(defn relative-url? [url]
  (nil? (re-find #"^(?i)(?:[a-z+]+:)?//" url)))

(defn fix-url [url]
  (if (relative-url? url)
    (-> url
        (str/replace #"\.md$" ".html"))
    url))

(defmethod render* ::doc/link [{:keys [text destination]}]
  (vec (concat [:a {:href (fix-url destination)}]
               (map render text))))

(defmethod render* ::doc/image [{:keys [src description]}]
  [:img {:src src :alt (str/join (keep as-text description))}])

(defmethod render* ::doc/paragraph [{:keys [elements]}]
  (let [rendered-elements (keep render elements)]
    (when (seq rendered-elements)
      (vec (concat [:p] rendered-elements)))))

(defmethod render* ::doc/code-block [{:keys [info text]}]
  [:pre [:code {:class info} text]])

(defmethod render* ::doc/code-span [{:keys [text]}]
  [:code text])

(defmethod render* ::doc/thematic-break [_]
  [:hr])

(defmethod render* ::doc/document [{:keys [blocks]}] 
  (let [rendered-blocks (keep render blocks)]
    (when (seq rendered-blocks)
      (vec (concat [:div] rendered-blocks)))))

(defn render [element]
  (let [result (render* element)]
    (e/eval-render element result)))

(defn html [content]
  (try 
    (str "<!doctype html>"
         (h/html content))
    (catch Throwable _
      (println "ERROR INVALID HICCUP!!" content)
      (str "ERROR (" content ")"))))

(comment
  (require '[hiccup.core :as h])

  (def x 5)
  (def lst '(a b c))
  
  

  (concat [:p]
          [1 2 23])
  (h/html [:span "Richo capo"])
  (h/html [:h1 [:span "Richo"]])

  (re-find #"^(?i)(?:[a-z+]+:)?//"
           "test")
  (->> (render (doc/document
                (doc/paragraph
                 (doc/line (doc/link "test"
                                     "http://url.com")))))
       (map #(h/html %))
       (str/join "\n"))

  (render (doc/heading 1 (doc/text "Richo")
                       (doc/code '(+ 3 4))
                       (doc/text "Capo")))
  )