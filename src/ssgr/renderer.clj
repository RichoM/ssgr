(ns ssgr.renderer
  (:require [ssgr.doc :as doc]
            [ssgr.eval :as e]
            [hiccup.core :as h]
            [clojure.string :as str]))

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
  [:img {:src src :alt (str/join (keep doc/as-text description))}])

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

(defmethod render* ::doc/soft-break [_]
  "\n")

(defmethod render* ::doc/hard-break [_]
  [:br])

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
