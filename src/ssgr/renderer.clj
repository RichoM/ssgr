(ns ssgr.renderer
  (:require [ssgr.doc :as doc]
            [hiccup.core :as h]
            [clojure.string :as str]))

(declare render)

(defmulti render* :type)

(defmethod render* ::doc/text [{:keys [text]} _]
  [:span text])

(defmethod render* ::doc/heading [{:keys [level elements]} eval-render]
  ; TODO(Richo): If all elements are text we should avoid the spans
  (apply conj [(keyword (str \h level))]
         (map #(render % eval-render) elements)))

(defmethod render* ::doc/clojure [{:keys [result]} _]
  result)

(defn relative-url? [url]
  (nil? (re-find #"^(?i)(?:[a-z+]+:)?//" url)))

(defn fix-url [url]
  (if (relative-url? url)
    (-> url
        (str/replace #"\.md$" ".html"))
    url))

(defmethod render* ::doc/link [{:keys [text destination]} eval-render]
  (apply conj [:a {:href (fix-url destination)}]
         (map #(render % eval-render) text)))

(defmethod render* ::doc/image [{:keys [src description]} _]
  [:img {:src src :alt (str/join (keep doc/as-text description))}])

(defmethod render* ::doc/paragraph [{:keys [elements]} eval-render]
  (let [rendered-elements (keep #(render % eval-render) elements)]
    (when (seq rendered-elements)
      (apply conj [:p] rendered-elements))))

(defmethod render* ::doc/code-block [{:keys [info text]} _]
  [:pre [:code {:class info} text]])

(defmethod render* ::doc/code-span [{:keys [text]} _]
  [:code text])

(defmethod render* ::doc/thematic-break [_ _]
  [:hr])

(defmethod render* ::doc/soft-break [_ _]
  "\n")

(defmethod render* ::doc/hard-break [_ _]
  [:br])

(defmethod render* ::doc/emphasis [{:keys [text]} eval-render]
  (let [rendered-elements (keep #(render % eval-render) text)]
    (when (seq rendered-elements)
      (apply conj [:em] rendered-elements))))

(defmethod render* ::doc/strong-emphasis [{:keys [text]} eval-render]
  (let [rendered-elements (keep #(render % eval-render) text)]
    (when (seq rendered-elements)
      (apply conj [:strong] rendered-elements))))

(defmethod render* ::doc/list-item [{:keys [blocks]} eval-render]
  (let [rendered-blocks (keep #(render % eval-render) blocks)]
    (when (seq rendered-blocks)
      (apply conj [:li] rendered-blocks))))

(defmethod render* ::doc/ordered-list [{:keys [start items]} eval-render]
  (let [rendered-items (keep #(render % eval-render) items)
        rendered-list (if (not= 1 start)
                        [:ol {:start start}]
                        [:ol])]
    (when (seq rendered-items)
      (apply conj rendered-list rendered-items))))

(defmethod render* ::doc/bullet-list [{:keys [items]} eval-render]
  (let [rendered-items (keep #(render % eval-render) items)]
    (when (seq rendered-items)
      (apply conj [:ul] rendered-items))))

(defmethod render* ::doc/blockquote [{:keys [blocks]} eval-render]
  (let [rendered-blocks (keep #(render % eval-render) blocks)]
    (when (seq rendered-blocks)
      (apply conj [:blockquote] rendered-blocks))))

(defmethod render* ::doc/document [{:keys [blocks]} eval-render] 
  (let [rendered-blocks (keep #(render % eval-render) blocks)]
    (when (seq rendered-blocks)
      (apply conj [:div] rendered-blocks))))

(defn render [element eval-render]
  (let [result (render* element eval-render)]
    (eval-render element result)))

(defn html [content]
  (try 
    (str "<!doctype html>"
         (h/html content))
    (catch Throwable _
      (println "ERROR INVALID HICCUP!!" content)
      (str "ERROR (" content ")"))))
