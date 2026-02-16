(ns ssgr.renderer
  (:require [ssgr.doc :as doc]
            [hiccup.core :as h]
            [clojure.string :as str]))

(declare render)

(defn render-text [{:keys [text]} _]
  [:span text])

(defn render-heading [{:keys [level elements]} eval-render]
  ; TODO(Richo): If all elements are text we should avoid the spans
  (apply conj [(keyword (str \h level))]
         (map #(render % eval-render) elements)))

(defn render-clojure [{:keys [result]} _]
  result)

(defn relative-url? [url]
  (nil? (re-find #"^(?i)(?:[a-z+]+:)?//" url)))

(defn fix-url [url]
  (if (relative-url? url)
    (-> url
        (str/replace #"\.md$" ".html"))
    url))

(defn render-link [{:keys [text destination]} eval-render]
  (apply conj [:a {:href (fix-url destination)}]
         (map #(render % eval-render) text)))

(defn render-image [{:keys [src description]} _]
  [:img {:src src :alt (str/join (keep doc/as-text description))}])

(defn render-paragraph [{:keys [elements]} eval-render]
  (let [rendered-elements (keep #(render % eval-render) elements)]
    (when (seq rendered-elements)
      (apply conj [:p] rendered-elements))))

(defn render-code-block [{:keys [info text]} _]
  [:pre [:code {:class info} text]])

(defn render-code-span [{:keys [text]} _]
  [:code text])

(defn render-thematic-break [_ _]
  [:hr])

(defn render-soft-break [_ _]
  "\n")

(defn render-hard-break [_ _]
  [:br])

(defn render-emphasis [{:keys [text]} eval-render]
  (let [rendered-elements (keep #(render % eval-render) text)]
    (when (seq rendered-elements)
      (apply conj [:em] rendered-elements))))

(defn render-strong-emphasis [{:keys [text]} eval-render]
  (let [rendered-elements (keep #(render % eval-render) text)]
    (when (seq rendered-elements)
      (apply conj [:strong] rendered-elements))))

(defn render-list-item [{:keys [blocks]} eval-render]
  (let [rendered-blocks (keep #(render % eval-render) blocks)]
    (when (seq rendered-blocks)
      (apply conj [:li] rendered-blocks))))

(defn render-ordered-list [{:keys [start items]} eval-render]
  (let [rendered-items (keep #(render % eval-render) items)
        rendered-list (if (not= 1 start)
                        [:ol {:start start}]
                        [:ol])]
    (when (seq rendered-items)
      (apply conj rendered-list rendered-items))))

(defn render-bullet-list [{:keys [items]} eval-render]
  (let [rendered-items (keep #(render % eval-render) items)]
    (when (seq rendered-items)
      (apply conj [:ul] rendered-items))))

(defn render-blockquote [{:keys [blocks]} eval-render]
  (let [rendered-blocks (keep #(render % eval-render) blocks)]
    (when (seq rendered-blocks)
      (apply conj [:blockquote] rendered-blocks))))

(defn render-document [{:keys [blocks]} eval-render] 
  (let [rendered-blocks (keep #(render % eval-render) blocks)]
    (when (seq rendered-blocks)
      (apply conj [:div] rendered-blocks))))

(defn render* [element eval-render]
  (case (:type element)
    ::doc/document (render-document element eval-render)
    ::doc/blockquote (render-blockquote element eval-render)
    ::doc/bullet-list (render-bullet-list element eval-render)
    ::doc/ordered-list (render-ordered-list element eval-render)
    ::doc/list-item (render-list-item element eval-render)
    ::doc/strong-emphasis (render-strong-emphasis element eval-render)
    ::doc/emphasis (render-emphasis element eval-render)
    ::doc/hard-break (render-hard-break element eval-render)
    ::doc/soft-break (render-soft-break element eval-render)
    ::doc/thematic-break (render-thematic-break element eval-render)
    ::doc/code-span (render-code-span element eval-render)
    ::doc/code-block (render-code-block element eval-render)
    ::doc/paragraph (render-paragraph element eval-render)
    ::doc/image (render-image element eval-render)
    ::doc/link (render-link element eval-render)
    ::doc/clojure (render-clojure element eval-render)
    ::doc/heading (render-heading element eval-render)
    ::doc/text (render-text element eval-render)))

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
