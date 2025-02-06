(ns ssgr.renderer
  (:require [ssgr.doc :as doc]
            [ssgr.eval :as e]
            [hiccup.core :as h]
            [hiccup.compiler :as h.c]
            [clojure.string :as str]
            [petitparser.token :as t]))

(declare render)

(defmulti render* :type)

(defmethod render* ::doc/text [{:keys [text]}]
  [:span text])

(defmethod render* ::doc/heading [{:keys [level elements]}]
  ; TODO(Richo): If all elements are text we should avoid the spans
  (vec (concat [(keyword (str \h level))]
               (map render elements))))

(defmethod render* ::doc/clojure [{:keys [form] :as el}]
  (try
    (let [result (e/eval-form form)]
      (if (vector? form)
        (h.c/normalize-element result)
        result))
    (catch Throwable _
      (if-let [token (-> el meta :token)]
        (render (doc/text (t/input-value token)))
        (str form)))))

(defn relative-url? [url]
  (nil? (re-find #"^(?i)(?:[a-z+]+:)?//" url)))

(defn fix-url [url]
  (if (relative-url? url)
    (-> url
        (str/replace #"\.md$" ".html"))
    url))

(defmethod render* ::doc/link [{:keys [text destination]}]
  [:a {:href (fix-url destination)} text])

(defmethod render* ::doc/image [{:keys [src alt]}]
  [:img {:src src :alt alt}])

(defmethod render* ::doc/paragraph [{:keys [lines]}]
  (let [rendered-lines (mapcat render lines)]
    (when (seq rendered-lines)
      (vec (concat [:p] rendered-lines)))))

(defmethod render* ::doc/code-block [{:keys [info text]}]
  [:pre [:code {:class info} text]])

(defmethod render* ::doc/line [{:keys [elements]}]
  (vec (keep render elements)))

(defmethod render* ::doc/document [{:keys [blocks]}] 
  (let [rendered-blocks (keep render blocks)]
    (when (seq rendered-blocks)
      (vec (concat [:div] rendered-blocks)))))

(defn render [element]
  (let [result (render* element)]
    (e/eval-render element result)))

(defn html [content]
  (try 
    (h/html content)
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