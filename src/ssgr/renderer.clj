(ns ssgr.renderer
  (:require [ssgr.doc :as doc]
            [hiccup.core :as h]
            [clojure.string :as str]))

(defmulti render* :type)

(defmethod render* ::doc/text [{:keys [text]}]
  [:span text])

(defmethod render* ::doc/heading [{:keys [level elements]}]
  ; TODO(Richo): If all elements are text we should avoid the spans
  (vec (concat [(keyword (str \h level))]
               (map render* elements))))

(defmethod render* ::doc/code [{:keys [form]}]
  (str form))

(defmethod render* ::doc/paragraph [{:keys [lines]}]
  (vec (concat [:p]
               (mapcat render* lines))))

(defmethod render* ::doc/line [{:keys [elements]}]
  (mapv render* elements))

(defmethod render* ::doc/document [{:keys [blocks]}] 
  (mapv render* blocks))

(defn render [document]
  (render* document))

(comment
  (require '[hiccup.core :as h])

  (concat [:p]
          [1 2 23])
  (h/html [:span "Richo capo"])
  (h/html [:h1 [:span "Richo"]])

  (->> (render (doc/document
                (doc/heading 1 (doc/text "Main title"))
                (doc/paragraph (doc/text-line "Richo capo"))))
       (map #(h/html %))
       (str/join "\n"))

  (render (doc/heading 1 (doc/text "Richo")
                       (doc/code '(+ 3 4))
                       (doc/text "Capo")))
  )