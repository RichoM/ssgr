(ns ssgr.renderer
  (:require [ssgr.doc :as doc]))

(defmulti render* :type)

(defmethod render* ::doc/text [{:keys [text]}]
  [:span text])

(defmethod render* ::doc/heading [{:keys [level elements]}]
  ; TODO(Richo): If all elements are text we should avoid the spans
  (vec (concat [(keyword (str \h level))]
               (map render* elements))))

(defmethod render* ::doc/code [{:keys [form]}]
  form)

(defn render [document]
  (render* document))

(comment
  
  (render (doc/heading 1 (doc/text "Richo")
                       (doc/code '(+ 3 4))
                       (doc/text "Capo")))
  )