(ns ssgr.doc)

(defn heading [level & elements]
  {:type ::heading
   :level level
   :elements elements})

(defn text [text]
  {:type ::text
   :text text})

(defn code [form]
  {:type ::code
   :form form})

(defn paragraph [& lines]
  {:type ::paragraph
   :lines lines})

(defn line [& elements]
  {:type ::line
   :elements elements})

(defn text-line [text-content]
  (line (text text-content)))

(defn code-line [form]
  (line (code form)))

(def empty-line {:type ::empty-line})

(defn document [& blocks]
  {:type ::document
   :blocks blocks})