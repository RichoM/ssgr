(ns ssgr.utils.writer
  (:require [clojure.string :as str]))

(defn make-writer []
  {:contents (volatile! []) ; TODO(Richo): Maybe use a native string builder or somethin?
   :indent-level (volatile! 0)})

(defn contents [writer]
  (str/join "" @(:contents writer)))

(defn append!
  ([writer string]
   (vswap! (:contents writer) conj string)
   writer)
  ([writer string & more] (append! writer (apply str string more))))

(defn append-line!
  ([writer] (append! writer "\n"))
  ([writer string]
   (append! writer string "\n")))

(defn append-indent! [writer]
  (let [level @(:indent-level writer)]
    (dotimes [_ level]
      (vswap! (:contents writer) conj "\t"))
    writer))

(defn inc-indent! [writer f]
  (let [old-level @(:indent-level writer)]
    (try
      (vswap! (:indent-level writer) inc)
      (f writer)
      (finally (vreset! (:indent-level writer) old-level)))
    writer))