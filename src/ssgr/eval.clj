(ns ssgr.eval
  (:require [sci.core :as sci]))

(def ctx (sci/init {}))

(defn sci-var? [result]
  (= sci.lang.Var
     (type result)))

(defn eval-form [form]
  (let [result (sci/eval-form ctx form)]
    (println (sci-var? result))
    (if (sci-var? result)
      nil
      result)))
