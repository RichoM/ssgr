(ns ssgr.eval
  (:require [sci.core :as sci]))

(def callbacks (atom []))

(defn register-callback! [c]
  (swap! callbacks conj c)
  nil)

(defn reset-callbacks! [] (reset! callbacks []))

(def ^:dynamic *element* nil)
(def ^:dynamic *render* nil)

(defn render []
  (reduce (fn [result callback]
            (callback *element* result))
          *render*
          @callbacks))

(def fns (sci/create-ns 'ssgr-ns nil))

(def ssgr-ns {'callbacks (sci/copy-var callbacks fns)
              'register-callback! (sci/copy-var register-callback! fns)
              'render (sci/copy-var render fns)})

(def opts {:namespaces {'ssgr ssgr-ns}})

(def ctx (sci/init opts))

(defn sci-var? [result]
  (= sci.lang.Var
     (type result)))

(defn eval-form [form]
  (let [result (sci/binding [sci/out *out*]
                 (sci/eval-form ctx form))]
    (if (sci-var? result)
      nil
      result)))

(defn eval-render [e r]
  (binding [*element* e
            *render* r]
    (eval-form '(ssgr/render))))

(comment
  (eval-form '(ssgr/render 1 2))

  (sci/eval-string* ctx "(ssgr/register-callback! (fn [e r] [:div 42]))")
  (sci/eval-string* ctx "@ssgr/callbacks")
  (sci/eval-string* ctx "(ssgr/render 1 2)")

  (eval-form '(deref ssgr/callbacks))
  (eval-form '(swap! ssgr/callbacks conj 2))
  (eval-form '(ssgr/register-callback! (fn [e r] (println "Test") r)))
  (eval-form '(ssgr/render 1 2))
)