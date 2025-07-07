(ns ssgr.eval
  (:require [sci.core :as sci]
            [ssgr.token :as t]
            [ssgr.parser :as p]
            [ssgr.renderer :as r]))

(def callbacks (atom []))

(defn register-callback! [c]
  (swap! callbacks conj c)
  nil)

(defn reset-callbacks! [] (reset! callbacks []))

(def ^:dynamic *element* nil)
(def ^:dynamic *render* nil)

(defn render []
  (reduce (fn [result callback]
            (try
              (callback *element* result)
              (catch Throwable ex
                (println "ERROR applying render callbacks:"
                         (ex-message ex))
                result)))
          *render*
          @callbacks))

(declare eval-render)

(defn markdown [src]
  (let [{:keys [blocks]} (p/parse src {} nil)]
    (if (= 1 (count blocks))
      (let [{:keys [type] :as block} (first blocks)]
        (if (= :ssgr.doc/paragraph type)
          (if (= 1 (count (:elements block)))
            (r/render (first (:elements block)) eval-render)
            (map #(r/render % eval-render) (:elements block)))
          (r/render block eval-render)))
      (map #(r/render % eval-render) blocks))))

(def ssgr-fns (sci/create-ns 'ssgr-ns nil))
(def ssgr-ns {'callbacks (sci/copy-var callbacks ssgr-fns)
              'register-callback! (sci/copy-var register-callback! ssgr-fns)
              'render (sci/copy-var render ssgr-fns)
              'markdown (sci/copy-var markdown ssgr-fns)})

(def token-fns (sci/create-ns 'token-ns nil))
(def token-ns {'input-value (sci/copy-var t/input-value token-fns)
               'source (sci/copy-var t/source token-fns)
               'start (sci/copy-var t/start token-fns)
               'stop (sci/copy-var t/stop token-fns)
               'count (sci/copy-var t/count token-fns)
               'parsed-value (sci/copy-var t/parsed-value token-fns)})

(def opts {:namespaces {'ssgr ssgr-ns
                        'ssgr.token token-ns}})

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