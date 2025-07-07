(ns ssgr.eval
  (:require [sci.core :as sci]
            [ssgr.doc :as doc]
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

(defn call-render-callbacks []
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
              'call-render-callbacks (sci/copy-var call-render-callbacks ssgr-fns)
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

(defn eval-form 
  "Called by the parser any time it encounters some text that looks like it could be
   clojure code. If it evaluates correctly, it returns the result. Otherwise, it should
   throw an exception and it's the parser's responsibility to handle it appropriately.
   This is also called from eval-render to invoke the render callbacks for all elements."
  [form]
  (let [result (sci/binding [sci/out *out*]
                 (sci/eval-form ctx form))]
    ; NOTE(Richo): We check if the result is a sci.lang.Var to avoid exposing any function
    ; of variable defined by the user into the document
    (if (sci-var? result)
      nil
      result)))

(defn eval-render
  "Called by the renderer after processing each element. Its arguments are the element
   being processed and the result of rendering that element.
   This is useful for providing a callback mechanism that allows to intercept the 
   rendering of any element from user code.
   I don't know if there is a better way of then calling the render function (in the 
   context of sci) with this values as parameters, but what I found works is to just
   bind this values to some global variables and make the render function simply read
   from those global variables. It kind of sucks, but it works..."
  [e r]
  (binding [*element* e
            *render* r]
    (eval-form '(ssgr/call-render-callbacks))))
