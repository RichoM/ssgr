(ns ssgr.token
  (:require [petitparser.input-stream :as in]
            [petitparser.token :as t]))

(def input-value t/input-value)
(def make-token t/make-token)
(def source t/source)
(def start t/start)
(def stop t/stop)
(def count t/count)
(def parsed-value t/parsed-value)

(def ^:dynamic *debug-verbose-tokens* false)
(def ^:dynamic *parser-file* nil)

(defn assoc-input-value [token]
  (if *debug-verbose-tokens*
    (try (assoc token :input-value (input-value token))
         (catch Exception ex
           (println ex)
           (println token)))
    token))

(defn stream->token
  "Utility function to make a token from the current position of a stream"
  ([stream begin-pos parsed-value]
   (stream->token stream
                  begin-pos
                  (in/position stream)
                  parsed-value))
  ([stream ^long begin-pos ^long end-pos parsed-value]
   (make-token (in/source stream)
               begin-pos
               (- end-pos begin-pos)
               parsed-value)))

(defn merge-tokens [nodes]
  (let [tokens (vec (keep #(-> % meta :token) nodes))]
    (when (seq tokens)
      (let [first-token (first tokens)
            last-token (peek tokens)]
        (if (= 1 (clojure.core/count tokens))
          first-token
          (make-token (source first-token)
                      (start first-token)
                      (- ^long (stop last-token)
                         ^long (start first-token))
                      nodes))))))

(defn with-token [node token]
  (vary-meta (if token
               (vary-meta node assoc :token (assoc-input-value token))
               node)
             assoc :file *parser-file*))