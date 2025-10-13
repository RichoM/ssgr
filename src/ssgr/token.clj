(ns ssgr.token
  (:refer-clojure :exclude [count])
  (:require [ssgr.input-stream :as in]))

(def ^:dynamic *debug-verbose-tokens* false)
(def ^:dynamic *parser-file* nil)

(defn make-token [source start count]
  {:source source
   :start start
   :count count})

(defn source [token] (:source token))
(defn start [token] (:start token))
(defn count [token] (:count token))

(defn stop [{:keys [^long start ^long count]}]
  (+ start count))

(defn input-value [{:keys [source ^long start ^long count]}]
  (subs source start (+ start count)))


(defn assoc-input-value [token]
  (if *debug-verbose-tokens*
    (try (assoc token :input-value (input-value token))
         (catch Exception ex
           (println ex)
           (println token)))
    token))

(defn lexer-tokens->token [lexer-tokens]
  (let [first-token (first lexer-tokens)
        last-token (peek lexer-tokens)
        start (:start first-token)
        end (+ (:start last-token)
               (:count last-token))]
    (make-token (:src first-token)
                start
                (- end start))))

(defn stream->token
  "Utility function to make a token from the current position of a stream"
  ([stream begin-pos]
   (stream->token stream
                  begin-pos
                  (in/position stream)))
  ([stream ^long begin-pos ^long end-pos]
   (when (> end-pos begin-pos)
     (let [tokens (subvec (in/source stream)
                          begin-pos end-pos)]
       (lexer-tokens->token tokens)))))


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
                      ))))))

(defn with-token [node token]
  (vary-meta (if token
               (vary-meta node assoc :token (assoc-input-value token))
               node)
             assoc :file *parser-file*))