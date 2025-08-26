(ns ssgr.clojure
  (:require [clojure.string :as str]
            [ssgr.input-stream :as in]
            [ssgr.token :as t :refer [*parser-file*]]
            [edamame.core :as e]
            [hiccup.compiler :as h.c]
            [ssgr.doc :as doc]))

(def ^:dynamic *verbose-eval* false)

(defn find-line-col [string ^long position]
  (let [lines (str/split-lines (subs string 0 position))
        line-number (count lines)
        column-number (inc (count (peek lines)))]
    [line-number column-number]))

(defn find-line-idx ^long [string ^long line-number]
  (loop [[line & rest] (str/split string #"\n" line-number)
         line-idx 0
         line-count 1]
    (if (and line (< line-count line-number))
      (recur rest
             (+ line-idx (count line) 1)
             (inc line-count))
      line-idx)))

(defn advance-stream-to-match! [stream ^long begin-pos reader source]
  ; TODO(Richo): This could be made faster if the tokens would hold their line and
  ; column numbers. I could avoid calling find-line-idx and simply advance the stream
  ; until the correct token. 
  (let [^long line-number (e/get-line-number reader)
        ^long column-number (e/get-column-number reader)
        line-position (find-line-idx source line-number)
        position (+ begin-pos
                    line-position
                    (dec column-number))]
    (loop []
      (when-let [{:keys [^long start]} (in/peek stream)]
        (when (< start position)
          (in/next! stream) ; Skip
          (recur))))))

(defn eval-clojure [form eval-form]
  (let [result (eval-form form)]
    (if (vector? form)
      (h.c/normalize-element result)
      result)))

(defn parse-clojure! [stream ctx]
  (when-let [eval-form (-> ctx :eval-form)]
    (let [begin-pos (in/position stream)
          next-token (in/peek stream)]
      (when (#{\( \[} (:char next-token))
        (try
          (let [src (subs (:src next-token)
                          (:start next-token))
                reader (e/source-reader src)
                form (e/parse-next reader (e/normalize-opts {:all true}))
                result (eval-clojure form eval-form)]
            (advance-stream-to-match! stream (:start next-token) reader src)
            (t/with-token (doc/clojure form result)
              (t/stream->token stream begin-pos nil)))
          (catch Exception ex
            (when *verbose-eval*
              (let [[line-number column-number]
                    (find-line-col (:src next-token) ; TODO(Richo): I'm not sure this is correct!
                                   (:start next-token))]
                (println "ERROR evaluating clojure code at"
                         *parser-file* (str "(Ln " line-number ", Col " column-number "):")
                         (ex-message ex))))
            (in/reset-position! stream begin-pos)))))))

(defn eval-file! [file options eval-form]
  (binding [*parser-file* (str file)
            *verbose-eval* (:verbose options)]
    (try
      (doseq [form (e/parse-string-all (slurp file) {:all true})]
        (try
          (eval-clojure form eval-form)
          (catch Exception ex
            (println "ERROR evaluating clojure code at"
                     *parser-file* (str "(Ln " (-> form meta :row) 
                                        ", Col " (-> form meta :col) "):")
                     (ex-message ex)))))
      (catch Exception ex
        (println (str "ERROR parsing clojure file " file ":")
                 (ex-message ex))))))

(comment
  (def src (slurp "test-files/_layout.clj"))

  (e/parse-string-all src {:all true})
  (meta (first *1))
  
  )