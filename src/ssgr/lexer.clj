(ns ssgr.lexer
  (:require [ssgr.input-stream :as in])
  (:import java.io.StringReader
           ssgr.InputStream))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn make-token
  ([type start]
   {:type type :start start :count 1})
  ([type start count]
   {:type type :start start :count count}))

;; (defprotocol Stream
;;   (peek [stream])
;;   (next! [stream])
;;   (position [stream]))

;; (deftype StringStream [src ^:unsynchronized-mutable ^long pos]
;;   Stream
;;   (peek [stream]
;;         (let [^String src (.src stream)
;;               pos (.pos stream)]
;;           (when (< pos (.length src))
;;             (char (.charAt src pos)))))
;;   (next! [stream]
;;     (when-let [val (peek stream)]
;;       (set! pos (inc (.pos stream)))
;;       val))
;;   (position [stream]
;;     (.pos stream)))

(defn test-input-stream [src]
  (let [stream (in/make-stream src)]
    (loop [result (transient [])]
      (if-let [next-char (in/next! stream)]
        (recur (conj! result next-char))
        (persistent! result)))))

(defn test-string-reader [src]
  (let [reader (StringReader. src)]
    (loop [result (transient [])
           idx 0]
      (let [next-char (.read reader)]
        (if-not (= -1 next-char)
          (recur (conj! result (char next-char))
                 (inc idx))
          (persistent! result))))))

(defn test-input-stream2 [src]
  (let [reader (InputStream. src)]
    (loop [result (transient [])]
      (let [next-char (.read reader)]
        (if-not (= -1 next-char)
          (recur (conj! result (char next-char)))
          (persistent! result))))))

(comment
  (require 'user)

  (nth "abc" 0)
  (get "abc" 10 nil)
  
  (def src (slurp "test-files/intro.cljmd"))

  (user/time+
   (dotimes [_ 100]
     (test-input-stream src)))
  
  (user/time+
   (dotimes [_ 100]
     (test-string-reader src)))
  
  (user/time+
   (dotimes [_ 100]
     (test-input-stream2 src)))
  
  (user/time+
   (dotimes [_ 100]
     (test-string-stream src)))

  (= (test-input-stream3 src)
     (test-string-reader src))
  (def reader (InputStream. src))

  (char (.read reader))
  (.-next reader)
  (.skip reader 2)

  (.reset reader)
  (.mark reader 1)
  (char 97)
  )