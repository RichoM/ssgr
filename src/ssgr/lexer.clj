(ns ssgr.lexer
  (:require [ssgr.input-stream :as in]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn make-token
  ([src type char start]
   {:src src :type type :char char :start start :count 1})
  ([src type char start count]
   {:src src :type type :char char :start start :count count}))

(defn letter? [chr]
  (and chr (Character/isLetter ^char chr)))

(defn digit? [chr]
  (and chr (Character/isDigit ^char chr)))

(def space? #{\space \tab})

(def newline-char? #{\return \newline})

(defn skip-letters! [stream]
  (loop []
    (when (letter? (in/peek stream))
      (in/next! stream)
      (recur))))

(defn tokenize! [src]
  (let [stream (in/make-stream src)]
    (loop [tokens (transient [])
           pos (in/position stream)]
      (if-let [next-char (in/next! stream)]
        (let [token (cond
                      (letter? next-char)
                      (do (skip-letters! stream)
                          (make-token src ::word next-char 
                                      pos 
                                      (- ^long (in/position stream) 
                                         ^long pos)))

                      (space? next-char)
                      (make-token src ::space next-char pos)

                      (digit? next-char)
                      (make-token src ::digit next-char pos)
                      
                      (newline-char? next-char)
                      (if (= \return next-char)
                        (if (= \newline next-char)
                          (do (in/next! stream)
                              (make-token src ::newline next-char pos 2))
                          (make-token src ::newline next-char pos))
                        (make-token src ::newline next-char pos))
                      
                      :else
                      (make-token src ::symbol next-char pos))]
          (recur (conj! tokens token)
                 (in/position stream)))
        (persistent! tokens)))))

(comment
  (require 'user)

  
  (nth "abc" 0)
  (get "abc" 10 nil)

  (def src (slurp "test-files/intro.cljmd"))
  (def src (slurp "test-files/03/02_UsoDeConsola.md"))
  (count src)
  (count (tokenize! src))
  
  (user/time+
   (tokenize! src))

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