(ns ssgr.lexer
  (:refer-clojure :exclude [flatten])
  (:require [ssgr.input-stream :as in]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn make-token
  ([src type char line-number column-number start]
   {:src src 
    ;:input-value (subs src start (+ start 1))
    :type type :char char 
    :line-number line-number
    :column-number column-number
    :start start :count 1})
  ([src type char line-number column-number start count]
   {:src src     
    ;:input-value (subs src start (+ start count))
    :type type :char char 
    :line-number line-number
    :column-number column-number
    :start start :count count}))

(defn prev-char [{:keys [src ^long start]}]
  (nth src (dec start) nil))

(defn input-value [{:keys [src type char ^long start ^long count]}]
  (if (= ::word type)
    (subs src start (+ start count))
    (str char)))

(defn flatten [tokens]
  (let [first-token (first tokens)
        last-token (last tokens)]
    (subs (:src first-token)
          (:start first-token)
          (+ ^long (:start last-token)
             ^long (:count last-token)))))

(defn letter? [chr]
  (and chr (Character/isLetter ^char chr)))

(defn digit? [chr]
  (and chr (Character/isDigit ^char chr)))

(defn letter-or-digit? [chr]
  (and chr (Character/isLetterOrDigit ^char chr)))

(def space? #{\space \tab})

(def newline-char? #{\return \newline})

(def important-symbol? (set "`~-+*.(){}[]>-_*#-=\\!"))

(defn count-spaces-backwards! 
  [{:keys [src ^long start]} ^long offset]
  (loop [result 0
         idx (- start offset)]
    (let [char (nth src idx nil)]
      (if (and char (space? char))
        (recur (inc result)
               (dec idx))
        result))))

(defn skip-letters! [stream]
  (loop []
    (when-let [next-char (in/peek stream)]
      (when (or (letter-or-digit? next-char)
                (space? next-char)
                (and (not (newline-char? next-char))
                     (not (important-symbol? next-char))))
        (in/skip! stream)
        (recur)))))

(defn tokenize [src]
  (let [stream (in/make-stream src)]
    (loop [tokens (transient [])
           ^long pos (in/position stream)
           line-number 1
           ^long begin-line-pos pos]
      (if-let [next-char (in/next! stream)]
        (let [column-number (- pos begin-line-pos)
              is-newline? (newline-char? next-char)

              token
              (cond
                (letter? next-char)
                (do (skip-letters! stream)
                    (make-token src ::word next-char
                                line-number column-number
                                pos
                                (- ^long (in/position stream)
                                   ^long pos)))

                (space? next-char)
                (make-token src ::space next-char line-number column-number pos)

                (digit? next-char)
                (make-token src ::digit next-char line-number column-number pos)

                is-newline?
                (if (= \return next-char)
                  (if (= \newline (in/peek stream))
                    (do (in/skip! stream)
                        (make-token src ::newline next-char line-number column-number pos 2))
                    (make-token src ::newline next-char line-number column-number pos))
                  (make-token src ::newline next-char line-number column-number pos))

                :else
                (make-token src ::symbol next-char line-number column-number pos))]
          (recur (conj! tokens token)
                 (in/position stream)
                 (if is-newline?
                   (inc line-number)
                   line-number)
                 (if is-newline?
                   (in/position stream)
                   begin-line-pos)))
        (persistent! tokens)))))

(comment
  (require 'user)


  (def src (slurp "test-files/intro.cljmd"))
  (def src (slurp "test-files/03/02_UsoDeConsola.md"))
  (count src)
  (count (tokenize src))
  (def tokens (tokenize src))
  (take 20 tokens)

  (doseq [token (tokenize src)]
    (println (pr-str (input-value token))))

  (def token-stream (in/make-stream (tokenize src)))

  (in/next! token-stream)
  (in/reset-position! token-stream 0)
  (in/take-while! token-stream (comp #{\# \space} :char))
  (in/position token-stream)

  (user/time+
   (tokenize src))
  )