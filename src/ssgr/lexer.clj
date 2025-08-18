(ns ssgr.lexer
  (:require [ssgr.input-stream :as in]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn make-token
  ([src type char start]
   {:src src :type type :char char :start start :count 1})
  ([src type char start count]
   {:src src :type type :char char :start start :count count}))

(defn input-value [{:keys [src type char ^long start ^long count]}]
  (if (= ::word type)
    (subs src start (+ start count))
    (str char)))

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

(defn tokenize [src]
  (let [stream (in/make-stream src)]
    (loop [tokens (transient [])
           pos (in/position stream)]
      (if-let [next-char (in/next! stream)]
        (let [token
              (cond
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
                  (if (= \newline (in/peek stream))
                    (do (in/next! stream) ; Skip
                        (make-token src ::newline next-char pos 2))
                    (make-token src ::newline next-char pos))
                  (make-token src ::newline next-char pos))

                :else
                (make-token src ::symbol next-char pos))]
          (recur (conj! tokens token)
                 (in/position stream)))
        (persistent! tokens)))))

(defn flatten [tokens]
  (let [first-token (first tokens)
        last-token (last tokens)]
    (subs (:src first-token)
          (:start first-token)
          (+ ^long (:start last-token)
             ^long (:count last-token)))))

(comment
  (require 'user)


  (def src (slurp "test-files/intro.cljmd"))
  (def src (slurp "test-files/03/02_UsoDeConsola.md"))
  (count src)
  (def token-stream (in/make-stream (tokenize src)))

  (in/next! token-stream)
  (in/reset-position! token-stream 0)
  (in/take-while! token-stream (comp #{\# \space} :char))
  (in/position token-stream)
  
  (user/time+
   (tokenize src))

  
  ; digit? 0-9
  ; space? \space \tab
  ; code-fence-char? \` \~
  ; newline-char? \return \newline
  ; bullet-list-marker? \- \+ \*
  ; ordered-list-separator? \. \)
  ; blockquote-marker? \>
  ; thematic-break? \- \_ \*
  ; atx-heading? \#
  ; setext-underline? \- \=
  ; 
  ; 
  ; word
  ; digit
  ; space
  ; symbol
  ; newline

  )