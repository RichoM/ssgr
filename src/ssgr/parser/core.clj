(ns ssgr.parser.core
  (:require [clojure.string :as str]
            [petitparser.input-stream :as in]
            [petitparser.core :as pp]
            [petitparser.results :as r]
            [petitparser.token :as t]))

(defn transform-with-token [p f]
  (pp/transform (pp/token p)
                (fn [token]
                  (vary-meta (f (t/parsed-value token))
                             assoc :token token))))

(def thematic-break 
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/or (pp/min \- 3)
                          (pp/min \_ 3)
                          (pp/min \* 3))
                   (pp/star pp/space)))
   (fn [[_ chars]]
     {:type ::thematic-break
      :chars chars})))

(def atx-heading 
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/times \# 1 6)
                   (pp/flatten
                    (pp/optional (pp/seq pp/space
                                         (pp/plus pp/any))))))
   (fn [[_ level inline-text]]
     {:type ::atx-heading
      :level (count level)
      :content inline-text})))

(def setext-heading-underline
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/or (pp/plus \-)
                          (pp/plus \=))
                   (pp/star pp/space)))
   (fn [[_ chars]]
     {:type ::setext-heading-underline
      :chars chars})))

(def indented-code-block 
  (transform-with-token
   (pp/end (pp/flatten (pp/seq (pp/min pp/space 4)
                               (pp/plus (pp/negate pp/space))
                               (pp/star pp/any))))
   (fn [inline-text]
     {:type ::indented-code-block
      :content inline-text})))

(def opening-code-fence 
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/or (pp/min \` 3)
                          (pp/min \~ 3))
                   (pp/flatten (pp/star pp/any))))
   (fn [[_ chars info-string]]
     {:type ::opening-code-fence
      :chars chars
      :info-string info-string})))

(def closing-code-fence
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/or (pp/min \` 3)
                          (pp/min \~ 3))
                   (pp/star pp/space)))
   (fn [[_ chars]]
     {:type ::closing-code-fence
      :chars chars})))

(def blank (transform-with-token (pp/end (pp/star pp/space))
                                 (constantly {:type ::blank})))

(def paragraph
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/flatten [(pp/plus (pp/negate pp/space))
                                (pp/star pp/any)])))
   (fn [[_ inline-text]]
     {:type ::paragraph
      :content inline-text})))

(defn thematic-break? [line]
  (pp/matches? thematic-break line))

(defn atx-heading? [line]
  (pp/matches? atx-heading line))

(defn setext-heading-underline? [line]
  (pp/matches? setext-heading-underline line))

(defn indented-code-block? [line]
  (pp/matches? indented-code-block line))

(defn opening-code-fence? [line]
  (pp/matches? opening-code-fence line))

(defn closing-code-fence? [line]
  (pp/matches? closing-code-fence line))

(defn blank? [line]
  (pp/matches? blank line))

(defn paragraph? [line]
  (pp/matches? paragraph line))

(defn parse-line [line]
  (let [stream (in/make-stream line)
        parsers [thematic-break atx-heading setext-heading-underline
                 indented-code-block opening-code-fence closing-code-fence
                 blank paragraph]]
    (loop [[parser & rest] parsers]
      (let [result (pp/parse-on parser stream)]
        (if (r/failure? result)
          (when rest (recur rest))
          (r/actual-result result))))))

(defn parse-paragraph! [stream !blocks]
  (let [!lines (volatile! [])]
    (loop []
      (let [{:keys [type] :as next-line} (in/peek stream)]
        (case type
          ::paragraph
          (do (vswap! !lines conj (in/next! stream))
              (recur))

          ::setext-heading-underline
          (let [{:keys [chars]} (in/next! stream)]
            (vswap! !blocks conj
                    {:type ::heading
                     :level (if (= \- (first chars))
                              2
                              1)
                     :lines @!lines}))
          
          ; NOTE(Richo): Thematic breaks can be confused with setext-headings, in
          ; which case the setext-heading takes precedence
          ::thematic-break
          (when (= \- (:chars next-line))
            (in/next! stream) ; discard next
            (vswap! !blocks conj
                    {:type ::heading
                     :level 2
                     :lines @!lines}))
          
          ; NOTE(Richo): Indented code blocks can't interrupt a paragraph, so if
          ; we found one we just treat it as a valid line
          ::indented-code-block
          (do (vswap! !lines conj (in/next! stream))
              (recur))

          (vswap! !blocks conj
                  {:type ::paragraph
                   :lines @!lines}))))))

(defn parse-thematic-break! [stream !blocks]
  (in/next! stream)
  (vswap! !blocks conj {:type ::thematic-break}))

(defn parse-atx-heading! [stream !blocks]
  (let [{:keys [level content]} (in/next! stream)]
    (vswap! !blocks conj
            {:type ::atx-heading
             :level level
             :content content})))

(defn parse-blank-lines! [stream !blocks]
  (loop []
    (when (= ::blank (:type (in/peek stream)))
      (in/next! stream)
      (recur))))

(defn parse-indented-code-block! [stream !blocks]
  (let [!lines (volatile! [])]
    (loop []
      (let [{:keys [type]} (in/peek stream)]
        (when (= ::indented-code-block type)
          (vswap! !lines conj (in/next! stream))
          (recur))))
    (vswap! !blocks conj 
            {:type ::code-block
             :lines @!lines})))

(defn parse-block! [stream !blocks]
  (let [{:keys [type]} (in/peek stream)]
    (case type
      ::paragraph (parse-paragraph! stream !blocks)
      ::thematic-break (parse-thematic-break! stream !blocks)
      ::atx-heading (parse-atx-heading! stream !blocks)
      ::setext-heading-underline (parse-paragraph! stream !blocks)
      ::indented-code-block (parse-indented-code-block! stream !blocks)
      ::blank (parse-blank-lines! stream !blocks))))

(defn group-lines [parsed-lines]
  (let [stream (in/make-stream parsed-lines)
        !blocks (volatile! [])]
    (loop []
      (when-not (in/end? stream)
        (parse-block! stream !blocks)
        (recur)))
    @!blocks))

(defn parse [src]
  (let [input-lines (str/split-lines src)
        parsed-lines (map-indexed (fn [idx line]
                                    (vary-meta (parse-line line)
                                               assoc :line idx))
                                  input-lines)
        groups (group-lines parsed-lines)]
    groups))

(comment

  (def src (slurp "test-files/intro.md"))
  (def parsed-lines (parse src))

  (tap> parsed-lines)

  (def stream (in/make-stream [1 2 3 4]))

  (in/next! stream)
  
  )