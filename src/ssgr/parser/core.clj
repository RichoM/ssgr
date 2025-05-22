(ns ssgr.parser.core
  (:require [clojure.string :as str]
            [petitparser.input-stream :as in]
            [petitparser.core :as pp]
            [petitparser.results :as r]
            [petitparser.token :as t]
            [ssgr.doc :as doc]))

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

(def code-fence
  (transform-with-token
   (pp/end (pp/seq (pp/max pp/space 3)
                   (pp/or (pp/min \` 3)
                          (pp/min \~ 3))
                   (pp/flatten (pp/star pp/any))))
   (fn [[_ chars info-string]]
     {:type ::code-fence
      :chars chars
      :info-string info-string})))

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

(defn code-fence? [line]
  (pp/matches? code-fence line))

(defn blank? [line]
  (pp/matches? blank line))

(defn paragraph? [line]
  (pp/matches? paragraph line))

(defn parse-line [line]
  (let [stream (in/make-stream line)
        parsers [thematic-break atx-heading
                 setext-heading-underline
                 indented-code-block code-fence
                 blank paragraph]]
    (loop [[parser & rest] parsers]
      (let [result (pp/parse-on parser stream)]
        (if (r/failure? result)
          (when rest (recur rest))
          (r/actual-result result))))))

;;; Inline parsers

(defn consume-while! [stream while-fn]
  (loop [result []]
    (let [chr (in/peek stream)]
      (if (and chr (while-fn chr))
        (recur (conj result (in/next! stream)))
        result))))

(defn consume-until! [stream until-fn]
  (consume-while! stream (complement until-fn)))

(defn consume-chars! [stream & chars]
  (consume-while! stream (set chars)))

(defn consume-1-char! [stream char]
  (let [next (in/peek stream)]
    (when (= char next)
      (in/next! stream))))

(defn parse-code-span! [stream !elements]
  (let [begin-pos (in/position stream)
        opening (consume-chars! stream \`)
        [content closing]
        (loop [content []]
          (let [next-char (in/peek stream)]
            (case next-char
              ; The closing and opening must be of equal length
              \` (let [closing (consume-chars! stream \`)]
                   (if (= (count closing)
                          (count opening))
                     [content closing]
                     (recur (apply conj content closing))))

              ; Line endings are converted to spaces
              \newline (do (in/next! stream) ; Discard
                           (recur (conj content \space)))
              \return (do (in/next! stream) ; Discard
                          (consume-1-char! stream \newline)
                          (recur (conj content \space)))

              ; Anything else is appended to the content
              (if next-char
                (recur (conj content (in/next! stream)))
                [content nil]))))
        end-pos (in/position stream)
        token (t/make-token (in/source stream)
                            begin-pos
                            (- end-pos begin-pos)
                            [opening content closing])]
    (vswap! !elements conj 
            (vary-meta (if closing
                         (doc/code-span (str/join content))
                         (doc/text (t/input-value token)))
                       assoc :token token))))

(defn parse-text! [stream !elements]
  (let [begin-pos (in/position stream)
        content (consume-until! stream #{\`})
        end-pos (in/position stream)]
    (vswap! !elements conj
            (vary-meta (doc/text (str/join content))
                       assoc :token (t/make-token (in/source stream)
                                                  begin-pos
                                                  (- end-pos begin-pos)
                                                  content)))))

(defn parse-inlines [src]
  (let [stream (in/make-stream src)
        !elements (volatile! [])]
    (loop []
      (when-let [next-char (in/peek stream)]
        (case next-char
          \` (parse-code-span! stream !elements)
          (parse-text! stream !elements))
        (recur)))
    @!elements))

(comment

  (do (def src "``foo``")
      (def stream (in/make-stream src)))

  (consume-1-char! stream \`)
  (consume-chars! stream \`)
  (in/peek stream)

  (parse-inlines src)

  (tap> *1)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn trim-heading [content]
  (-> content
      (str/replace #"^\s+" "")
      (str/replace #"\s+#*\r*\n*$" "")))

(defn parse-paragraph! [stream !blocks]
  (let [!lines (volatile! [])
        close-paragraph! #(let [lines (->> @!lines
                                           (map :content)
                                           (str/join "\n")
                                           (parse-inlines))]
                            (vswap! !blocks conj
                                    (vary-meta (apply doc/paragraph lines)
                                               assoc :lines @!lines)))]
    (loop []
      (let [{:keys [type] :as next-line} (in/peek stream)]
        (case type
          ::paragraph
          (do (vswap! !lines conj (in/next! stream))
              (recur))

          ; If we find setext-heading-underline, we convert the whole 
          ; paragraph to a heading
          ::setext-heading-underline
          (let [{:keys [chars]} (in/next! stream)]
            (vswap! !blocks conj
                    (vary-meta (apply doc/heading (if (= \- (first chars))
                                                    2
                                                    1)
                                      (->> @!lines
                                           (map :content)
                                           (map trim-heading) ; TODO(Richo): Parse inlines!
                                           (map doc/text)))
                               assoc :lines (conj @!lines next-line))))

          ; Thematic breaks can be confused with setext-headings, in
          ; which case the setext-heading takes precedence
          ::thematic-break
          (if (= \- (first (:chars next-line)))
            (do (in/next! stream) ; discard next
                (vswap! !blocks conj
                        (vary-meta (apply doc/heading 2
                                          (->> @!lines
                                               (map :content)
                                               (map trim-heading) ; TODO(Richo): Parse inlines!
                                               (map doc/text)))
                                   assoc :lines (conj @!lines next-line))))
            (close-paragraph!))

          ; Indented code blocks can't interrupt a paragraph, so if
          ; we found one we just treat it as a valid line
          ::indented-code-block
          (do (vswap! !lines conj (in/next! stream))
              (recur))

          ; Anything else, simply breaks the paragraph, we do nothing
          (close-paragraph!))))))

(defn parse-thematic-break! [stream !blocks]
  (let [line (in/next! stream)]
    (vswap! !blocks conj (vary-meta (doc/thematic-break)
                                    assoc :lines [line]))))

(defn parse-atx-heading! [stream !blocks]
  (let [{:keys [level content] :as line} (in/next! stream)]
    (vswap! !blocks conj
            (vary-meta (apply doc/heading level
                              ; TODO(Richo): Parse content as inline
                              [(doc/text (trim-heading content))])
                       assoc :lines [line]))))

(defn parse-blank-lines! [stream _]
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

    ; NOTE(Richo): We take the actual lines from the tokens because
    ; the code blocks should preserve whatever the user typed. 
    ; However, since we're parsing an indented block code we, first
    ; need to remove the first 4 indentation spaces.
    (let [lines (map #(-> % meta :token t/input-value (subs 4))
                     @!lines)]
      (vswap! !blocks conj
              (vary-meta (doc/code-block "" (str/join "\n" lines))
                         assoc :lines @!lines)))))

(defn parse-fenced-code-block! [stream !blocks]
  (let [opening (in/next! stream)
        !lines (volatile! [])]
    (loop []
      (when-let [{:keys [type] :as next} (in/peek stream)]
        (when-not (and (= ::code-fence type)
                       (str/blank? (:info-string next))
                       (= (-> opening :chars first)
                          (-> next :chars first))
                       (<= (-> opening :chars count)
                           (-> next :chars count)))
          (vswap! !lines conj (in/next! stream))
          (recur))))

    ; NOTE(Richo): We take the actual lines from the tokens because
    ; the code blocks should preserve whatever the user typed
    (let [closing (in/next! stream) ; Discard close fence (if any)
          content-lines (map #(-> % meta :token t/input-value)
                             @!lines)]
      (vswap! !blocks conj
              (vary-meta (doc/code-block (str/trim (:info-string opening))
                                         (str/join "\n" content-lines))
                         assoc :lines (let [lines (vec (cons opening @!lines))]
                                        (if closing
                                          (conj lines closing)
                                          lines)))))))

(defn parse-block! [stream !blocks]
  (let [{:keys [type]} (in/peek stream)]
    (case type
      ::paragraph (parse-paragraph! stream !blocks)
      ::thematic-break (parse-thematic-break! stream !blocks)
      ::atx-heading (parse-atx-heading! stream !blocks)
      ::setext-heading-underline (parse-paragraph! stream !blocks)
      ::indented-code-block (parse-indented-code-block! stream !blocks)
      ::code-fence (parse-fenced-code-block! stream !blocks)
      ::blank (parse-blank-lines! stream !blocks)
      (throw (ex-info (str "Parse error! Type not found: " type) {})))))

(defn parse-blocks [parsed-lines]
  (let [stream (in/make-stream parsed-lines)
        !blocks (volatile! [])]
    (loop []
      (when-not (in/end? stream)
        (parse-block! stream !blocks)
        (recur)))
    @!blocks))

(def line-parser
  (let [newline (pp/or "\r\n" \return \newline)]
    (pp/separated-by (pp/flatten (pp/star (pp/negate newline)))
                     newline)))

(defn split-lines [src]
  (take-nth 2 (pp/parse line-parser src)))

(defn parse [src]
  (let [input-lines (split-lines src)
        parsed-lines (map-indexed (fn [idx line]
                                    (vary-meta (parse-line line)
                                               assoc :line idx))
                                  input-lines)
        blocks (parse-blocks parsed-lines)]
    (apply doc/document blocks)))

(comment

  (def src (slurp "test-files/test.md"))
  (tap> (parse src))

  (tap> parsed-lines)

  (def stream (in/make-stream [1 2 3 4]))

  (+ 3 4)
  (in/next! stream))