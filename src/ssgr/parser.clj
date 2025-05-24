(ns ssgr.parser
  (:require [clojure.string :as str]
            [petitparser.input-stream :as in]
            [petitparser.core :as pp]
            [petitparser.results :as r]
            [petitparser.token :as t]
            [edamame.core :as e]
            [hiccup.compiler :as h.c]
            [ssgr.doc :as doc]
            [ssgr.eval :refer [eval-form]]))

(def newline-parser (pp/or "\r\n" \return \newline))

(defrecord NewlineOrEndParser []
  petitparser.parsers.Parser
  (parse-on [_self stream]
    (if (in/end? stream)
      (r/success ::end)
      (pp/parse-on newline-parser stream))))

(def newline-or-end (NewlineOrEndParser.))

(def space (pp/or \space \tab))

(defn transform-with-token [p f]
  (pp/transform (pp/token p)
                (fn [token]
                  (vary-meta (f (t/parsed-value token))
                             assoc :token token))))

(def thematic-break
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/or (pp/min \- 3)
                  (pp/min \_ 3)
                  (pp/min \* 3))
           (pp/star space)
           newline-or-end)
   (fn [[_ chars]]
     {:type ::thematic-break
      :chars chars})))

(def atx-heading
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/times \# 1 6)
           (pp/token
            (pp/optional (pp/seq space
                                 (pp/plus (pp/negate newline-or-end)))))
           newline-or-end)
   (fn [[_ level inline-token]]
     {:type ::atx-heading
      :level (count level)
      :content (t/input-value inline-token)
      :content-start (t/start inline-token)})))

(def setext-heading-underline
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/or (pp/plus \-)
                  (pp/plus \=))
           (pp/star space)
           newline-or-end)
   (fn [[_ chars]]
     {:type ::setext-heading-underline
      :chars chars})))

(def indented-code-block
  (transform-with-token
   (pp/seq (pp/flatten (pp/seq (pp/min space 4)
                               (pp/plus (pp/negate pp/space))
                               (pp/star (pp/negate newline-or-end))))
           newline-or-end)
   (fn [inline-text]
     {:type ::indented-code-block
      :content inline-text})))

(def code-fence
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/or (pp/min \` 3)
                  (pp/min \~ 3))
           (pp/flatten (pp/star (pp/negate newline-or-end)))
           newline-or-end)
   (fn [[_ chars info-string]]
     {:type ::code-fence
      :chars chars
      :info-string info-string})))

(def blank (transform-with-token (pp/seq (pp/star space)
                                         newline-parser)
                                 (constantly {:type ::blank})))

(def paragraph
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/flatten [(pp/plus (pp/negate pp/space))
                        (pp/star (pp/negate newline-or-end))])
           newline-or-end)
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

(defn next-line! [stream]
  (let [parsers [thematic-break atx-heading
                 setext-heading-underline
                 indented-code-block code-fence
                 blank paragraph]]
    (loop [[parser & rest] parsers]
      (let [result (pp/parse-on parser stream)]
        (if (r/failure? result)
          (when rest (recur rest))
          (r/actual-result result))))))

(defn peek-line [stream]
  (let [begin-pos (in/position stream)
        result (next-line! stream)]
    (in/reset-position! stream begin-pos)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure parser

(defn line-indices [string]
  (loop [[line & rest] (str/split string #"\n")
         start 0
         indices (transient [])]
    (if line
      (let [count (count line)
            stop (+ start count)]
        (recur
         rest
         (inc stop)
         (conj! indices [start stop])))
      (persistent! indices))))

(defn advance-stream-to-match! [stream reader source]
  (let [^long line-number (e/get-line-number reader)
        ^long column-number (e/get-column-number reader)
        [line-position] (nth (line-indices source)
                             (dec line-number))
        ^long position (+ (in/position stream)
                          line-position
                          (dec column-number))]
    (in/reset-position! stream position)))

(defn eval-clojure [form]
  (let [result (eval-form form)]
    (if (vector? form)
      (h.c/normalize-element result)
      result)))

(defn parse-clojure! [stream]
  (let [begin-pos (in/position stream)]
    (when (#{\( \[} (in/peek stream))
      (try
        (let [src (subs (in/source stream) begin-pos)
              reader (e/source-reader src)
              form (e/parse-next reader (e/normalize-opts {:all true}))
              result (eval-clojure form)]
          (advance-stream-to-match! stream reader src)
          (vary-meta (doc/clojure form result)
                     assoc :token (t/make-token (in/source stream)
                                                begin-pos
                                                (- (in/position stream) begin-pos)
                                                nil)))
        (catch Exception _
          (in/reset-position! stream begin-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline parsers

(defn consume-while! [stream while-fn]
  (loop [result []]
    (let [chr (in/peek stream)]
      (if (and chr (while-fn chr))
        (recur (conj result (in/next! stream)))
        result))))

(defn consume-chars! [stream & chars]
  (consume-while! stream (set chars)))

(defn consume-1-char! [stream char]
  (let [next (in/peek stream)]
    (when (= char next)
      (in/next! stream))))

(defn parse-code-span! [stream]
  (when (= \` (in/peek stream))
    (let [begin-pos (in/position stream)
          opening (consume-chars! stream \`)
          begin-content (in/position stream)
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
                             (when (= ::paragraph (:type (peek-line stream)))
                               (recur (conj content \space))))
                \return (do (in/next! stream) ; Discard
                            (consume-1-char! stream \newline)
                            (when (= ::paragraph (:type (peek-line stream)))
                              (recur (conj content \space))))

                ; Anything else is appended to the content
                (when next-char
                  (recur (conj content (in/next! stream)))))))
          end-pos (in/position stream)
          token (t/make-token (in/source stream)
                              begin-pos
                              (- end-pos begin-pos)
                              [opening content closing])]
      (if content
        (vary-meta (doc/code-span (let [text (str/join content)]
                                    ; If the resulting string both begins and ends 
                                    ; with a space character, but does not consist 
                                    ; entirely of space characters, a single space 
                                    ; character is removed from the front and back
                                    (if (and (str/starts-with? text " ")
                                             (str/ends-with? text " ")
                                             (not (str/blank? text)))
                                      (subs text 1 (dec (count text)))
                                      text)))
                   assoc :token token)
        (do (in/reset-position! stream begin-content)
            (doc/text (str/join opening)))))))

(declare parse-inlines!)

(defn parse-link-text! [stream]
  (when (= \[ (in/peek stream))
    (let [[content closed?]
          (loop [bracket-count 0
                 prev-char (in/next! stream)
                 content []]
            (let [next-char (in/peek stream)]
              (case next-char
                \\ (recur bracket-count
                          (in/next! stream)
                          content)
                \[ (recur (if (= \\ prev-char)
                            bracket-count
                            (inc bracket-count))
                          (in/next! stream)
                          (conj content next-char))
                \] (if (= \\ prev-char)
                     (recur bracket-count
                            (in/next! stream)
                            (conj content next-char))
                     (if (zero? bracket-count)
                       (do (in/next! stream)
                           [content true])
                       (recur (dec bracket-count)
                              (in/next! stream)
                              (conj content next-char))))
                nil [content false]
                (recur bracket-count
                       (in/next! stream)
                       (conj content next-char)))))]
      (when closed?
        (parse-inlines! (in/make-stream (str/join content))
                        :allow-links? false
                        :allow-clojure? false)))))

(defn parse-link-destination! [stream]
  (when (= \( (in/peek stream))
    (let [[content closed?]
          (loop [bracket-count 0
                 prev-char (in/next! stream)
                 content []]
            (let [next-char (in/peek stream)]
              (case next-char
                \\ (recur bracket-count
                          (in/next! stream)
                          content)
                \( (recur (if (= \\ prev-char)
                            bracket-count
                            (inc bracket-count))
                          (in/next! stream)
                          (conj content next-char))
                \) (if (= \\ prev-char)
                     (recur bracket-count
                            (in/next! stream)
                            (conj content next-char))
                     (if (zero? bracket-count)
                       (do (in/next! stream)
                           [content true])
                       (recur (dec bracket-count)
                              (in/next! stream)
                              (conj content next-char))))
                nil [content false]
                (recur bracket-count
                       (in/next! stream)
                       (conj content next-char)))))]
      (when closed? (str/join content)))))

(defn parse-link! [stream]
  (let [begin-pos (in/position stream)
        link-text (parse-link-text! stream)
        link-destination (parse-link-destination! stream)
        end-pos (in/position stream)]
    (if (and link-text link-destination)
      (vary-meta (doc/link link-text link-destination)
                 assoc :token (t/make-token (in/source stream)
                                            begin-pos
                                            (- end-pos begin-pos)
                                            [link-text link-destination]))
      (do (in/reset-position! stream begin-pos)
          nil))))

(defn parse-escaped-characters! [stream]
  (let [begin-pos (in/position stream)]
    (when (= \\ (in/peek stream))
      (let [result (in/next! stream)]
        (if (contains? #{\( \[} (in/peek stream))
          result
          (do (in/reset-position! stream begin-pos)
              nil))))))

(defn condj [v val]
  (if val (conj v val) v))

(defn parse-inline!
  [stream
   & {:keys [allow-links? allow-clojure?]
      :or {allow-links? true, allow-clojure? true}}]
  (let [close-text (fn [text-begin text-end]
                     (let [text-token (when text-begin
                                        (t/make-token (in/source stream)
                                                      text-begin
                                                      (- text-end text-begin)
                                                      nil))]
                       (when text-token
                         (vary-meta (doc/text (t/input-value text-token))
                                    assoc :token text-token))))]
    (loop [elements []
           text-begin nil]
      (let [begin-pos (in/position stream)]
        (if (r/success? (pp/parse-on newline-or-end stream))
          (condj elements (close-text text-begin begin-pos))
          (if (parse-escaped-characters! stream)
            (do (in/next! stream)
                (recur (condj elements (close-text text-begin begin-pos))
                       (dec (in/position stream))))
            (let [text-end (in/position stream)
                  element (or (when allow-clojure? (parse-clojure! stream))
                              (parse-code-span! stream)
                              (when allow-links? (parse-link! stream)))]
              (if element
                (recur (-> elements
                           (condj (close-text text-begin text-end))
                           (conj element))
                       nil)
                (do (in/next! stream)
                    (recur elements (or text-begin text-end)))))))))))

(defn parse-inlines! [stream & options]
  (loop [inlines []]
    (let [next-inline (parse-inline! stream options)]
      (if (seq next-inline)
        (recur (concat inlines next-inline))
        (vec inlines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block parsers

(defn parse-paragraph! [stream]
  (let [begin-pos (in/position stream)
        make-token (fn [lines] (t/make-token (in/source stream)
                                             begin-pos
                                             (- (in/position stream) begin-pos)
                                             lines))
        make-paragraph (fn [lines] (vary-meta (apply doc/paragraph (vec (apply concat lines)))
                                              assoc :token (make-token lines)))
        make-heading (fn [level lines]
                       (vary-meta (apply doc/heading level
                                         (->> lines
                                              (interpose [(doc/text "\n")])
                                              (apply concat)
                                              (vec)))
                                  assoc :token (make-token lines)))]
    (loop [lines []]
      (let [{:keys [type] :as next-line} (peek-line stream)]
        (case type
          ::paragraph
          (let [inlines (parse-inline! stream)]
            (if (seq inlines)
              (recur (conj lines inlines))
              (make-paragraph lines)))

          ; If we find setext-heading-underline, we convert the whole 
          ; paragraph to a heading
          ::setext-heading-underline
          (let [{:keys [chars]} (next-line! stream)]
            (make-heading (if (= \- (first chars)) 2 1)
                          lines))

          ; Thematic breaks can be confused with setext-headings, in
          ; which case the setext-heading takes precedence
          ::thematic-break
          (if (= \- (first (:chars next-line)))
            (do (next-line! stream) ; discard next line
                (make-heading 2 lines))
            (make-paragraph lines))

          ; Indented code blocks can't interrupt a paragraph, so if
          ; we found one we just treat it as a valid line
          ::indented-code-block
          (let [inlines (parse-inline! stream)]
            (if (seq inlines)
              (recur (conj lines inlines))
              (make-paragraph lines)))

          ; Anything else, simply breaks the paragraph, we do nothing
          (make-paragraph lines))))))


(defn parse-thematic-break! [stream]
  (let [begin-pos (in/position stream)
        line (next-line! stream)]
    (vary-meta (doc/thematic-break)
               assoc :token (t/make-token (in/source stream)
                                          begin-pos
                                          (- (in/position stream) begin-pos)
                                          line))))

(defn parse-atx-heading! [stream]
  (let [begin-pos (in/position stream)
        {:keys [level content-start] :as line} (next-line! stream)]
    ; We reset the stream to the beginning of the content
    (in/reset-position! stream content-start)
    (consume-chars! stream \space \tab)
    (let [inlines (parse-inline! stream)]
      (vary-meta (apply doc/heading level inlines)
                 assoc :token (t/make-token (in/source stream)
                                            begin-pos
                                            (- (in/position stream) begin-pos)
                                            line)))))

(defn parse-blank-lines! [stream]
  (loop []
    (when (= ::blank (:type (peek-line stream)))
      (next-line! stream)
      (recur))))

(defn parse-indented-code-block! [stream]
  ; NOTE(Richo): We take the actual lines from the tokens because
  ; the code blocks should preserve whatever the user typed. 
  ; However, since we're parsing an indented block code, we need 
  ; to remove the first 4 indentation spaces.
  (let [begin-pos (in/position stream)
        lines (loop [lines []]
                (let [{:keys [type]} (peek-line stream)]
                  (if (= ::indented-code-block type)
                    (recur (conj lines (next-line! stream)))
                    lines)))
        line-contents (map #(-> % meta :token t/input-value (subs 4))
                           lines)]
    (vary-meta (doc/code-block "" (str/join line-contents))
               assoc :token (t/make-token (in/source stream)
                                          begin-pos
                                          (- (in/position stream) begin-pos)
                                          lines))))

(defn parse-fenced-code-block! [stream]
  (let [begin-pos (in/position stream)
        opening (next-line! stream)
        lines (loop [lines []]
                (if-let [{:keys [type] :as next} (peek-line stream)]
                  (if-not (and (= ::code-fence type)
                               (str/blank? (:info-string next))
                               (= (-> opening :chars first)
                                  (-> next :chars first))
                               (<= (-> opening :chars count)
                                   (-> next :chars count)))
                    (recur (conj lines (next-line! stream)))
                    lines)
                  lines))

        ; We may or may not have a closing fence
        closing (when (= ::code-fence (:type (peek-line stream)))
                  (next-line! stream))

        ; NOTE(Richo): We take the actual lines from the tokens because
        ; the code blocks should preserve whatever the user typed
        line-contents (map #(-> % meta :token t/input-value)
                           lines)]
    (vary-meta (doc/code-block (str/trim (:info-string opening))
                               (str/join line-contents))
               assoc :token (t/make-token (in/source stream)
                                          begin-pos
                                          (- (in/position stream) begin-pos)
                                          [opening lines closing]))))

(defn parse-block! [stream]
  (let [{:keys [type]} (peek-line stream)]
    (case type
      ::paragraph (parse-paragraph! stream)
      ::thematic-break (parse-thematic-break! stream)
      ::atx-heading (parse-atx-heading! stream)
      ::setext-heading-underline (parse-paragraph! stream)
      ::indented-code-block (parse-indented-code-block! stream)
      ::code-fence (parse-fenced-code-block! stream)
      ::blank (parse-blank-lines! stream)
      (throw (ex-info (str "Parse error! Type not found: " type) {})))))

(defn parse-blocks! [stream]
  (loop [blocks []]
    (if-not (in/end? stream)
      (if-let [next-block (parse-block! stream)]
        (recur (conj blocks next-block))
        (recur blocks))
      blocks)))

(defn parse [src]
  (let [stream (in/make-stream src)
        blocks (parse-blocks! stream)]
    (vary-meta (apply doc/document blocks)
               assoc :token (t/make-token src 0 (count src) nil))))

(comment
  (parse "texto `abc` texto")
  (tap> *1)
  )