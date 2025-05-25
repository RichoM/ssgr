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

(defn make-token
  "Utility function to make a token from the current position of a stream"
  ([stream begin-pos parsed-value]
   (make-token stream
               begin-pos
               (in/position stream)
               parsed-value))
  ([stream begin-pos end-pos parsed-value]
   (t/make-token (in/source stream)
                 begin-pos
                 (- end-pos begin-pos)
                 parsed-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line parsers

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
  ; We try each line parser in order until we find one that matches
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

(defn find-line-idx [string line-number]
  (loop [[line & rest] (str/split string #"\n" line-number)
         line-idx 0
         line-count 1]
    (if (and line (< line-count line-number))
      (recur rest
             (+ line-idx (count line) 1)
             (inc line-count))
      line-idx)))

(defn advance-stream-to-match! [stream reader source]
  (let [^long line-number (e/get-line-number reader)
        ^long column-number (e/get-column-number reader)
        line-position (find-line-idx source line-number)
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
                     assoc :token (make-token stream begin-pos nil)))
        (catch Exception _
          (in/reset-position! stream begin-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline parsers

(defn consume-while! [stream while-fn]
  (loop [result (transient [])]
    (let [chr (in/peek stream)]
      (if (and chr (while-fn chr))
        (recur (conj! result (in/next! stream)))
        (persistent! result)))))

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
          (loop [content (transient [])]
            (let [next-char (in/peek stream)]
              (case next-char
                ; The closing and opening must be of equal length
                \` (let [closing (consume-chars! stream \`)]
                     (if (= (count closing)
                            (count opening))
                       [(persistent! content) closing]
                       (recur (reduce conj! content closing))))

                ; Line endings are converted to spaces, but only if the next line is either
                ; a paragraph or an indented-code-block.
                \newline (do (in/next! stream) ; Discard newline
                             (when (contains? #{::paragraph ::indented-code-block}
                                              (:type (peek-line stream)))
                               ; Discard leading spaces
                               (consume-chars! stream \space \tab)
                               (recur (conj! content \space))))
                \return (do (in/next! stream) ; Discard newline
                            (consume-1-char! stream \newline) ; Discard newline (if any)
                            (when (contains? #{::paragraph ::indented-code-block}
                                             (:type (peek-line stream)))
                              ; Discard leading spaces
                              (consume-chars! stream \space \tab)
                              (recur (conj! content \space))))

                ; Anything else is appended to the content
                (when next-char
                  (recur (conj! content (in/next! stream)))))))
          token (make-token stream
                            begin-pos
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
    (in/next! stream) ; Discard the first bracket
    (let [content
          (loop [bracket-count 0 ; We keep track of the open brackets to make sure
                                 ; they match the closing brackets
                 content (transient [])]
            (when-let [next-char (in/peek stream)]
              (case next-char
                ; If we find an escape character, we check if the following char is
                ; a bracket, in which case we discard the escape char and add the 
                ; bracket to the content list, otherwise we add the escape char
                \\ (let [escape-char (in/next! stream)]
                     (if (contains? #{\[ \]} (in/peek stream))
                       (recur bracket-count
                              (conj! content (in/next! stream)))
                       (recur bracket-count
                              (conj! content escape-char))))

                ; We found an open bracket, we increment the bracket-count, we add
                ; it to the content list, and we keep parsing
                \[ (recur (inc bracket-count)
                          (conj! content (in/next! stream)))

                ; We found a closing bracket, if the bracket-count is zero it means
                ; the open/close brackets are balanced and we can stop parsing. If
                ; the bracket-count is not zero we decrement the bracket-count, we
                ; add the closing bracket to the content list, and keep going
                \] (if (zero? bracket-count)
                     (do (in/next! stream)
                         (persistent! content))
                     (recur (dec bracket-count)
                            (conj! content (in/next! stream))))

                ; Any other character is simply added to the content list 
                (recur bracket-count
                       (conj! content (in/next! stream))))))]
      (when content
        (parse-inlines! (in/make-stream (str/join content)) ; TODO(Richo): avoid new stream
                        :skip-links? true
                        :skip-clojure? true)))))

(defn parse-link-destination! [stream]
  (when (= \( (in/peek stream))
    (in/next! stream) ; Discard the first bracket
    (let [content
          (loop [bracket-count 0 ; We keep track of the open brackets to make sure
                                 ; they match the closing brackets
                 content (transient [])]
            (when-let [next-char (in/peek stream)]
              (case next-char
                ; If we find an escape character, we check if the following char is
                ; a bracket, in which case we discard the escape char and add the 
                ; bracket to the content list, otherwise we add the escape char
                \\ (let [escape-char (in/next! stream)]
                     (if (contains? #{\( \)} (in/peek stream))
                       (recur bracket-count
                              (conj! content (in/next! stream)))
                       (recur bracket-count
                              (conj! content escape-char))))

                ; We found an open bracket, we increment the bracket-count, we add
                ; it to the content list, and we keep parsing
                \( (recur (inc bracket-count)
                          (conj! content (in/next! stream)))

                ; We found a closing bracket, if the bracket-count is zero it means
                ; the open/close brackets are balanced and we can stop parsing. If
                ; the bracket-count is not zero we decrement the bracket-count, we
                ; add the closing bracket to the content list, and keep going
                \) (if (zero? bracket-count)
                     (do (in/next! stream)
                         (persistent! content))
                     (recur (dec bracket-count)
                            (conj! content (in/next! stream))))

                ; Any other character is simply added to the content list 
                (recur bracket-count
                       (conj! content (in/next! stream))))))]
      (when content (str/join content)))))

(defn parse-link! [stream]
  (let [begin-pos (in/position stream)
        link-text (parse-link-text! stream)
        link-destination (parse-link-destination! stream)]
    (if (and link-text link-destination)
      (vary-meta (doc/link link-text link-destination)
                 assoc :token (make-token stream
                                          begin-pos
                                          [link-text link-destination]))
      (do (in/reset-position! stream begin-pos)
          nil))))

(defn parse-image! [stream]
  (when (= \! (in/peek stream))
    (let [begin-pos (in/position stream)
          bang (in/next! stream) ; Skip the bang before parsing the rest as a link
          img-description (parse-link-text! stream)
          img-src (parse-link-destination! stream)]
      (if (and img-description img-src)
        (vary-meta (apply doc/image img-src img-description)
                   assoc :token (make-token stream
                                            begin-pos
                                            [bang img-description img-src]))
        (do (in/reset-position! stream begin-pos)
            nil)))))

(defn parse-escaped-characters! [stream & chars]
  ; If we find any of the chars preceded by a backslash (\) we return them, 
  ; otherwise we leave the stream untouched and return nil
  (let [begin-pos (in/position stream)]
    (when (= \\ (in/peek stream))
      (let [result (in/next! stream)]
        (if (contains? (set chars) (in/peek stream))
          result
          (do (in/reset-position! stream begin-pos)
              nil))))))

(defn condj! [v val]
  (if val (conj! v val) v))

(defn parse-inline!
  [stream
   & {:keys [skip-clojure? skip-code-spans? skip-links? skip-images?]}]
  (let [; This close-text fn will make a text element if we pass two valid 
        ; indices. Keep in mind we might call close-text with nil if we 
        ; didn't have a pending text open already, in which case it just
        ; returns nil.
        close-text (fn [text-begin text-end]
                     (let [text-token (when text-begin
                                        (make-token stream
                                                    text-begin
                                                    text-end
                                                    nil))]
                       (when text-token
                         (vary-meta (doc/text (t/input-value text-token))
                                    assoc :token text-token))))]
    (loop [text-begin nil ; Marks the beginning of a pending text element, we
                          ; need to close this pending text and add it to the
                          ; elements list before adding another element
           elements (transient [])]
      (let [begin-pos (in/position stream)]
        (cond
          ; If we find a newline or reach end of stream, we simply close the 
          ; pending text (if any) and stop parsing
          (r/success? (pp/parse-on newline-or-end stream))
          (persistent! (condj! elements (close-text text-begin begin-pos)))

          ; Then, we try to match escaped characters, if we find them we first
          ; close the pending text and open a new one (skipping the backslash) 
          (parse-escaped-characters! stream \( \[)
          (do (in/next! stream)
              (recur (dec (in/position stream))
                     (condj! elements (close-text text-begin begin-pos))))

          ; Then we try to match special inlines (clojure, code-spans, links, 
          ; images, etc). If we find any of them, we first close the pending 
          ; text (if any) and add the special inline to the elements list. 
          ; If no special inline is found we mark the current stream position 
          ; as the beginning of a text element (but only if we didn't have a
          ; pending text already open)
          :else (let [text-end (in/position stream)
                      element (or (when-not skip-clojure? (parse-clojure! stream))
                                  (when-not skip-code-spans? (parse-code-span! stream))
                                  (when-not skip-links? (parse-link! stream))
                                  (when-not skip-images? (parse-image! stream)))]
                  (if element
                    (recur nil
                           (-> elements
                               (condj! (close-text text-begin text-end))
                               (conj! element)))
                    (do (in/next! stream)
                        (recur (or text-begin text-end) 
                               elements)))))))))

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
        make-token (fn [lines] (make-token stream begin-pos lines))
        make-paragraph (fn [lines]
                         (vary-meta (apply doc/paragraph (vec (apply concat lines)))
                                    assoc :token (make-token lines)))
        make-heading (fn [level lines]
                       (vary-meta (apply doc/heading level
                                         (->> lines
                                              (interpose [(doc/text "\n")])
                                              (apply concat)
                                              (vec)))
                                  assoc :token (make-token lines)))]
    (loop [lines (transient [])]
      (let [{:keys [type] :as next-line} (peek-line stream)]
        (case type
          ::paragraph
          (let [inlines (parse-inline! stream)]
            (if (seq inlines)
              (recur (conj! lines inlines))
              (make-paragraph (persistent! lines))))

          ; If we find setext-heading-underline, we convert the whole 
          ; paragraph to a heading
          ::setext-heading-underline
          (let [{:keys [chars]} (next-line! stream)]
            (make-heading (if (= \- (first chars)) 2 1)
                          (persistent! lines)))

          ; Thematic breaks can be confused with setext-headings, in
          ; which case the setext-heading takes precedence
          ::thematic-break
          (if (= \- (first (:chars next-line)))
            (do (next-line! stream) ; discard next line
                (make-heading 2 (persistent! lines)))
            (make-paragraph (persistent! lines)))

          ; Indented code blocks can't interrupt a paragraph, so if
          ; we found one we just treat it as a valid line
          ::indented-code-block
          (let [inlines (parse-inline! stream)]
            (if (seq inlines)
              (recur (conj! lines inlines))
              (make-paragraph (persistent! lines))))

          ; Anything else, simply breaks the paragraph, we do nothing
          (make-paragraph (persistent! lines)))))))


(defn parse-thematic-break! [stream]
  (let [begin-pos (in/position stream)
        line (next-line! stream)]
    (vary-meta (doc/thematic-break)
               assoc :token (make-token stream begin-pos line))))

(defn parse-atx-heading! [stream]
  (let [begin-pos (in/position stream)
        {:keys [level content-start] :as line} (next-line! stream)]
    ; We reset the stream to the beginning of the content (skipping all the #)
    (in/reset-position! stream content-start)
    ; Skip any spaces or tabs before parsing the inline content
    (consume-chars! stream \space \tab)
    (let [inlines (parse-inline! stream)]
      (vary-meta (apply doc/heading level inlines)
                 assoc :token (make-token stream begin-pos line)))))

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
        lines (loop [lines (transient [])]
                (let [{:keys [type]} (peek-line stream)]
                  (if (= ::indented-code-block type)
                    (recur (conj! lines (next-line! stream)))
                    (persistent! lines))))
        line-contents (map #(-> % meta :token t/input-value (subs 4))
                           lines)]
    (vary-meta (doc/code-block "" (str/join line-contents))
               assoc :token (make-token stream begin-pos lines))))

(defn parse-fenced-code-block! [stream]
  (let [begin-pos (in/position stream)
        opening (next-line! stream)
        lines (loop [lines (transient [])]
                (if-let [{:keys [type] :as next} (peek-line stream)]
                  (if-not (and (= ::code-fence type)
                               (str/blank? (:info-string next))
                               (= (-> opening :chars first)
                                  (-> next :chars first))
                               (<= (-> opening :chars count)
                                   (-> next :chars count)))
                    (recur (conj! lines (next-line! stream)))
                    (persistent! lines))
                  (persistent! lines)))

        ; We may or may not have a closing fence
        closing (when (= ::code-fence (:type (peek-line stream)))
                  (next-line! stream))

        ; NOTE(Richo): We take the actual lines from the tokens because
        ; the code blocks should preserve whatever the user typed
        line-contents (map #(-> % meta :token t/input-value)
                           lines)]
    (vary-meta (doc/code-block (str/trim (:info-string opening))
                               (str/join line-contents))
               assoc :token (make-token stream begin-pos
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
  (loop [blocks (transient [])]
    (if-not (in/end? stream)
      (if-let [next-block (parse-block! stream)]
        (recur (conj! blocks next-block))
        (recur blocks))
      (persistent! blocks))))

(defn parse [src]
  (let [stream (in/make-stream src)
        blocks (parse-blocks! stream)]
    (vary-meta (apply doc/document blocks)
               assoc :token (t/make-token src 0 (count src) nil))))

(comment
  (parse (slurp "test-files/intro.md"))
  (tap> *1)
  )