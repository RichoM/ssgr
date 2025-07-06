(ns ssgr.parser
  (:require [clojure.string :as str]
            [petitparser.input-stream :as in]
            [petitparser.core :as pp]
            [petitparser.results :as r]
            [ssgr.token :as t :refer [*debug-verbose-tokens* *parser-file*]]
            [edamame.core :as e]
            [hiccup.compiler :as h.c]
            [ssgr.doc :as doc]
            [ssgr.eval :refer [eval-form]]))

(def ^:dynamic *debug-verbose-emphasis* false)
(def ^:dynamic *verbose-eval* false)

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
                  (t/with-token
                    (f (t/parsed-value token))
                    token))))

(def bullet-list-marker 
  (pp/transform (pp/or \- \+ \*)
                (fn [char]
                  {:type ::bullet-list-marker
                   :digits "" ; NOTE(Richo): Same interface as ordered-list-markers
                   :char char})))

(def ordered-list-marker
  (pp/transform (pp/seq (pp/flatten (pp/times pp/digit 1 9))
                        (pp/or \. \)))
                (fn [[digits delimiter]]
                  {:type ::ordered-list-marker
                   :digits digits
                   :char delimiter})))

(def list-item
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/or bullet-list-marker
                  ordered-list-marker)
           (pp/times space 1 4))
   (fn [[_ list-marker spaces]]
     {:type ::list-item
      :spaces (count spaces)
      :marker list-marker})))

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

(defn next-line! [stream ctx]
  ; If we have a line-prefix parser, we use it to consume the stream, the result
  ; doesn't matter, we just discard it (I don't know if this is correct, though)
  (when-let [line-prefix (-> ctx :line-prefix)]
    (pp/parse-on line-prefix stream))
  ; We try each line parser in order until we find one that matches
  (let [parsers [list-item
                 thematic-break atx-heading
                 setext-heading-underline
                 indented-code-block code-fence
                 blank paragraph]]
    (loop [[parser & rest] parsers]
      (let [result (pp/parse-on parser stream)]
        (if (r/failure? result)
          (when rest (recur rest))
          (r/actual-result result))))))

(defn peek-line [stream ctx]
  (let [begin-pos (in/position stream)
        result (next-line! stream ctx)]
    (in/reset-position! stream begin-pos)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure parser

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

(defn advance-stream-to-match! [stream reader source]
  (let [^long line-number (e/get-line-number reader)
        ^long column-number (e/get-column-number reader)
        line-position (find-line-idx source line-number)
        position (+ (in/position stream)
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
          (t/with-token (doc/clojure form result)
            (t/stream->token stream begin-pos nil)))
        (catch Exception ex
          (when *verbose-eval*
            (let [[line-number column-number]
                  (find-line-col (in/source stream)
                                 begin-pos)]
              (println "ERROR evaluating clojure code at"
                       *parser-file* (str "(Ln " line-number ", Col " column-number "):")
                       (ex-message ex))))
          (in/reset-position! stream begin-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline parsers

(def ^:private unicode-punctuation-character?
  "A Unicode punctuation character is a character in the Unicode P (puncuation) 
   or S (symbol) general categories."
  (comp #{Character/DASH_PUNCTUATION
          Character/END_PUNCTUATION
          Character/FINAL_QUOTE_PUNCTUATION
          Character/INITIAL_QUOTE_PUNCTUATION
          Character/OTHER_PUNCTUATION
          Character/START_PUNCTUATION
          Character/CURRENCY_SYMBOL
          Character/MATH_SYMBOL
          Character/MODIFIER_SYMBOL
          Character/OTHER_SYMBOL}
        #(Character/getType ^char %)))

(defn- unicode-whitespace-character?
  "A Unicode whitespace character is a character in the Unicode Zs general category, 
   or a tab (U+0009), line feed (U+000A), form feed (U+000C), or carriage return (U+000D)."
  [chr]
  (or (= Character/SPACE_SEPARATOR (Character/getType ^char chr))
      (contains? #{\u0009 \u000A \u000C \u000D} chr)))

(defn parse! [parser stream]
  (let [result (pp/parse-on parser stream)]
    (when (r/success? result)
      (r/actual-result result))))

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

(def punctuation-chars (set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(defn parse-escaped-characters! [stream]
  ; If we find any punctuation character preceded by a backslash (\) we return 
  ; them, otherwise we leave the stream untouched and return nil
  (let [begin-pos (in/position stream)]
    (when (= \\ (in/peek stream))
      (let [result (in/next! stream)]
        (if (punctuation-chars (in/peek stream))
          result
          (do (in/reset-position! stream begin-pos)
              nil))))))

(defn parse-code-span! [stream ctx]
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
                                              (:type (peek-line stream ctx)))
                               ; Discard leading spaces
                               (consume-chars! stream \space \tab)
                               (recur (conj! content \space))))
                \return (do (in/next! stream) ; Discard newline
                            (consume-1-char! stream \newline) ; Discard newline (if any)
                            (when (contains? #{::paragraph ::indented-code-block}
                                             (:type (peek-line stream ctx)))
                              ; Discard leading spaces
                              (consume-chars! stream \space \tab)
                              (recur (conj! content \space))))

                ; Anything else is appended to the content
                (when next-char
                  (recur (conj! content (in/next! stream)))))))
          token (t/stream->token stream
                                 begin-pos
                                 [opening content closing])]
      (if content
        (t/with-token
          (doc/code-span (let [text (str/join content)]
                                    ; If the resulting string both begins and ends 
                                    ; with a space character, but does not consist 
                                    ; entirely of space characters, a single space 
                                    ; character is removed from the front and back
                           (if (and (str/starts-with? text " ")
                                    (str/ends-with? text " ")
                                    (not (str/blank? text)))
                             (subs text 1 (dec (count text)))
                             text)))
          token)
        (do (in/reset-position! stream begin-content)
            (doc/text (str/join opening)))))))

(declare parse-inlines!)

(defn condj [v val]
  (if val (conj v val) v))

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

(def line-break-parser (pp/seq (pp/star \space)
                               (pp/optional \\)
                               newline-parser))

(defn parse-line-breaks! [stream]
  (parse! line-break-parser stream))

(defn peek-at [stream pos]
  (nth (in/source stream) pos nil))

(defn append-next! [inline-text stream]
  (if inline-text
    (-> inline-text
        (update :text #(str % (in/next! stream)))
        (assoc :stop (in/position stream)))
    {:stream stream
     :start (in/position stream)
     :stop (inc (in/position stream))
     :text (str (in/next! stream))}))

(defn append-text [inline-text stream chars ^long stop]
  (if inline-text
    (-> inline-text
        (update :text #(str % (apply str chars)))
        (assoc :stop stop))
    {:stream stream
     :start (- stop (count chars))
     :stop stop
     :text (apply str chars)}))

(defn close-text [inline-text]
  (when-let [{:keys [stream ^long start ^long stop text]} inline-text]
    (when (> stop start)
      (t/with-token (doc/text text)
        (t/stream->token stream start stop text)))))

(defn- can-open?
  "A left-flanking delimiter run is a delimiter run that is (1) not followed 
     by Unicode whitespace, and either (2a) not followed by a Unicode punctuation 
     character, or (2b) followed by a Unicode punctuation character and preceded 
     by Unicode whitespace or a Unicode punctuation character. For purposes of this
     definition, the beginning and the end of the line count as Unicode whitespace."
  [prev-char next-char]
  (and (not (unicode-whitespace-character? next-char))
       (or (not (unicode-punctuation-character? next-char))
           (unicode-whitespace-character? prev-char)
           (unicode-punctuation-character? prev-char))))

(defn- can-close?
  "A right-flanking delimiter run is a delimiter run that is (1) not preceded 
     by Unicode whitespace, and either (2a) not preceded by a Unicode punctuation 
     character, or (2b) preceded by a Unicode punctuation character and followed 
     by Unicode whitespace or a Unicode punctuation character. For purposes of this 
     definition, the beginning and the end of the line count as Unicode whitespace."
  [prev-char next-char]
  (and (not (unicode-whitespace-character? prev-char))
       (or (not (unicode-punctuation-character? prev-char))
           (unicode-whitespace-character? next-char)
           (unicode-punctuation-character? next-char))))

(defn parse-emph-delimiter! [stream char]
  (let [prev-char (or (peek-at stream (dec (in/position stream))) \space)
        chars (consume-chars! stream char)
        next-char (or (in/peek stream) \space)]
    {:type ::delimiter
     :text (str/join chars)
     :open? (can-open? prev-char next-char)
     :close? (can-close? prev-char next-char)
     :prev-char prev-char
     :next-char next-char}))

(defn parse-delimiter!
  "When we’re parsing inlines and we hit either a run of * or _ characters, or a [ or ![
   we insert a text node with these symbols as its literal content, and we add a pointer
   to this text node to the delimiter stack."
  [stream]
  (let [begin-pos (in/position stream)]
    (when-let [delimiter
               (case (in/peek stream)
                 \* (parse-emph-delimiter! stream \*)
                 \_ (parse-emph-delimiter! stream \_)
                 \! (do (in/next! stream)
                        (if (= \[ (in/peek stream))
                          (do (in/next! stream)
                              {:type ::delimiter
                               :text "!["
                               :open? true
                               :close? false})
                          (do (in/reset-position! stream begin-pos)
                              nil)))
                 \[ (do (in/next! stream)
                        {:type ::delimiter
                         :text "["
                         :open? true
                         :close? false})
                 \] (do (in/next! stream)
                        {:type ::delimiter
                         :text "]"
                         :open? false
                         :close? true})
                 nil)]
      (t/with-token
        delimiter
        (t/stream->token stream begin-pos (:text delimiter))))))

(defn split-delimiter-at
  [{:keys [text] :as delimiter} ^long n]
  (let [token (-> delimiter meta :token)
        l-text (subs text 0 n)
        r-text (subs text n)]
    [(t/with-token
       (assoc delimiter :text l-text)
       (when token
         (t/make-token (t/source token)
                       (t/start token)
                       n
                       l-text)))
     (t/with-token
       (assoc delimiter :text r-text)
       (when token
         (t/make-token (t/source token)
                       (+ ^long (t/start token) n)
                       (- (count text) n)
                       r-text)))]))

(defn- find-last-index ^long [items pred]
  (loop [idx (dec (count items))]
    (if (>= idx 0)
      (if (pred (nth items idx))
        idx
        (recur (dec idx)))
      -1)))

(defn delimiter->text [delimiter]
  (t/with-token (doc/text (:text delimiter))
    (-> delimiter meta :token)))

(defn- ensure-no-delimiter-left-behind [inlines] ; TODO(Richo): This shouldn't be necessary!
  (->> inlines
       (keep (fn [{:keys [type text] :as inline}]
               (if (= ::delimiter type)
                 (if (= "" text)
                   nil
                   (delimiter->text inline))
                 inline)))
       (vec)))

(defn- find-first-closer
  ([inlines] (find-first-closer inlines 0))
  ([inlines ^long current-pos]
   (loop [idx current-pos]
     (when-let [{:keys [type] :as inline} (nth inlines idx nil)]
       (if (and (= ::delimiter type)
                (:close? inline)
                (#{\* \_} (first (:text inline)))
                (or (= \* (first (:text inline)))
                    (not (:open? inline))
                    (unicode-punctuation-character? (:next-char inline))))
         idx
         (recur (inc idx)))))))

(defn- find-potential-opener [inlines ^long closer-idx ^long openers-bottom]
  (let [closer (nth inlines closer-idx)
        closer-char (first (:text closer))
        closer-count (-> closer :text count)]
    (loop [idx (dec closer-idx)]
      (when (>= idx openers-bottom)
        (when-let [opener (nth inlines idx nil)]
          (let [opener-char (first (:text opener))
                opener-count (-> opener :text count)
                odd-match? (and (or (:open? closer)
                                    (:close? opener))
                                (not= opener-count closer-count)
                                (zero? ^long (mod (+ opener-count closer-count) 3))
                                (or (pos? ^long (mod opener-count 3))
                                    (pos? ^long (mod closer-count 3))))]
            (if (and (= ::delimiter (:type opener))
                     (:open? opener)
                     (= closer-char opener-char)
                     (not odd-match?)
                     (or (= \* opener-char)
                         (not (:close? opener))
                         (unicode-punctuation-character? (:prev-char opener))))
              idx
              (recur (dec idx)))))))))

(defn next-emphasis-group [inlines current-pos openers-bottom]
  (if-let [closer-idx (find-first-closer inlines current-pos)]
    (if-let [opener-idx (find-potential-opener inlines closer-idx openers-bottom)]
      [opener-idx closer-idx]
      [-1 closer-idx])
    [-1 -1]))

(defn- process-emphasis [inlines]
  (ensure-no-delimiter-left-behind
   (loop [current-pos 0
          openers-bottom 0
          inlines inlines]
     (when *debug-verbose-emphasis*
       (println "")
       (println "(def current-pos" current-pos ")")
       (println "(def openers-bottom" openers-bottom ")")
       (println "(def inlines" (pr-str inlines) ")"))
     (if (>= current-pos (count inlines))
       inlines
       (let [[^long opener-idx ^long closer-idx]
             (next-emphasis-group inlines current-pos openers-bottom)]
         (when *debug-verbose-emphasis*
           (println [opener-idx closer-idx]))
         (cond
           ; We found both a closer and an opener
           (and (>= opener-idx 0)
                (>= closer-idx 0))
           (let [open (nth inlines opener-idx)
                 content (subvec inlines (inc opener-idx) closer-idx)
                 close (nth inlines closer-idx)
                 open-count (-> open :text count)
                 close-count (-> close :text count)
                 emph-count (if (and (>= open-count 2)
                                     (>= close-count 2))
                              2 1)
                 [new-open open] (split-delimiter-at open (- open-count emph-count))
                 [close new-close] (split-delimiter-at close emph-count)
                 emph (t/with-token (apply (if (= 2 emph-count)
                                             doc/strong-emphasis
                                             doc/emphasis)
                                           (ensure-no-delimiter-left-behind content))
                        (t/merge-tokens [open content close]))
                 pre (subvec inlines
                             0
                             (min opener-idx (count inlines)))
                 post (subvec inlines
                              (min (inc closer-idx) (count inlines)))
                 new-inlines (vec (concat pre
                                          [new-open emph new-close]
                                          post))]
             (recur (+ 2 ^long opener-idx)
                    openers-bottom
                    new-inlines))

           ; We found a closer but no matching opener
           (and (< opener-idx 0)
                (>= closer-idx 0))
           (recur (inc ^long closer-idx)
                  openers-bottom
                  (if (:open? (nth inlines closer-idx))
                    inlines
                    (update inlines closer-idx delimiter->text)))

           ; We found neither
           (and (< opener-idx 0)
                (< closer-idx 0))
           (recur (count inlines)
                  (count inlines)
                  inlines)))))))

(defn- look-for-link-or-image! [stream inlines close-delimiter]
  (let [begin-pos (in/position stream)
        idx (find-last-index inlines (comp #{"[" "!["} :text))]
    (if (>= idx 0)
      (let [open-delimiter (nth inlines idx)]
        (if (= ::delimiter (:type open-delimiter))
          (if-let [link-destination (parse-link-destination! stream)]
            (let [before (subvec inlines 0 idx)
                  after (subvec inlines (inc idx))
                  disable-open-links (fn [inlines]
                                       (if (= "[" (:text open-delimiter))
                                         (mapv #(if (= "[" (:text %))
                                                  (delimiter->text %)
                                                  %)
                                               inlines)
                                         inlines))]
              (-> before
                  (disable-open-links)
                  (conj (t/with-token (if (= "[" (:text open-delimiter))
                                        (doc/link (process-emphasis after)
                                                  link-destination)
                                        (apply doc/image link-destination
                                               (process-emphasis after)))
                          (t/stream->token stream
                                           (-> open-delimiter meta :token t/start)
                                           [open-delimiter after close-delimiter link-destination])))))
            (do (in/reset-position! stream begin-pos)
                (-> inlines
                    (update idx delimiter->text)
                    (conj (delimiter->text close-delimiter)))))
          (-> inlines
              (update idx delimiter->text)
              (conj (delimiter->text close-delimiter)))))
      (conj inlines (delimiter->text close-delimiter)))))


(defn parse-inlines! [stream ctx]
  (loop [inlines []
         inline-text nil]
    (cond
      (in/end? stream)
      (process-emphasis
       (condj inlines (close-text inline-text)))

      (parse-escaped-characters! stream)
      (recur (condj inlines (close-text inline-text))
             (append-next! nil stream))

      :else (let [begin-pos (in/position stream)]
              (if-let [[spaces backslash] (parse-line-breaks! stream)]
                (let [token (t/stream->token stream begin-pos [spaces backslash])
                      inlines (if backslash
                                (-> inlines
                                    (condj (-> inline-text
                                               (append-text stream spaces
                                                            (+ begin-pos (count spaces)))
                                               (close-text)))
                                    (condj (t/with-token (doc/hard-break) token)))
                                (-> inlines
                                    (condj (close-text inline-text))
                                    (condj (t/with-token (if (>= (count spaces) 2)
                                                           (doc/hard-break)
                                                           (doc/soft-break))
                                             token))))]
                  (if (contains? #{::paragraph ::indented-code-block} ; TODO(Richo): Check if this works for all inlines
                                 (:type (peek-line stream ctx)))
                    (do (consume-chars! stream \space \tab)
                        (recur inlines nil))
                    (process-emphasis inlines)))
                (if-let [special-inline (or (parse-clojure! stream)
                                            (parse-code-span! stream ctx))]
                  (recur (-> inlines
                             (condj (close-text inline-text))
                             (condj special-inline))
                         nil)
                  (if-let [{:keys [text] :as delimiter} (parse-delimiter! stream)]
                    (case (first text)
                      \] (recur (look-for-link-or-image! stream
                                                         (condj inlines (close-text inline-text))
                                                         delimiter)
                                nil)

                      (recur (-> inlines
                                 (condj (close-text inline-text))
                                 (condj delimiter))
                             nil))
                    (recur inlines
                           (append-next! inline-text stream)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block parsers

(defn parse-paragraph! [stream ctx]
  (let [begin-pos (in/position stream)
        make-token (fn [lines] (t/stream->token stream begin-pos lines))
        make-paragraph (fn [lines]
                         (t/with-token (apply doc/paragraph (apply concat lines))
                           (make-token lines)))
        make-heading (fn [level lines]
                       (t/with-token (apply doc/heading level
                                            (->> lines
                                                 (interpose [(doc/soft-break)])
                                                 (apply concat)))
                         (make-token lines)))]
    (loop [lines (transient [])]
      (let [{:keys [type] :as next-line} (peek-line stream ctx)]
        (case type
          ::paragraph
          (let [inlines (parse-inlines! stream ctx)]
            (if (seq inlines)
              (recur (conj! lines inlines))
              (make-paragraph (persistent! lines))))

          ; If we find setext-heading-underline, we convert the whole 
          ; paragraph to a heading
          ::setext-heading-underline
          (let [{:keys [chars]} (next-line! stream ctx)]
            (make-heading (if (= \- (first chars)) 2 1)
                          (persistent! lines)))

          ; Thematic breaks can be confused with setext-headings, in
          ; which case the setext-heading takes precedence
          ::thematic-break
          (if (= \- (first (:chars next-line)))
            (do (next-line! stream ctx) ; discard next line
                (make-heading 2 (persistent! lines)))
            (make-paragraph (persistent! lines)))

          ; Indented code blocks can't interrupt a paragraph, so if
          ; we found one we just treat it as a valid line
          ::indented-code-block
          (let [inlines (parse-inlines! stream ctx)]
            (if (seq inlines)
              (recur (conj! lines inlines))
              (make-paragraph (persistent! lines))))

          ; Anything else, simply breaks the paragraph, we do nothing
          (make-paragraph (persistent! lines)))))))


(defn parse-thematic-break! [stream ctx]
  (let [begin-pos (in/position stream)
        line (next-line! stream ctx)]
    (t/with-token (doc/thematic-break)
      (t/stream->token stream begin-pos line))))

(defn parse-atx-heading! [stream ctx]
  (let [begin-pos (in/position stream)
        {:keys [level content-start] :as line} (next-line! stream ctx)]
    ; We reset the stream to the beginning of the content (skipping all the #)
    (in/reset-position! stream content-start)
    ; Skip any spaces or tabs before parsing the inline content
    (consume-chars! stream \space \tab)
    (let [inlines (parse-inlines! stream ctx)]
      (t/with-token (apply doc/heading level inlines)
        (t/stream->token stream begin-pos line)))))

(defn parse-blank-lines! [stream ctx]
  (loop []
    (when (= ::blank (:type (peek-line stream ctx)))
      (next-line! stream ctx)
      (recur))))

(defn parse-indented-code-block! [stream ctx]
  ; NOTE(Richo): We take the actual lines from the tokens because
  ; the code blocks should preserve whatever the user typed. 
  ; However, since we're parsing an indented block code, we need 
  ; to remove the first 4 indentation spaces.
  (let [begin-pos (in/position stream)
        lines (loop [lines (transient [])]
                (let [{:keys [type]} (peek-line stream ctx)]
                  (if (= ::indented-code-block type)
                    (recur (conj! lines (next-line! stream ctx)))
                    (persistent! lines))))
        line-contents (map #(-> % meta :token t/input-value (subs 4))
                           lines)]
    (t/with-token (doc/code-block "" (str/join line-contents))
      (t/stream->token stream begin-pos lines))))

(defn parse-fenced-code-block! [stream ctx]
  (let [begin-pos (in/position stream)
        opening (next-line! stream ctx)
        lines (loop [lines (transient [])]
                (if-let [{:keys [type] :as next} (peek-line stream ctx)]
                  (if-not (and (= ::code-fence type)
                               (str/blank? (:info-string next))
                               (= (-> opening :chars first)
                                  (-> next :chars first))
                               (<= (-> opening :chars count)
                                   (-> next :chars count)))
                    (recur (conj! lines (next-line! stream ctx)))
                    (persistent! lines))
                  (persistent! lines)))

        ; We may or may not have a closing fence
        closing (when (= ::code-fence (:type (peek-line stream ctx)))
                  (next-line! stream ctx))

        ; NOTE(Richo): We take the actual lines from the tokens because
        ; the code blocks should preserve whatever the user typed
        line-contents (map #(-> % meta :token t/input-value)
                           lines)]
    (t/with-token (doc/code-block (str/trim (:info-string opening))
                                  (str/join line-contents))
      (t/stream->token stream begin-pos
                       [opening lines closing]))))

(declare parse-block!)

(defn compatible-list-markers? [m1 m2]
  (and (= (:type m1) (:type m2))
       (= (:char m1) (:char m2))))

(defn parse-list-item-blocks! [stream ctx]
  (let [space-parser (-> ctx :line-prefix)
        first-block (parse-block! stream ctx)
        next-blocks (loop [blocks (transient [])]
                      (if (and (r/success? (pp/parse-on space-parser stream))
                               (not (in/end? stream)))
                        (recur (conj! blocks (parse-block! stream ctx)))
                        (let [{:keys [type]} (peek-line stream ctx)]
                          (if (= type ::blank)
                            (recur (conj! blocks (parse-block! stream ctx)))
                            (persistent! blocks)))))]
    (cons first-block (remove nil? next-blocks))))

(defn parse-list-item! [stream first-item ctx]
  (let [begin-pos (in/position stream)
        next-line (next-line! stream ctx)]
    (if (and (= ::list-item (:type next-line))
             (compatible-list-markers? (:marker next-line)
                                       (:marker first-item)))
      (let [blocks (parse-list-item-blocks! stream ctx)]
        (t/with-token (apply doc/list-item blocks)
          (t/stream->token stream begin-pos nil)))
      (do (in/reset-position! stream begin-pos)
          nil))))

(defn parse-list! 
  [stream {:keys [marker ^long spaces] :as first-item} ctx]
  (let [begin-pos (in/position stream)
        new-ctx (update ctx :line-prefix
                        (fn [parser]
                          (let [space-parser (pp/times space (+ spaces (count (:digits marker)) 1))]
                            (if parser
                              (pp/seq parser space-parser)
                              space-parser))))
        items (loop [items (transient [])]
                (if-let [item (parse-list-item! stream first-item new-ctx)]
                  (recur (conj! items item))
                  (persistent! items)))
        list-fn (case (:type marker)
                  ::ordered-list-marker (partial doc/ordered-list 
                                                 (parse-long (:digits marker)))
                  ::bullet-list-marker doc/bullet-list)]
    (t/with-token (apply list-fn items)
      (t/stream->token stream begin-pos nil))))

(defn parse-block! [stream ctx]
  (let [{:keys [type] :as line} (peek-line stream ctx)]
    (case type
      ::list-item (parse-list! stream line ctx)
      ::paragraph (parse-paragraph! stream ctx)
      ::thematic-break (parse-thematic-break! stream ctx)
      ::atx-heading (parse-atx-heading! stream ctx)
      ::setext-heading-underline (parse-paragraph! stream ctx)
      ::indented-code-block (parse-indented-code-block! stream ctx)
      ::code-fence (parse-fenced-code-block! stream ctx)
      ::blank (parse-blank-lines! stream ctx)
      (throw (ex-info (str "Parse error! Type not found: " type) {})))))

(defn parse-blocks! [stream ctx]
  (loop [blocks (transient [])]
    (if-not (in/end? stream)
      (if-let [next-block (parse-block! stream ctx)]
        (recur (conj! blocks next-block))
        (recur blocks))
      (persistent! blocks))))

(defn make-context []
  {:line-prefix nil})

(defn parse
  ([src] (parse src {}))
  ([src options] (parse src options (make-context)))
  ([src options ctx]
   (binding [*debug-verbose-emphasis* (:debug options)
             *debug-verbose-tokens* (:debug options)
             *verbose-eval* (:verbose options)]
     (let [stream (in/make-stream src)
           blocks (parse-blocks! stream ctx)]
       (t/with-token (apply doc/document blocks)
         (t/make-token src 0 (count src) nil))))))

(defn parse-file [file options]
  (binding [*parser-file* (str file)]
    (parse (slurp file) options)))