(ns ssgr.parser
  (:require [clojure.string :as str]
            [petitparser.input-stream :as in]
            [petitparser.core :as pp]
            [petitparser.results :as r]
            [ssgr.clojure :as c :refer [*verbose-eval*]]
            [ssgr.token :as t :refer [*debug-verbose-tokens* *parser-file*]]
            [ssgr.doc :as doc]
            [ssgr.utils.pprint :refer [pprintln pprint-str]]))

; (require 'hashp.preload)

(def ^:dynamic *debug-verbose-emphasis* false)

(defn digit? [chr]
  (and chr (Character/isDigit ^char chr)))

(def space? #{\space \tab})

(defn parse! [parser stream]
  (let [result (pp/parse-on parser stream)]
    (when (r/success? result)
      (r/actual-result result))))

(defn discard! [parser stream]
  (when parser
    (pp/parse-on parser stream)
    nil))

(defn peek-parser [parser stream]
  (let [begin-pos (in/position stream)
        result (pp/parse-on parser stream)]
    (in/reset-position! stream begin-pos)
    (when (r/success? result)
      (r/actual-result result))))

(defn take-while! [stream predicate]
  (loop [result (transient [])]
    (let [chr (in/peek stream)]
      (if (and chr (predicate chr))
        (recur (conj! result (in/next! stream)))
        (persistent! result)))))

(defn take-until! [stream predicate]
  (take-while! stream (complement predicate)))

(defn take-chars! [stream & chars]
  (take-while! stream (set chars)))

(defn take-1-char! [stream char]
  (let [next (in/peek stream)]
    (when (= char next)
      (in/next! stream))))

(defn take-max! [stream predicate ^long limit]
  (loop [result (transient [])
         count 0]
    (let [chr (in/peek stream)]
      (if (and (< count limit)
               (predicate chr))
        (recur (conj! result (in/next! stream))
               (inc count))
        (persistent! result)))))

(defn count-max! ^long [stream predicate ^long limit]
  (loop [count 0]
    (let [chr (in/peek stream)]
      (if (and (< count limit)
               (predicate chr))
        (do (in/next! stream)
            (recur (inc count)))
        count))))

(defn count-while! ^long [stream predicate]
  (loop [count 0]
    (let [chr (in/peek stream)]
      (if (predicate chr)
        (do (in/next! stream)
            (recur (inc count)))
        count))))

(defn count-spaces! ^long [stream ^long limit]
  (count-max! stream space? limit))

(defn count-digits! ^long [stream ^long limit]
  (count-max! stream digit? limit))

(defn peek-at [stream pos]
  (nth (in/source stream) pos nil))

(defn peek-offset [stream ^long offset]
  (nth (in/source stream)
       (+ (in/position stream) offset)
       nil))

(defn substream 
  ([stream start]
   (substream stream start (in/position stream)))
  ([stream start end]
   (subs (:src stream) start end)))

(defmacro try-parse [stream body]
  `(let [stream# ~stream
         begin-pos# (in/position stream#)]
     (if-some [result# ~body]
       result#
       (in/reset-position! stream# begin-pos#))))

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

(defn parse-newline! [stream]
  (case (in/peek stream)
    \return (do (in/next! stream)
                (if (= \newline (in/peek stream))
                  (do (in/next! stream)
                      "\r\n")
                  \return))
    \newline (do (in/next! stream)
                 \newline)
    nil))

(defn parse-newline-or-end! [stream]
  (try-parse
   stream
   (if (in/end? stream)
     ::end
     (parse-newline! stream))))

(def space (pp/or \space \tab))

(defn transform-with-token [p f]
  (pp/transform (pp/token p)
                (fn [token]
                  (t/with-token
                    (f (t/parsed-value token))
                    token))))

(defn parse-bullet-list-marker! [stream]
  (let [next (in/peek stream)]
    (when (#{\- \+ \*} next)
      {:type ::bullet-list-marker
       :digits ""
       :char (in/next! stream)})))

(defn parse-ordered-list-marker! [stream]
  (let [begin-pos (in/position stream)
        digit-count (count-digits! stream 9)]
    (if (>= digit-count 1)
      (let [digits (substream stream begin-pos)
            ^char next (in/peek stream)]
        (when (#{\. \)} next)
          {:type ::ordered-list-marker
           :digits digits
           :char (in/next! stream)}))
      (in/reset-position! stream begin-pos))))

(defn parse-list-item-marker! [stream]
  (try-parse
   stream
   (let [_leading-spaces (count-spaces! stream 3)
         marker (or (parse-bullet-list-marker! stream)
                    (parse-ordered-list-marker! stream))]
     (when marker
       (let [trailing-spaces (count-spaces! stream 4)]
         (when (>= trailing-spaces 1)
           ; TODO(Richo): Should I add a token here??? Probably not
           {:type ::list-item
            :spaces trailing-spaces
            :marker marker}))))))

; TODO(Richo): Can't remove yet because it's used by the line-prefix parser :(
(def blockquote 
  (transform-with-token
   (pp/seq (pp/max space 3)
           \> (pp/optional \space))
   (fn [[_ char _]]
     {:type ::blockquote})))

(defn parse-blockquote-marker! [stream]
  (try-parse
   stream
   (do (count-spaces! stream 3)
       (when (= \> (in/next! stream))
         (when (= \space (in/peek stream))
           (in/next! stream))
         {:type ::blockquote}))))

(defn *parse-thematic-break! [stream]
  (try-parse
   stream
   (do (count-spaces! stream 3)
       (let [next-char (in/peek stream)]
         (when (#{\- \_ \*} next-char)
           (let [chars (take-chars! stream next-char)]
             (when (>= (count chars) 3)
               (take-while! stream space?)
               (when (parse-newline-or-end! stream)
                 {:type ::thematic-break
                  :chars chars}))))))))

(defn *parse-atx-heading! [stream]
  (try-parse
   stream
   (do (count-spaces! stream 3) ; Discard leading spaces
       (let [level (count (take-max! stream #{\#} 6))
             content-start (in/position stream)]
         (when (>= level 1)
           (let [content (take-until! stream #{\return \newline})]
             (parse-newline! stream) ; Discard newline
             (when (or (empty? content)
                       (= \space (first content)))
               {:type ::atx-heading
                :level level
                :content-start content-start})))))))

(defn parse-setext-heading-underline! [stream]
  (try-parse
   stream
   (do (count-spaces! stream 3) ; Discard leading spaces
       (let [next-char (in/peek stream)]
         (when (#{\- \=} next-char)
           (let [chars (take-chars! stream next-char)]
             (count-while! stream space?) ; Discard trailing spaces
             (when (parse-newline-or-end! stream)
               {:type ::setext-heading-underline
                :chars chars})))))))

(defn *parse-indented-code-block! [stream]
  (try-parse
   stream
   (let [spaces (count-while! stream space?)]
     (when (>= spaces 4)
       (let [content (take-until! stream #{\return \newline})]
         (when (seq content)
           (parse-newline! stream)
           {:type ::indented-code-block}))))))

(def code-fence
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/or (pp/min \` 3)
                  (pp/min \~ 3))
           (pp/flatten (pp/star (pp/negate newline-or-end)))
           newline-or-end)
   (fn [[_ chars info-string]]
     {:type ::code-fence
      ;:chars chars
      :info-string info-string})))

(defn parse-code-fence! [stream]
  (let [result (pp/parse-on code-fence stream)]
    (when-not (r/failure? result)
      (r/actual-result result))))

(def blank (transform-with-token (pp/seq (pp/star space)
                                         newline-parser)
                                 (constantly {:type ::blank})))

(defn parse-blank! [stream]
  (let [result (pp/parse-on blank stream)]
    (when-not (r/failure? result)
      (r/actual-result result))))

(def paragraph
  (transform-with-token
   (pp/seq (pp/max space 3)
           (pp/flatten [(pp/plus (pp/negate pp/space))
                        (pp/star (pp/negate newline-or-end))])
           newline-or-end)
   (fn [[_ inline-text]]
     {:type ::paragraph
      ;:content inline-text
      })))

(defn *parse-paragraph! [stream]
  (let [result (pp/parse-on paragraph stream)]
    (when-not (r/failure? result)
      (r/actual-result result))))

(defn code-fence? [line]
  (pp/matches? code-fence line))

(defn blank? [line]
  (pp/matches? blank line))

(defn paragraph? [line]
  (pp/matches? paragraph line))

(defn next-line! [stream {:keys [line-prefix line-parser-cache]}]
  ; If we have a line-prefix parser, we use it to consume the stream, the result
  ; doesn't matter, we just discard it (I don't know if this is correct, though)
  (discard! line-prefix stream)
  ; Before actually trying to parse we look in the cache, if we find a hit we reset
  ; the stream to the cached final position and return the cached result
  (let [begin-pos (in/position stream)
        [cached-result cached-pos] (get @line-parser-cache begin-pos 
                                        [::not-found nil])]
    (if (not= cached-result ::not-found)
      (do (in/reset-position! stream cached-pos)
          cached-result)
      (let [; We try each line parser in order until we find one that matches
            final-result (or (parse-blockquote-marker! stream)
                             (parse-list-item-marker! stream)
                             (*parse-thematic-break! stream)
                             (*parse-atx-heading! stream)
                             (parse-setext-heading-underline! stream)
                             (*parse-indented-code-block! stream)
                             (parse-code-fence! stream)
                             (parse-blank! stream)
                             (*parse-paragraph! stream))
            final-pos (in/position stream)]
        (vswap! line-parser-cache assoc begin-pos [final-result final-pos])
        final-result))))

(defn peek-line [stream ctx]
  (let [begin-pos (in/position stream)
        result (next-line! stream ctx)]
    (in/reset-position! stream begin-pos)
    result))

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
          opening (take-chars! stream \`)
          begin-content (in/position stream)
          [content closing]
          (loop [content (transient [])]
            (let [next-char (in/peek stream)]
              (case next-char
                ; The closing and opening must be of equal length
                \` (let [closing (take-chars! stream \`)]
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
                               (take-chars! stream \space \tab)
                               (recur (conj! content \space))))
                \return (do (in/next! stream) ; Discard newline
                            (take-1-char! stream \newline) ; Discard newline (if any)
                            (when (contains? #{::paragraph ::indented-code-block}
                                             (:type (peek-line stream ctx)))
                              ; Discard leading spaces
                              (take-chars! stream \space \tab)
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
        chars (take-chars! stream char)
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

(defn peek-until-newline [stream]
  (let [begin-pos (in/position stream)
        result (take-while! stream (fn [chr] (not= chr \newline)))]
    (in/reset-position! stream begin-pos)
    (str/join "" result)))

(defn parse-inlines! [stream ctx & {:keys [multiline?] :or {multiline? true}}]
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
                  (if multiline?
                    (let [next-line-type (:type (peek-line stream ctx))]
                      (if (or (= ::paragraph next-line-type)
                              (and (= ::indented-code-block next-line-type)
                                   (some? (peek-parser (:line-prefix ctx) stream))))
                        (do (discard! (:line-prefix ctx) stream) ; Discard any line prefix
                            (take-chars! stream \space \tab)  ; Discard any leading spaces
                            (recur inlines nil))
                        (process-emphasis inlines)))
                    (process-emphasis inlines)))
                (if-let [special-inline (or (c/parse-clojure! stream ctx)
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
      (if (and (> (count lines) 0)
               (nil? (peek-parser (:line-prefix ctx) stream)))
        (make-paragraph (persistent! lines))
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
            (make-paragraph (persistent! lines))))))))

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
    (take-chars! stream \space \tab)
    (let [inlines (parse-inlines! stream ctx :multiline? false)]
      (t/with-token (apply doc/heading level inlines)
        (t/stream->token stream begin-pos line)))))

(defn parse-blank-lines! [stream ctx]
  (loop []
    (when (= ::blank (:type (peek-line stream ctx)))
      (next-line! stream ctx)
      (recur)))
  ::blank)

(defn parse-indented-code-block! [stream ctx]
  ; NOTE(Richo): We take the actual lines from the stream because
  ; the code blocks should preserve whatever the user typed. 
  ; However, since we're parsing an indented block code, we need 
  ; to remove the first 4 indentation spaces.
  (let [begin-pos (in/position stream)
        lines (loop [lines (transient [])]
                (let [line-begin (in/position stream)
                      {:keys [type]} (peek-line stream ctx)]
                  (if (= ::indented-code-block type)
                    (do (next-line! stream ctx)
                        (recur (conj! lines (subs (substream stream line-begin) 4))))
                    (persistent! lines))))]
    (t/with-token (doc/code-block "" (str/join lines))
      (t/stream->token stream begin-pos lines))))

(defn parse-fenced-code-block! [stream ctx]
  (let [begin-pos (in/position stream)
        opening (next-line! stream ctx)
        lines (loop [lines (transient [])]
                (let [line-begin (in/position stream)]
                  (if-let [{:keys [type] :as next} (peek-line stream ctx)]
                    (if-not (and (= ::code-fence type)
                                 (str/blank? (:info-string next))
                                 (= (-> opening :chars first)
                                    (-> next :chars first))
                                 (<= (-> opening :chars count)
                                     (-> next :chars count)))
                      (do (next-line! stream ctx)
                          (recur (conj! lines (substream stream line-begin))))
                      (persistent! lines))
                    (persistent! lines))))

        ; We may or may not have a closing fence
        closing (when (= ::code-fence (:type (peek-line stream ctx)))
                  (next-line! stream ctx))]
    (t/with-token (doc/code-block (str/trim (:info-string opening))
                                  (str/join lines))
      (t/stream->token stream begin-pos
                       [opening lines closing]))))

(declare parse-block!)

(defn compatible-list-markers? [m1 m2]
  (and (= (:type m1) (:type m2))
       (= (:char m1) (:char m2))))

(defn parse-list-item-blocks! [stream {:keys [line-prefix] :as ctx}]
  (let [[next-blocks last-valid-pos]
        (loop [blocks (transient [])
               last-valid-pos (in/position stream)]
          (let [begin-pos (in/position stream)
                first-block? (= (count blocks) 0)]
            (if (or first-block?
                    (and (r/success? (pp/parse-on line-prefix stream))
                         (not (in/end? stream))))
              (recur (conj! blocks (parse-block! stream ctx))
                     begin-pos)
              (let [{:keys [type]} (peek-line stream ctx)]
                (case type
                  ::blank (recur (conj! blocks (parse-block! stream ctx))
                                 begin-pos)
                  [(persistent! blocks) last-valid-pos])))))]
    ; HACK(Richo): If the last block we found is blank we reset the stream just
    ; before the blank lines. This way we only consume blank lines if they are
    ; inside the list item
    (when (= ::blank (peek next-blocks))
      (in/reset-position! stream last-valid-pos))
    (remove #{::blank} next-blocks)))

(defn parse-list! [stream ctx {:keys [^long spaces marker]}]
  (let [begin-list-pos (in/position stream)
        new-ctx (update ctx :line-prefix pp/seq
                        (pp/times space (+ spaces
                                           (count (:digits marker)) 1)))
        [items last-valid-pos]
        (loop [items (transient [])
               last-valid-pos begin-list-pos]
          (let [begin-item-pos (in/position stream)]
            (if (or (= 0 (count items))
                    (some? (parse! (:line-prefix ctx) stream)))
              (let [next-line (next-line! stream ctx)]
                (if (= ::blank (:type next-line))
                  (recur (conj! items (do (in/reset-position! stream begin-item-pos)
                                          (parse-block! stream ctx)))
                         begin-item-pos)
                  (if (and (= ::list-item (:type next-line))
                           (compatible-list-markers? marker (:marker next-line)))
                    (let [blocks (parse-list-item-blocks! stream new-ctx)
                          item (t/with-token (apply doc/list-item blocks)
                                 (t/stream->token stream begin-item-pos nil))]
                      (recur (conj! items item)
                             (in/position stream)))
                    (do (in/reset-position! stream begin-item-pos)
                        [(persistent! items) last-valid-pos]))))
              [(persistent! items) last-valid-pos])))
        list-fn (case (:type marker)
                  ::ordered-list-marker (partial doc/ordered-list
                                                 (parse-long (:digits marker)))
                  ::bullet-list-marker doc/bullet-list)]
    ; HACK(Richo): If the last item we found is blank we reset the stream just
    ; before the blank item. This way we only consume blank lines if they are
    ; between list items
    (when (= ::blank (peek items))
      (in/reset-position! stream last-valid-pos))
    (t/with-token (apply list-fn (remove #{::blank} items))
      (t/stream->token stream begin-list-pos nil))))

(defn parse-blockquote! [stream ctx]
  (let [begin-pos (in/position stream)
        _marker (next-line! stream ctx) ; Discard marker
        marker-parser (pp/seq (:line-prefix ctx) blockquote)
        new-ctx (update ctx :line-prefix pp/seq (pp/optional blockquote))
        blocks (loop [blocks (transient [])]
                 (if-not (in/end? stream)
                   ; If the next block begins with a blockquote marker, no matter what we parse
                   ; we continue parsing the blockquote. If the next block is not prefixed with
                   ; a blockquote marker then we stop at blank lines.
                   (if-let [marker (peek-parser marker-parser stream)]
                     (let [next-block (parse-block! stream new-ctx)]
                       (if (= ::blank next-block) ; Ignore blank lines and continue
                         (recur blocks)
                         (recur (conj! blocks next-block))))
                     (let [next-block (parse-block! stream new-ctx)]
                       (if (= ::blank next-block) ; Ignore blank lines and stop!
                         (persistent! blocks)
                         (recur (conj! blocks next-block)))))
                   (persistent! blocks)))]
    (t/with-token (apply doc/blockquote blocks)
      (t/stream->token stream begin-pos nil))))

(defn parse-block! [stream ctx]
  (when-let [{:keys [type] :as line} (peek-line stream ctx)]
    (case type
      ::list-item (parse-list! stream ctx line)
      ::blockquote (parse-blockquote! stream ctx)
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
      ; If the next-block is blank, keep going and just ignore the block
      (if-let [next-block (parse-block! stream ctx)]
        (if (= ::blank next-block)
          (recur blocks)
          (recur (conj! blocks next-block)))
        (persistent! blocks))
      (persistent! blocks))))

(defn make-context [eval-form]
  {:line-prefix (pp/seq)
   :eval-form eval-form
   :line-parser-cache (volatile! {})})

(defn parse
  ([src] (parse src {}))
  ([src options] (parse src options nil))
  ([src options eval-form]
   (binding [*debug-verbose-emphasis* (:debug options)
             *debug-verbose-tokens* (:debug options)
             *verbose-eval* (:verbose options)]
     (let [stream (in/make-stream src)
           ctx (make-context eval-form)
           blocks (parse-blocks! stream ctx)]
       (t/with-token (apply doc/document blocks)
         (t/make-token src 0 (count src) nil))))))

(defn parse-file [file options eval-form]
  (binding [*parser-file* (str file)]
    (parse (slurp file) options eval-form)))
