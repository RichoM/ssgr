(ns ssgr.parser
  (:refer-clojure :exclude [symbol?])
  (:require [clojure.string :as str]
            [ssgr.input-stream :as in]
            [ssgr.lexer :as lexer]
            [ssgr.clojure :as c :refer [*verbose-eval*]]
            [ssgr.token :as t :refer [*debug-verbose-tokens* *parser-file*]]
            [ssgr.doc :as doc]))

; (require 'hashp.preload)

(def ^:dynamic *debug-verbose-emphasis* false)

(defn peek-parser [parser stream]
  (let [begin-pos (in/position stream)
        result (parser stream)]
    (in/reset-position! stream begin-pos)
    result))

(defmacro try-parse [stream body]
  `(let [stream# ~stream
         begin-pos# (in/position stream#)]
     (if-some [result# ~body]
       result#
       (in/reset-position! stream# begin-pos#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line parsers

(defn digit? [lex-token]
  (= ::lexer/digit (:type lex-token)))

(defn space? [lex-token]
  (= ::lexer/space (:type lex-token)))

(defn symbol? [lex-token]
  (= ::lexer/symbol (:type lex-token)))

(defn newline? [lex-token]
  (= ::lexer/newline (:type lex-token)))

(defn word? [lex-token]
  (= ::lexer/word (:type lex-token)))

(def code-fence-char? #{\` \~})

(def newline-char? #{\return \newline})

(defn parse-newline! [stream]
  (let [token (in/peek stream)]
    (when (newline? token)
      (in/skip! stream)
      token)))

(defn parse-newline-or-end! [stream]
  (try-parse
   stream
   (if (in/end? stream)
     ::end
     (parse-newline! stream))))

(defn parse-bullet-list-marker! [stream]
  (when-let [next (:char (in/peek stream))]
    (when (#{\- \+ \*} next)
      (in/skip! stream)
      {:type ::bullet-list-marker
       :digits ""
       :char next})))

(defn parse-ordered-list-marker! [stream]
  (let [begin-pos (in/position stream)
        digit-count (in/count-max! stream digit? 9)]
    (if (>= digit-count 1)
      (let [digits (->> (in/substream stream begin-pos)
                        (mapv :char)
                        (str/join))
            next-char (:char (in/peek stream))]
        (when (#{\. \)} next-char)
          (in/skip! stream)
          {:type ::ordered-list-marker
           :digits digits
           :char next-char}))
      (in/reset-position! stream begin-pos))))

(defn parse-list-item-marker! [stream]
  (try-parse
   stream
   (let [marker (or (parse-bullet-list-marker! stream)
                    (parse-ordered-list-marker! stream))]
     (when marker
       (let [spaces (in/count-max! stream space? 4)]
         (when (>= spaces 1)
           {:type ::list-item
            :spaces spaces
            :marker marker}))))))

(defn parse-blockquote-marker! [stream]
  (try-parse
   stream
   (when (= \> (:char (in/next! stream)))
     (when (= \space (:char (in/peek stream)))
       (in/skip! stream))
     {:type ::blockquote})))

(defn parse-thematic-break-line! [stream]
  (try-parse
   stream
   (let [next-char (:char (in/peek stream))]
     (when (#{\- \_ \*} next-char)
       (let [chars (in/take-while! stream (fn [t] (= next-char (:char t))))]
         (when (>= (count chars) 3)
           (in/skip-while! stream space?)
           (when (parse-newline-or-end! stream)
             {:type ::thematic-break
              :chars (mapv :char chars)})))))))

(defn parse-atx-heading-line! [stream]
  (try-parse
   stream
   (let [level (in/count-max! stream (comp #{\#} :char) 6)
         content-start (in/position stream)]
     (when (>= level 1)
       (let [content (in/take-until! stream newline?)]
         (parse-newline! stream) ; Discard newline
         (when (or (empty? content)
                   (= \space (:char (first content))))
           {:type ::atx-heading
            :level level
            :content-start content-start}))))))

(defn parse-setext-heading-underline! [stream]
  (try-parse
   stream
   (let [next-char (:char (in/peek stream))]
     (when (#{\- \=} next-char)
       (let [chars (in/take-while! stream (fn [t] (= next-char (:char t))))]
         (in/skip-while! stream space?) ; Discard trailing spaces
         (when (parse-newline-or-end! stream)
           {:type ::setext-heading-underline
            :chars (mapv :char chars)}))))))

(defn parse-indented-code-block-line! [stream ^long spaces-before]
  (try-parse
   stream
   (let [spaces (in/count-while! stream space?)]
     (when (>= spaces (- 4 spaces-before))
       (let [content (in/take-until! stream newline?)]
         (when (seq content)
           (parse-newline! stream)
           {:type ::indented-code-block}))))))

(defn parse-code-fence-line! [stream]
  (try-parse
   stream
   (let [chars (in/count-while! stream (comp code-fence-char? :char))]
     (when (>= chars 3)
       (let [info-string-begin (in/position stream)
             info-string-count (in/count-until! stream newline?)
             info-string (if (zero? info-string-count)
                           ""
                           (lexer/flatten (in/substream stream info-string-begin)))]
         (parse-newline! stream) ; Discard newline
         {:type ::code-fence
          :info-string info-string})))))

(defn parse-blank-line! [stream]
  (try-parse
   stream
   (do (in/skip-while! stream space?) ; Discard all spaces
       (when (parse-newline! stream)         
         {:type ::blank}))))

(defn parse-paragraph-line! [stream]
  (try-parse
   stream
   (when-not (in/end? stream)
     (when-not (space? (in/peek stream))
       (in/skip-until! stream newline?)
       (parse-newline! stream)
       {:type ::paragraph}))))

(defn parse-line-prefix! [stream {:keys [line-prefix]}]
  (let [begin-pos (in/position stream)
        result (line-prefix stream)]
    (when-not result
      (in/reset-position! stream begin-pos))
    result))

(defn peek-line-prefix [stream {:keys [line-prefix]}]
  (peek-parser line-prefix stream))

(defn next-line! [stream {:keys [line-parser-cache] :as ctx}]
  ; If we have a line-prefix parser, we use it to consume the stream, the result
  ; doesn't matter, we just discard it (I don't know if this is correct, though)
  (parse-line-prefix! stream ctx)
  ; Before actually trying to parse we look in the cache, if we find a hit we reset
  ; the stream to the cached final position and return the cached result
  (let [begin-pos (in/position stream)
        [cached-result cached-pos] (get @line-parser-cache begin-pos 
                                        [::not-found nil])]
    (if (not= cached-result ::not-found)
      (do (in/reset-position! stream cached-pos)
          cached-result)
      (let [; First, we discard up to 3 leading spaces (we need to count how many we
            ; discarded for the indented-code-block case) 
            spaces (in/count-max! stream space? 3)
            ; We try each line parser in order until we find one that matches
            final-result (or (parse-blockquote-marker! stream)
                             (parse-list-item-marker! stream)
                             (parse-thematic-break-line! stream)
                             (parse-atx-heading-line! stream)
                             (parse-setext-heading-underline! stream)
                             (parse-indented-code-block-line! stream spaces)
                             (parse-code-fence-line! stream)
                             (parse-blank-line! stream)
                             (parse-paragraph-line! stream))
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
    (when (= \\ (:char (in/peek stream)))
      (let [result (in/next! stream)]
        (if (punctuation-chars (:char (in/peek stream)))
          (:char result)
          (do (in/reset-position! stream begin-pos)
              nil))))))

(defn parse-code-span! [stream ctx]
  (when (= \` (:char (in/peek stream)))
    (let [begin-pos (in/position stream)
          opening (in/take-while! stream (fn [t] (= \` (:char t))))
          begin-content (in/position stream)
          [content closing]
          (loop [content (transient [])] ; TODO(Richo): Replace with StringBuilder?
            (let [next-token (in/peek stream)]
              (cond
                ; The closing and opening must be of equal length
                (and (symbol? next-token)
                     (= \` (:char next-token)))
                (let [closing (in/take-while! stream (fn [t] (= \` (:char t))))]
                  (if (= (count closing)
                         (count opening))
                    [(persistent! content) closing]
                    (recur (reduce conj! content
                                   (str/join (mapv :char closing))))))

                ; Line endings are converted to spaces, but only if the next line is either
                ; a paragraph or an indented-code-block.
                (newline? next-token)
                (do (in/skip! stream) ; Discard newline
                    (when (contains? #{::paragraph ::indented-code-block}
                                     (:type (peek-line stream ctx)))
                      ; Discard leading spaces
                      (in/skip-while! stream space?)
                      (recur (conj! content \space))))

                ; Anything else is appended to the content
                :else
                (when next-token
                  (recur (conj! content (lexer/input-value (in/next! stream))))))))
          token (t/stream->token stream begin-pos)]
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
            ; TODO(Richo): Text element without token??
            (doc/text (str/join (mapv :char opening))))))))

(declare parse-inlines!)

(defn condj [v val]
  (if val (conj v val) v))

(defn parse-link-destination! [stream]
  (when (= \( (:char (in/peek stream)))
    (in/skip! stream) ; Discard the first bracket
    (let [content
          (loop [bracket-count 0 ; We keep track of the open brackets to make sure
                                 ; they match the closing brackets
                 content (transient [])]
            (when-let [next-token (in/peek stream)]
              (let [next-char (:char next-token)]
                (case next-char
                  ; If we find an escape character, we check if the following char is
                  ; a bracket, in which case we discard the escape char and add the 
                  ; bracket to the content list, otherwise we add the escape char
                  \\ (let [escape-char next-char]
                       (in/skip! stream) ; Skip
                       (if (contains? #{\( \)} (:char (in/peek stream)))
                         (recur bracket-count
                                (conj! content (:char (in/next! stream))))
                         (recur bracket-count
                                (conj! content escape-char))))

                  ; We found an open bracket, we increment the bracket-count, we add
                  ; it to the content list, and we keep parsing
                  \( (do (in/skip! stream) ; Skip
                         (recur (inc bracket-count)
                                (conj! content next-char)))

                  ; We found a closing bracket, if the bracket-count is zero it means
                  ; the open/close brackets are balanced and we can stop parsing. If
                  ; the bracket-count is not zero we decrement the bracket-count, we
                  ; add the closing bracket to the content list, and keep going
                  \) (do (in/skip! stream) ; Skip
                         (if (zero? bracket-count)
                           (persistent! content)
                           (recur (dec bracket-count)
                                  (conj! content next-char))))

                  ; Any other character is simply added to the content list 
                  (do (in/skip! stream) ; Skip
                      (recur bracket-count
                             (conj! content (lexer/input-value next-token))))))))]
      (when content (str/join content)))))

(defn parse-line-breaks! [stream]
  (try-parse
   stream
   (do (in/skip-while! stream (fn [{:keys [char]}]
                                (or (space? char)
                                    (= \\ char))))
       (when-let [newline (parse-newline! stream)]
         (let [backslash (= \\ (lexer/prev-char newline))
               spaces (lexer/count-spaces-backwards!
                       newline
                       (if backslash 2 1))]
           [spaces backslash])))))

(defn append-next! [inline-text stream]
  ;; TODO(Richo): I'm not sure we need to keep track of the :text, maybe we can get it later from the src
  (if inline-text
    (-> inline-text
        (update :text #(str % (lexer/input-value (in/next! stream))))
        (assoc :stop (in/position stream)))
    {:stream stream
     :start (in/position stream)
     :stop (inc ^long (in/position stream))
     :text (lexer/input-value (in/next! stream))}))

(defn close-text [inline-text]
  (when-let [{:keys [stream ^long start ^long stop text]} inline-text]
    (when (> stop start)
      (t/with-token (doc/text text)
        (t/stream->token stream start stop)))))

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

(defn parse-emph-delimiter! [stream token char]
  (let [prev-char (or (lexer/prev-char token)
                      \space)
        chars (mapv :char (in/take-while! stream (fn [t] (= char (:char t)))))
        next-char (or (:char (in/peek stream)) 
                      \space)]
    {:type ::delimiter
     :text (str/join chars)
     :open? (can-open? prev-char next-char)
     :close? (can-close? prev-char next-char)
     :start (:start token)
     :prev-char prev-char
     :next-char next-char}))

(defn parse-delimiter!
  "When we’re parsing inlines and we hit either a run of * or _ characters, or a [ or ![
   we insert a text node with these symbols as its literal content, and we add a pointer
   to this text node to the delimiter stack."
  [stream]
  (let [begin-pos (in/position stream)]
    (when-let [delimiter
               (let [token (in/peek stream)]
                 (case (:char token)
                   \* (parse-emph-delimiter! stream token \*)
                   \_ (parse-emph-delimiter! stream token \_)
                   \! (do (in/skip! stream) ; Skip
                          (if (= \[ (:char (in/peek stream)))
                            (do (in/skip! stream) ; Skip
                                {:type ::delimiter
                                 :text "!["
                                 :open? true
                                 :close? false
                                 :start begin-pos})
                            (do (in/reset-position! stream begin-pos)
                                nil)))
                   \[ (do (in/skip! stream) ; Skip
                          {:type ::delimiter
                           :text "["
                           :open? true
                           :close? false
                           :start begin-pos})
                   \] (do (in/skip! stream) ; Skip
                          {:type ::delimiter
                           :text "]"
                           :open? false
                           :close? true
                           :start begin-pos})
                   nil))]
      delimiter)))

(defn split-delimiter-at
  [{:keys [text] :as delimiter} ^long n]
  (let [l-text (subs text 0 n)
        r-text (subs text n)]
    [(assoc delimiter :text l-text)
     (assoc delimiter :text r-text)]))

(defn- find-last-index ^long [items pred]
  (loop [idx (dec (count items))]
    (if (>= idx 0)
      (if (pred (nth items idx))
        idx
        (recur (dec idx)))
      -1)))

(defn delimiter->text [delimiter]
  (doc/text (:text delimiter)))

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
                        nil
                        #_(t/merge-tokens [open content close]))
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
                          nil
                          #_(t/stream->token stream
                                           (-> open-delimiter meta :token t/start))))))
            (do (in/reset-position! stream begin-pos)
                (-> inlines
                    (update idx delimiter->text)
                    (conj (delimiter->text close-delimiter)))))
          (-> inlines
              (update idx delimiter->text)
              (conj (delimiter->text close-delimiter)))))
      (conj inlines (delimiter->text close-delimiter)))))

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

      :else (let [^long begin-pos (in/position stream)]
              (if-let [[spaces backslash] (parse-line-breaks! stream)]
                (let [token (t/stream->token stream begin-pos)
                      inlines (if backslash
                                (-> inlines
                                    (condj (close-text inline-text))
                                    (condj (t/with-token (doc/hard-break) token)))
                                (-> inlines
                                    (condj (when-let [text (close-text inline-text)] ; TODO(Richo): This sucks!
                                             (when (> (count (:text text)) spaces)
                                               (update text :text #(subs % 0 (- (count %) spaces))))))
                                    (condj (t/with-token (if (>= spaces 2)
                                                           (doc/hard-break)
                                                           (doc/soft-break))
                                             token))))]
                  (if multiline?
                    (let [next-line-type (:type (peek-line stream ctx))]
                      (if (or (= ::paragraph next-line-type)
                              (and (= ::indented-code-block next-line-type)
                                   (peek-line-prefix stream ctx)))
                        (do (parse-line-prefix! stream ctx) ; Discard any line prefix
                            (in/skip-while! stream space?)  ; Discard any leading spaces
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
        make-paragraph (fn [lines]
                         (t/with-token (apply doc/paragraph (apply concat lines))
                           (t/stream->token stream begin-pos)))
        make-heading (fn [level lines]
                       (t/with-token (apply doc/heading level
                                            (->> lines
                                                 (interpose [(doc/soft-break)])
                                                 (apply concat)))
                         (t/stream->token stream begin-pos)))]
    (loop [lines (transient [])]
      (if (and (> (count lines) 0)
               (not (peek-line-prefix stream ctx)))
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
  (let [begin-pos (in/position stream)]
    (next-line! stream ctx) ; Skip next line
    (t/with-token (doc/thematic-break)
      (t/stream->token stream begin-pos))))

(defn parse-atx-heading! [stream ctx]
  (let [begin-pos (in/position stream)
        {:keys [level content-start]} (next-line! stream ctx)]
    ; We reset the stream to the beginning of the content (skipping all the #)
    (in/reset-position! stream content-start)
    ; Skip any spaces or tabs before parsing the inline content
    (in/skip-while! stream space?)
    (let [inlines (parse-inlines! stream ctx :multiline? false)]
      (t/with-token (apply doc/heading level inlines)
        (t/stream->token stream begin-pos)))))

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
  ; TODO(Richo): Instead of flattening each line and adding it to the lines
  ; vector, maybe we can just collect all the tokens and flatten them together
  ; at the end. That should be faster since it also avoids calling str/join
  (let [begin-pos (in/position stream)
        lines (loop [lines (transient [])]
                (let [line-begin (in/position stream)
                      {:keys [type]} (peek-line stream ctx)]
                  (if (= ::indented-code-block type)
                    (do (next-line! stream ctx) ; Discard next line
                        (let [actual-line (lexer/flatten (in/substream stream line-begin))]
                          (recur (conj! lines (subs actual-line 4)))))
                    (persistent! lines))))]
    (t/with-token (doc/code-block "" (str/join lines))
      (t/stream->token stream begin-pos))))

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
                          (let [actual-line (lexer/flatten (in/substream stream line-begin))]
                            (recur (conj! lines actual-line))))
                      (persistent! lines))
                    (persistent! lines))))

        ; We may or may not have a closing fence
        _closing (when (= ::code-fence (:type (peek-line stream ctx)))
                   (next-line! stream ctx))]
    (t/with-token (doc/code-block (str/trim (:info-string opening))
                                  (str/join lines))
      (t/stream->token stream begin-pos))))

(declare parse-block!)

(defn compatible-list-markers? [m1 m2]
  (and (= (:type m1) (:type m2))
       (= (:char m1) (:char m2))))

(defn parse-list-item-blocks! [stream ctx]
  (let [[next-blocks last-valid-pos]
        (loop [blocks (transient [])
               last-valid-pos (in/position stream)]
          (let [begin-pos (in/position stream)
                first-block? (= (count blocks) 0)]
            (if (or first-block?
                    (and (parse-line-prefix! stream ctx)
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
        new-ctx (update ctx :line-prefix
                        (fn [line-prefix]
                          (fn [stream]
                            (and (line-prefix stream)
                                 (let [expected-spaces (+ spaces (count (:digits marker)) 1)]
                                   (= (in/count-max! stream space? expected-spaces)
                                      expected-spaces))))))
        [items last-valid-pos]
        (loop [items (transient [])
               last-valid-pos begin-list-pos]
          (let [begin-item-pos (in/position stream)]
            (if (or (= 0 (count items))
                    (parse-line-prefix! stream ctx))
              (let [next-line (next-line! stream ctx)]
                (if (= ::blank (:type next-line))
                  (recur (conj! items (do (in/reset-position! stream begin-item-pos)
                                          (parse-block! stream ctx)))
                         begin-item-pos)
                  (if (and (= ::list-item (:type next-line))
                           (compatible-list-markers? marker (:marker next-line)))
                    (let [blocks (parse-list-item-blocks! stream new-ctx)
                          item (t/with-token (apply doc/list-item blocks)
                                 (t/stream->token stream begin-item-pos))]
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
      (t/stream->token stream begin-list-pos))))

(defn parse-blockquote! [stream ctx]
  (let [begin-pos (in/position stream)
        _marker (next-line! stream ctx) ; Discard marker
        marker-parser (let [line-prefix (:line-prefix ctx)]
                        (fn [stream]
                          (and (line-prefix stream)
                               (some? (parse-blockquote-marker! stream)))))
        new-ctx (update ctx :line-prefix
                        (fn [line-prefix]
                          (fn [stream]
                            (and (line-prefix stream)
                                 (do (parse-blockquote-marker! stream)
                                     true)))))
        blocks (loop [blocks (transient [])]
                 (if-not (in/end? stream)
                   ; If the next block begins with a blockquote marker, no matter what we parse
                   ; we continue parsing the blockquote. If the next block is not prefixed with
                   ; a blockquote marker then we stop at blank lines.
                   (if (peek-parser marker-parser stream)
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
      (t/stream->token stream begin-pos))))

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
  {:line-prefix (constantly true)
   :eval-form eval-form
   :line-parser-cache (volatile! {})})

(defn parse
  ([src] (parse src {}))
  ([src options] (parse src options nil))
  ([src options eval-form]
   (binding [*debug-verbose-emphasis* (:debug options)
             *debug-verbose-tokens* (:debug options)
             *verbose-eval* (:verbose options)]
     (let [stream (in/make-stream (lexer/tokenize src))
           ctx (make-context eval-form)
           blocks (parse-blocks! stream ctx)]
       (t/with-token (apply doc/document blocks)
         (t/stream->token stream 0))))))

(defn parse-file [file options eval-form]
  (binding [*parser-file* (str file)]
    (parse (slurp file) options eval-form)))

(comment
  (do (def src "foo\\\r\nbar")
      (def stream (in/make-stream (lexer/tokenize src)))
      (def ctx (make-context nil)))

  (def doc (parse "*foo*"))
  (meta doc)
  (meta (-> doc :blocks first :elements first))

  (parse-paragraph! stream ctx)
  (next-line! stream ctx)
  (peek-line stream ctx)

  )