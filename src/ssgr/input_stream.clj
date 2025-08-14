(ns ssgr.input-stream
  (:refer-clojure :exclude [peek]))

(defn make-stream [src]
  {:src src :pos (volatile! 0)})

(defn position ^long [{pos :pos}] @pos)
(defn source [stream] (:src stream))

(defn reset-position! [stream pos]
  (vreset! (:pos stream) pos)
  nil)

(defn peek [{:keys [src pos]}]
  (nth src @pos nil))

(defn next! [stream]
  (when-let [val (peek stream)]
    (vreset! (:pos stream) (inc (position stream)))
    val))

(defn end? [stream]
  (nil? (peek stream)))

(defn take! [stream ^long length]
  (let [start (position stream)
        end (min (count (:src stream))
                 (+ length start))]
    (reset-position! stream end)
    (subs (:src stream) start end)))


(defn take-while! [stream predicate]
  (loop [result (transient [])]
    (let [chr (peek stream)]
      (if (and chr (predicate chr))
        (recur (conj! result (next! stream)))
        (persistent! result)))))

(defn take-until! [stream predicate]
  (take-while! stream (complement predicate)))

(defn take-chars! [stream & chars]
  (take-while! stream (set chars)))

(defn take-1-char! [stream char]
  (let [next (peek stream)]
    (when (= char next)
      (next! stream))))

(defn take-max! [stream predicate ^long limit]
  (loop [result (transient [])
         count 0]
    (let [chr (peek stream)]
      (if (and (< count limit)
               (predicate chr))
        (recur (conj! result (next! stream))
               (inc count))
        (persistent! result)))))

(defn count-max! ^long [stream predicate ^long limit]
  (loop [count 0]
    (let [chr (peek stream)]
      (if (and chr
               (< count limit)
               (predicate chr))
        (do (next! stream)
            (recur (inc count)))
        count))))

(defn count-while! ^long [stream predicate]
  (loop [count 0]
    (let [chr (peek stream)]
      (if (and chr (predicate chr))
        (do (next! stream)
            (recur (inc count)))
        count))))

(defn count-until! ^long [stream predicate]
  (count-while! stream (complement predicate)))

(defn peek-at [stream pos]
  (nth (source stream) pos nil))

(defn peek-offset [stream ^long offset]
  (nth (source stream)
       (+ (position stream) offset)
       nil))

(defn substream
  ([stream start]
   (substream stream start (position stream)))
  ([stream start end]
   (subs (:src stream) start end)))