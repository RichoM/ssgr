(ns ssgr.input-stream
  (:refer-clojure :exclude [peek]))

(defprotocol Stream
  (source [stream])
  (position [stream])
  (end? [stream])
  (peek [stream])
  (next! [stream])
  (reset-position! [stream new-pos]))

(deftype StringStream [src ^:unsynchronized-mutable pos]
  Stream
  (source ^String [stream] (.src stream))
  (position ^long [stream] (.pos stream))
  (end? [stream] 
    (>= ^long (.pos stream) 
        (.length ^String (.src stream))))
  (peek [stream]
    (let [^String src (.src stream)
          ^long pos (.pos stream)]
      (when (< pos (.length src))
        (char (.charAt src pos)))))
  (next! [stream]
    (when-let [val (peek stream)]
      (set! pos (inc ^long (.pos stream)))
      val))
  (reset-position! [_ new-pos]
    (set! pos ^long new-pos)
    nil))

(defn make-stream [src]
  (StringStream. src 0))

#_(defn end? [stream]
  (nil? (peek stream)))

#_(defn take! [stream ^long length]
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
       (+ ^long (position stream) offset)
       nil))

(defn substream
  ([stream start]
   (substream stream start (position stream)))
  ([stream start end]
   (subs (source stream) start end)))