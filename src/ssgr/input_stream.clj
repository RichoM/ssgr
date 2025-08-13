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
