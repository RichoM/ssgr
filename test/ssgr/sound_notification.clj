(ns ssgr.sound-notification
  (:require [clojure.test :refer [report with-test-out]]
            [clj-audio.core :refer [->stream play]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go]]))

(defn- play-sound! [sound-name]
  (when-let [resource (io/resource sound-name)]
    (go (play (->stream resource)))))

(defn play! [success?]
  (play-sound! (if success? "success.wav" "error.wav")))

(defmethod report :summary [m]
  (play! (and (zero? (:fail m))
              (zero? (:error m))))
  (with-test-out
    (println "\nRan" (:test m) "tests containing"
             (+ (:pass m) (:fail m) (:error m)) "assertions.")
    (println (:fail m) "failures," (:error m) "errors.")))
