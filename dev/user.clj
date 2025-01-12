(ns user
  (:require [portal.api :as p]))

(def !portal (atom nil))

(defn open-portal! []
  (reset! !portal (p/open {:launcher :vs-code}))
  (add-tap #'p/submit))

(defn close-portal! []
  (let [[portal _] (reset-vals! !portal nil)]
    (when portal (p/close portal))))

(comment
  
  (open-portal!)
  (close-portal!)
  )