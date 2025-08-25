(ns ssgr.utils
  (:require [babashka.fs :as fs]))

(defn list-files [src]
  (when-let [entries (sort (.listFiles (fs/file src)))]
    (concat (filter fs/regular-file? entries)
            (->> entries
                 (filter fs/directory?)
                 (mapcat list-files)))))


(defn write-file! [path contents]
  (when-let [parent (fs/parent path)]
    (fs/create-dirs parent))
  (spit (fs/file path) contents))

(defn delete-path! [path]
  (try
    (fs/delete path)
    (catch Exception ex
      (println "Couldn't delete file" (ex-message ex)))))

(defn delete-all! [target-dir]
  (fs/walk-file-tree target-dir
                     {:visit-file (fn [path _]
                                    (delete-path! path)
                                    :continue)
                      :post-visit-dir (fn [path _]
                                        (delete-path! path)
                                        :continue)}))

(defn copy-file! [src dest]
  (when-let [parent (fs/parent dest)]
    (fs/create-dirs parent))
  (fs/copy src dest))