(ns ssgr.watcher
  (:import (java.nio.file WatchService Path StandardWatchEventKinds
                          WatchEvent Files FileSystems LinkOption
                          SimpleFileVisitor FileVisitResult))
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(def WATCH-EVENT-KINDS
  (into-array [StandardWatchEventKinds/ENTRY_CREATE
               StandardWatchEventKinds/ENTRY_DELETE
               StandardWatchEventKinds/ENTRY_MODIFY]))

(def LINK-OPTIONS
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))


(defn make-watcher []
  {:watcher (.newWatchService (FileSystems/getDefault))
   :watch-keys (atom {})})

(defn register! [{:keys [watch-keys watcher]} ^Path path]
  (Files/walkFileTree
   path
   (proxy [SimpleFileVisitor] []
     (preVisitDirectory [^Path dir _]
       (if (Files/isHidden dir)
         FileVisitResult/SKIP_SUBTREE
         (let [watch-key (.register dir watcher WATCH-EVENT-KINDS)]
           (swap! watch-keys assoc watch-key dir)
           FileVisitResult/CONTINUE))))))

(defn start!
  [{:keys [^WatchService watcher watch-keys] :as w} callback]
  (doto (Thread.
         #(try
            (loop []
              (let [watch-key (.take watcher)]
                (when-let [^Path dir (get @watch-keys watch-key)]
                  (doseq [^WatchEvent evt (.pollEvents watch-key)]
                    (when-let [^Path evt-context (.context evt)]
                      (let [child (.resolve dir evt-context)
                            kind (.kind evt)]
                        (when-not (Files/isHidden child)
                          (callback kind child)
                          (try
                            (when (and (= StandardWatchEventKinds/ENTRY_CREATE kind)
                                       (Files/isDirectory child LINK-OPTIONS))
                              (register! w child))
                            (catch Exception ex
                              (println "ERROR:" ex)))))))
                  (when-not (.reset watch-key)
                    (swap! watch-keys dissoc watch-key)))
                (when (seq @watch-keys)
                  (recur))))
            (catch Throwable t
              (println "Unexpected error!" t)))
         "watcher/polling-process")
    (.start)))

(defn stop! [{:keys [^WatchService watcher]}]
  (.close watcher))

(defn start-watcher! [path callback]
  (let [watcher (doto (make-watcher)
                  (register! path))
        ^Thread polling-process
        (start! watcher callback)]
    [watcher polling-process]))
