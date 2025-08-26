(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [ssgr.parser :as p]
            [ssgr.clojure :as c]
            [ssgr.renderer :as r]
            [ssgr.eval :as e]
            [ssgr.utils :as u])
  (:gen-class))

(defn process-regular-file! [src dest _]
  (u/copy-file! src dest))

(defn process-clj-file! [path options]
  (c/eval-file! (fs/file path)
                options e/eval-form))

(defn process-cljmd-file! [path out options]
  (let [doc (p/parse-file (fs/file path)
                          options e/eval-form)
        hiccup (r/render doc e/eval-render)
        html (r/html hiccup)]
    (u/write-file! (str (fs/strip-ext out) ".html")
                   html)))

(defn process-md-file! [path out options]
  (let [doc (p/parse-file (fs/file path)
                          options nil)
        hiccup (r/render doc e/eval-render)
        html (r/html hiccup)]
    (u/write-file! (str (fs/strip-ext out) ".html")
                   html)))

(defn process! [src out options]
  (println "Looking for files on" (str src))
  (u/delete-all! out)
  (e/reset-callbacks!)
  (let [files (u/list-files src)
        begin-time (System/nanoTime)]
    (println "Found" (count files) "files.")
    (doseq [path files]
      (try
        (let [out-path (str/replace-first path (str src) (str out))]
          (when (:verbose options)
            (println (str src)))
          (case (fs/extension path)
            "md" (process-md-file! path out-path options)
            "cljmd" (process-cljmd-file! path out-path options)
            "clj" (process-clj-file! path options)
            (process-regular-file! path out-path options)))
        (catch Exception ex
          (println (str "ERROR processing " path) ex))))
    (let [end-time (System/nanoTime)]
      (println "DONE!")
      (println "Elapsed time:"
               (/ (double (- end-time begin-time))
                  1000000.0)
               "ms"))))

(defmacro project-data [key]
  ; HACK(Richo): This macro allows to read the project.clj file at compile time
  `~(let [data (-> "project.clj" slurp read-string)
          name (str (nth data 1))
          version (nth data 2)
          rest (drop 3 data)]
      ((apply assoc {:name name, :version version} rest) key)))

(def project-name
  (let [description (project-data :description)
        version (project-data :version)]
    (format "%s (%s)" description version)))

(defn usage [options-summary]
  (->> [project-name
        ""
        "Usage: ssgr [options] in out"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Error:\n\n"
       (str/join \newline errors)))

(def cli-options
  [["-d" "--debug"
    :default false]
   ["-v" "--verbose"
    :default false]
   ["-h" "--help"]])

(defn validate-args [args]
  (let [{:keys [arguments errors options summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      (>= (count arguments) 1)
      (let [[src out] arguments]
        {:src (fs/path src)
         :out (fs/path (or out "out"))
         :options options})
      
      :else
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [src out options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (process! src out options))))
