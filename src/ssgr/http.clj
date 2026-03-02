(ns ssgr.http
  (:require [babashka.fs :as fs]
            [hiccup.core :as html]
            [clojure.string :as str]
            [ssgr.http-mime :refer [ext-mime-type]]
            [org.httpkit.server :as server])
  (:import [java.net URLDecoder URLEncoder]))

(def html-end-tags #"(?i)(<\s*\/\s*body\s*>\s*)?(<\s*\/\s*html\s*>\s*)?$")

(def reload-script
  "let socket = new WebSocket('/');
 socket.onopen = () => {
   socket.onmessage = () => { location.reload(); };
   socket.onclose = () => {
     var banner = document.createElement('div');
     banner.innerHTML = '<div style=\"width: 100%;background-color: #ff833d;position: fixed;bottom: 0;left: 0;text-align: center;font-family: monospace;border-top: 2px solid crimson;padding: 8px;\">Connection with server lost. This content might be stale. Reload the page!</div>';
     document.body.appendChild(banner);
   }
 }")

(defn inject-reload-script [html-string]
  (str/replace-first html-string html-end-tags
                     (str "<script>" reload-script "</script>" "$0")))

(defn- file-link
  "Get HTML link for a file/directory in the given dir."
  [dir f]
  (let [rel-path (fs/relativize dir f)
        ending (if (fs/directory? f) "/" "")
        names (seq rel-path)
        enc-names (map #(URLEncoder/encode (str %)) names)]
    [:a {:href (str "/" (str/join "/" enc-names) ending)}
     (str rel-path ending)]))

(defn- index [dir f]
  (let [files (->> (fs/list-dir f)
                   (remove fs/hidden?)
                   (map #(file-link dir %)))]
    {:body (-> [:html
                [:head
                 [:meta {:charset "UTF-8"}]
                 [:title (str "Index of `" f "`")]]
                [:body
                 [:h1 "Index of " [:code (str f)]]
                 [:ul
                  (for [child files]
                    [:li child])]
                 [:script reload-script]]]
               html/html
               str)}))

(defn- body
  ([path]
   (body path {}))
  ([path headers]
   {:headers (merge {"Content-Type" (ext-mime-type (fs/file-name path))} headers)
    :body (if (= "html" (fs/extension path))
            (inject-reload-script (slurp (fs/file path)))
            (fs/file path))}))

(defn- parse-range-header [range-header]
  (map #(when % (Long/parseLong %))
       (-> range-header
           (str/replace #"^bytes=" "")
           (str/split #"-"))))

(defn- read-bytes [^java.io.File f [start end]]
  (let [end (if end (inc end)
                (min (fs/size f)
                     (+ start (* 1024 1024))))
        len (- end start)
        arr (byte-array len)]
    (with-open [r (java.io.RandomAccessFile. f "r")]
      (.seek r start)
      (.read r arr 0 len))
    arr))

(defn- byte-range
  ([path request-headers]
   (byte-range path request-headers {}))
  ([path request-headers response-headers]
   (let [f (fs/file path)
         [start _end
          :as requested-range] (parse-range-header (request-headers "range"))
         arr (read-bytes f requested-range)
         num-bytes-read (count arr)]
     {:status 206
      :headers (merge {"Content-Type" (ext-mime-type (fs/file-name path))
                       "Accept-Ranges" "bytes"
                       "Content-Length" num-bytes-read
                       "Content-Range" (format "bytes %d-%d/%d"
                                               start
                                               (+ start num-bytes-read)
                                               (fs/size f))}
                      response-headers)
      :body arr})))

(defn- with-ext [path ext]
  (fs/path (fs/parent path) (str (fs/file-name path) ext)))

(defn- file-router [dir]
  (fn [{:keys [uri] :as req}]
    (let [f (fs/path dir (str/replace-first (URLDecoder/decode uri) #"^/" ""))
          index-file (fs/path f "index.html")]
      (cond
        (and (fs/directory? f)
             (not (str/ends-with? uri "/")))
        {:status 302
         :headers {"location" (str uri "/")}}

        (and (fs/directory? f) (fs/readable? index-file))
        (body index-file)

        (fs/directory? f)
        (index dir f)

        (and (fs/readable? f) (contains? (:headers req) "range"))
        (byte-range f (:headers req))

        (fs/readable? f)
        (body f)

        (and (nil? (fs/extension f)) (fs/readable? (with-ext f ".html")))
        (body (with-ext f ".html"))

        :else
        {:status 404 :body (str "Not found `" f "` in " dir)}))))

(defonce server (atom nil))

(defonce channels (atom #{}))

(defn reload-all! []
  (doseq [ch @channels]
    (server/send! ch "reload!")))

(defn websocket-handler [handler]
  (fn [req]
    (if-not (:websocket? req)
      (handler req)
      (server/as-channel
       req
       {:on-open (fn [ch] (swap! channels conj ch))
        :on-close (fn [ch _] (swap! channels disj ch))}))))

(defn stop-server! []
  (when-not (nil? @server)
    (@server)
    (reset! server nil)))

(def paused? (atom false))

(defn pause-while! [action]
  (reset! paused? true)
  (action)
  (reset! paused? false))

(defn- pause-handler [handler]
  (fn [req]
    (if @paused?
      (do (Thread/sleep 100)
          (recur req))
      (handler req))))

(defn start-server! [dir port]
  (stop-server!)
  (reset! server (server/run-server (-> (file-router dir)
                                        (websocket-handler)
                                        (pause-handler))
                                    {:port port})))

(comment

  (start-server! "out" 8080)
  @server

  (stop-server!)

  @channels
  (doseq [ch @channels]
    (server/send! ch "Broadcasting: Richo capo!"))
  
  (reload-all!)
  )