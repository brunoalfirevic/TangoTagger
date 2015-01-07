(ns tango-tagger.core
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as enl]
            [claudio.id3 :as id3]
            [clojure.pprint :as pp]
            [clj-http.client :as http]
            [clojure.java.io :as io])
  (:import org.jaudiotagger.audio.AudioFileIO
           org.jaudiotagger.tag.id3.framebody.FrameBodyTXXX
           org.jaudiotagger.tag.id3.ID3v23Tag
           org.jaudiotagger.tag.id3.ID3v24Tag
           org.jaudiotagger.tag.id3.ID3v23Frame
           org.jaudiotagger.tag.id3.ID3v24Frame

           java.text.Normalizer
           java.text.Normalizer$Form)
  (:gen-class))

(def domap (comp dorun map))

(defn get-all-sub-folders [folder]
    (map #(.toString %) (filter #(.isDirectory %) (file-seq (io/file folder)))))

(defn get-files [folder]
    (map #(.toString %) (filter #(.isFile %) (file-seq (io/file folder)))))

(defn get-files-and-folders [folder]
    (map #(.toString %) (file-seq (io/file folder))))

(defn file-name [file-path]
  (.getName (io/file file-path)))

(defn file-exists? [file-path]
  (.exists (io/file file-path)))

(defn combine-path [first-part second-part]
  (.toString (java.nio.file.Paths/get first-part (into-array String [second-part]))))

(defn normalize-unicode-to-ascii [str]
  (-> (java.text.Normalizer/normalize str java.text.Normalizer$Form/NFKD)
      (.replaceAll "—" "-")
      (.replaceAll "\\p{InCombiningDiacriticalMarks}+" "")
      (.replaceAll "\\p{Cntrl}+" "")))

(defn write-custom-tag! [file tag-name value]
  (let [audio-file (org.jaudiotagger.audio.AudioFileIO/read file)]
    (let [tag (.getTagOrCreateAndSetDefault audio-file)]
      (if value
        (let [txxx-body (FrameBodyTXXX.)]
          (.setDescription txxx-body tag-name)
          (.setText txxx-body value)
          (let [frame (if (instance? ID3v23Tag tag)
                        (ID3v23Frame. "TXXX")
                        (ID3v24Frame. "TXXX"))]
            (.setBody frame txxx-body)
            (.addField tag frame)))
        (let [txxx-fields (filter #(not= (.getDescription (.getBody %)) tag-name) (.getFields tag "TXXX"))]
          (.deleteField tag "TXXX")
          (domap #(.addField tag %) txxx-fields)))
      (.commit audio-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tango-info-file [folder]
    (combine-path folder "tango.info.txt"))

(defn tango-info-cache-file [folder]
    (combine-path folder "tango.info.html"))

(defn is-track? [track]
  (.endsWith track ".mp3"))

(defn sort-tracks [tracks]
  (sort tracks))

(defn is-album-folder? [folder]
  (file-exists? (tango-info-file folder)))

(defn make-album [folder]
    {:folder-name folder
     :tango-info-url (str/replace (str/trim (slurp (tango-info-file folder))) "https://tango.info/" "")})

(defn get-albums [root]
  (for [album-folder (get-all-sub-folders root)
        :when (is-album-folder? album-folder)]
    (make-album album-folder)))

(defn get-all-albums [root]
  (for [album-folder (get-all-sub-folders root)]
    {:folder-name album-folder :tango-info-url ""}))

(defn list-tracks [album]
  (sort-tracks (for [track (get-files (:folder-name album))
                     :when (is-track? track)]
                  track)))

(def +tango-info-format+
  "{:tin \"%tin%\"
  :tint \"%tint%\"
  :album \"%album%\"
  :album-artist \"%album artist%\"
  :artists \"%artist_vocmarker_off%\"
  :album-collection \"%album_collection%\"
  :title \"%title%\"
  :genre \"%genre%\"
  :year \"%year%\"
  :orchestra \"%track_orch%\"
  :vocalist \"%track_voca%\"
  :track \"%track%\"
  :disc-no \"%discnumber%\"
  :track-total \"%disc_tqty%\"
  :disc-total \"%album_disc_qty%\"
  :language \"%iso_639_3%\"
  :work-id \"%tiwc%\"
  :date \"%date%\"
  :duration \"%duration%\"} ")

(defn download-tango-info [album]
  (let [tango-info-content (:body (http/post "https://tango.info/tracktagger/1.4.1" {:form-params {:track_references (:tango-info-url album)
                                                                                                   :format_string +tango-info-format+}}))]
    (spit (tango-info-cache-file (:folder-name album)) tango-info-content)
    tango-info-content))

(def +skip-tango-info-cache+ false)

(defn get-cached-tango-info [album]
  (let [tango-info-cache-file (tango-info-cache-file (:folder-name album))]
    (when (and (not +skip-tango-info-cache+) (file-exists? tango-info-cache-file))
      (slurp tango-info-cache-file))))

(defn extract-tango-info [content]
  (let [html (enl/html-resource (java.io.StringReader. content))
        tag-list (enl/text (first (enl/select html [:#tag_list])))]
    (read-string (str "[" tag-list "]"))))

(defn get-tango-info [album]
  (extract-tango-info (or (get-cached-tango-info album)
                          (download-tango-info album))))

(defn convert-to-id3v23-tag [file]
  (let [audio-file (org.jaudiotagger.audio.AudioFileIO/read (io/file file))]
    (.getTagAndConvertOrCreateAndSetDefault audio-file)
    (.commit audio-file)))


(defn fix-year [year]
  (let [parts (cond (.contains year "/") (.split year "/")
                    (.contains year "-") (.split year "-")
                    :elses [year])]
    (first (filter (fn [part]
                     (try
                       (let [parsed-year (Integer. part)]
                         (and (< parsed-year 2050) (> parsed-year 1900)))
                       (catch Exception e
                         false)))
                   parts))))

(defn tag-file! [track track-info]
  (let [vocalist (when (not= (:vocalist track-info) "-")
                   (normalize-unicode-to-ascii (:vocalist track-info)))]
    (println "Tagging file " track)
    (println track-info)

    (write-custom-tag! (io/file track) "PerformanceDate" (:date track-info))
    (write-custom-tag! (io/file track) "WorkId" (:work-id track-info))
    (write-custom-tag! (io/file track) "TangoInfoTIN" (:tin track-info))
    (write-custom-tag! (io/file track) "TangoInfoTINT" (:tint track-info))

    (id3/write-tag! (io/file track)
                    :artist (normalize-unicode-to-ascii (:orchestra track-info))
                    :album-artist (normalize-unicode-to-ascii (:album-artist track-info))
                    :artists (normalize-unicode-to-ascii (:artists track-info))
                    :album (:album track-info)
                    :title (normalize-unicode-to-ascii (:title track-info))
                    :disc-no (:disc-no track-info)
                    :disc-total (:disc-total track-info)
                    :track (:track track-info)
                    :track-total (:track-total track-info)
                    :genre (str/capitalize (when (not= (:genre track-info) "?")
                                             (:genre track-info)))
                    :record-label (when (not= (:album-collection track-info) "?")
                                    (:album-collection track-info))
                    :language (:language track-info)
                    :conductor (or vocalist "Instrumental")
                    :year (when (>= (count (:year track-info)) 4)
                            (:year track-info)))))

(defn tag-files! [tracks tracks-info]
  (if (= (count tracks) (count tracks-info))
    (domap tag-file! tracks tracks-info)
    (print "Number of tracks in folder and number of tracks on tango.info don't match. ")))

(defn -main
  ([] (-main "/Users/bruno/Transfer/Private/Tango/Traditional Orchestras"))
  ([root-folder]
   (.setLevel (java.util.logging.Logger/getLogger "org.jaudiotagger") java.util.logging.Level/OFF)
   (let [albums (get-albums root-folder)]
     (domap (fn [album]
              (let [tracks (list-tracks album)
                    tracks-info (get-tango-info album)]
                (tag-files! tracks tracks-info)))
            albums))))

(defn fix-tags [tags]
  (into {:track nil} (for [[k v] tags
             :when (not= k :cover-art)]
             {k (let [fixed-tag-value (-> v
                                          (.replace "\uFFE1" "")
                                          (.replace "\uFFFF" "")
                                          (.replace "\uFFFD" "")
                                          (.replace "\uFFE9" "e")
                                          (.replace "\uFFFA" "u")
                                          (.replace "\uFFF1" "n")
                                          (.replace "\uFFED" "i")
                                          (.replace "\uFFC1" "A")
                                          (.replace "Miguel Calo\u00CC\u0081" "Miguel Calo")
                                          (.replace "\uFFF3" "o"))]
                  (if (= k :year)
                    (fix-year fixed-tag-value)
                    fixed-tag-value))})))

(defn fix-file [file]
  (when (is-track? file)
    (try
      (let [tags (id3/read-tag (io/file file))]
        (convert-to-id3v23-tag file)
        (apply id3/write-tag! (flatten (into [(io/file file)] (fix-tags tags)))))
      (catch Exception e (println "Could not process track " file " due to " e)))))


(defn fix-unicode
  ([] (fix-unicode "/Users/bruno/Transfer/Private/Tango"))
  ([root-folder]
   (.setLevel (java.util.logging.Logger/getLogger "org.jaudiotagger") java.util.logging.Level/OFF)
   (let [files (get-files root-folder)]
     (domap fix-file files))))


(defn fix-unicode-filename [file]
  (let [file-obj (io/file file)
        leaf-name (.getName file-obj)
        fixed-filename (normalize-unicode-to-ascii leaf-name)]
    (when-not (or (.endsWith leaf-name ".jpg")
                  (.endsWith leaf-name ".url")
                  (.endsWith leaf-name ".txt")
                  (.endsWith leaf-name ".html")
                  (.matches leaf-name "\\A\\p{ASCII}*\\z"))
      (println "Found non ASCII file " file))
    (when (not= leaf-name fixed-filename)
          (.renameTo file-obj
                     (io/file (.getParentFile file-obj) fixed-filename))
          (println "Renamed " (.toString file-obj) " to " fixed-filename))))


(defn fix-unicode-filenames
  ([] (fix-unicode-filenames "/Users/bruno/Transfer/Private/Tango"))
  ([root-folder]
   (let [files (get-files-and-folders root-folder)]
     (domap fix-unicode-filename (reverse files)))))

;; TODO
;; handling of multiple values for artists/vocalists etc.
;; how to mark if song is instrumental
;; scrape info for single songs
;; file and folder renaming
;; album art

