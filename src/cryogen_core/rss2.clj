(ns cryogen-core.rss2
  (:require [clojure.data.xml :as xml]
            [text-decoration.core :refer :all]
            [cryogen-core.io :as cryogen-io])
  (:import java.util.Date))

(defn posts-to-items [^String site-url posts]
  (map
    (fn [{:keys [uri title content date enclosure author description] :as post}]
      (let [link (str (if (.endsWith site-url "/") (apply str (butlast site-url)) site-url) uri)
            media-group (when (:video-path post) 
                              {:src (str "https://ispooge.com/embed.html#" (:video-path post))
                               :thumbnail-src (str site-url (or (:video-thumbnail-url post)
                                                                (str "/media/videos/" (:video-path post) ".jpg")))
                               :width "1280"
                               :height "720"
                               :type "text/html"})]
 
        (merge {:guid        link
                :link        link  
                :title       title
                :description (or description content)
                :author      author
                :pubDate     date}
               (when-let [yt-id (:video-yt-id post)] {:youtube/id yt-id})
               (when media-group {:media/group media-group})
               (if enclosure {:enclosure   enclosure}))))
    posts))

;xml/alias-uri xml/emit-str
(xml/alias-uri 'atom "http://www.w3.org/2005/Atom")
(xml/alias-uri 'media "http://search.yahoo.com/mrss/")
(xml/alias-uri 'activity "http://activitystrea.ms/spec/1.0/")
(xml/alias-uri 'yt "http://www.youtube.com/xml/schemas/2015")

(defn make-media-group [item]
  (let [media-group (:media/group item)
        title [::media/title (:title item)]
        content [::media/content {:url (:src media-group)
                                  :height (:height media-group)
                                  :width (:width media-group)
                                  :type (:type media-group)}]
        thumbnail [::media/thumbnail {:url (:thumbnail-src media-group)
                                  :height (:height media-group)
                                  :width (:width media-group)
                                  :type (:type media-group)}]
        ;description [::media/description {} (:description item)]
        ]
    [::media/group {} title content thumbnail]))

(defn make-item [item]
  (let [{:keys [guid title link description author pubDate]} item
        media-group (:media/group item)
        yt-id (:youtube/id item)]
    [:item {}
      (when guid [:guid {} guid])
      (when title [:title {} title])
      (when link [:link {} link])
      (when media-group (make-media-group item))
      (when yt-id [::yt/videoId {} yt-id])
      (when description [:description {} description])
      (when pubDate [:pubDate {} pubDate])]))
      
(defn make-channel [config posts]
  (let [self-link [::atom/link {:rel "self"
                                :type "application/rss"
                                :href (:site-url config)}]
        title [:title (:site-title config)]

        items (posts-to-items (:site-url config) posts)
        
        items (map make-item items)
        
        channel [:channel {} title self-link items]
        
        rss-doc [:rss {:version "2.0"} channel]]
        
    (xml/emit-str (xml/sexp-as-element rss-doc))))


(defn make-filtered-channels [{:keys [rss-filters blog-prefix] :as config} posts-by-tag]
  (doseq [filter rss-filters]
    (let [uri (cryogen-io/path "/" blog-prefix (str (name filter) ".xml"))]
      (println "\t-->" (cyan uri))
      (cryogen-io/create-file uri (make-channel config (get posts-by-tag filter))))))
