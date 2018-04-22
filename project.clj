(defproject cryogen-core "0.1.60-harlanji-SNAPSHOT"
            :description "Cryogen's compiler"
            :url "https://github.com/cryogen-project/cryogen-core"
            :license {:name "Eclipse Public License"
                      :url  "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.8.0"]
                           [camel-snake-kebab "0.4.0"]
                           [cheshire "5.8.0"]
                           [clj-rss "0.2.3"] ; 0.2.4-harlanji-SNAPSHOT
                           [org.clojure/data.xml "0.2.0-alpha5"]
                           [clj-text-decoration "0.0.3"]
                           [enlive "1.1.6"]
                           [hawk "0.2.11"]
                           [hiccup "1.0.5"]
                           [io.aviso/pretty "0.1.34"]
                           [me.raynes/fs "1.4.6"]
                           [pandect "0.6.1"]
                           [prismatic/schema "1.1.7"]
                           [selmer "1.11.2"]]
            :deploy-repositories [["snapshots" :clojars]
                                  ["releases" :clojars]])
