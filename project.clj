(defproject tango-tagger "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main tango-tagger.core
  :aot  [tango-tagger.core]
  :repositories [["java.net snapshots" "https://maven.java.net/content/repositories/snapshots/"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [enlive "1.1.5"]
                 [clj-http "0.7.8"]
                 [net.jthink/jaudiotagger "2.2.0-SNAPSHOT"]
                 [claudio "0.1.2"]])
