(defproject goreco "0.1.0-SNAPSHOT"
  :description "GoReCo relation extraction evaluation"
  :url "http://github.com/mitchellkoch/goreco"
  :license {:name "The MIT License" 
            :url "http://opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [mitchellkoch/fipe "0.1.0"]
                 [prismatic/plumbing "0.3.2"]
                 [wharf "0.1.0-SNAPSHOT"]
                 [cheshire "5.3.1"]
                 [me.raynes/fs "1.4.5"]
                 [http-kit "2.1.16"]
                 [im.chit/iroh "0.1.5"]
                 [alandipert/enduro "1.1.5"]
                 [slingshot "0.10.3"]
                 [org.clojure/tools.logging "0.3.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main goreco.core)
