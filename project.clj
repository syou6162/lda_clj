(defproject lda_clj "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [
		 [org.clojure/clojure "1.2.0"]
		 [org.clojure/clojure-contrib "1.2.0"]
		 [org.apache.commons/commons-math "2.2"]
		 ; [incanter "1.3.0-SNAPSHOT"]
		 ]
  :dev-dependencies [[swank-clojure "1.3.2"]]
  :main lda_clj.core
  ; :java-source-path [["src/lda_clj"]]
  :jvm-opts ["-server" "-Xmx10024m"]
  :resources-path "resource")
