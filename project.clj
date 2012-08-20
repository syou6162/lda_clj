(defproject lda_clj "0.0.5"
  :description "Simple Implementation of Latent Dirichlet Allocation"
  :dependencies [[org.clojure/clojure "1.4.0"]
		 [org.apache.commons/commons-math "2.2"]
		 [org.clojure/tools.cli "0.2.1"]
		 [cheshire "2.2.0"]
		 [clojure-opennlp "0.1.7"]]
  :dev-dependencies [[swank-clojure "1.3.2"]]
  :main lda_clj.core)
