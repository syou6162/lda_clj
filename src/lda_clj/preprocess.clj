(ns lda_clj.preprocess
  (:use [clojure.contrib.import-static :only (import-static)])
  (:use [clojure.contrib.string :only (split)])
  (:use [clojure.string :only (lower-case)])
  (:use [opennlp.nlp])
  (:import (org.apache.commons/math.random.MersenneTwister)))

(def bin-dir "./model_files")
(def get-sentences (make-sentence-detector (str bin-dir "/" "en-sent.bin")))
(def tokenize (make-tokenizer (str bin-dir "/" "en-token.bin")))
(def pos-tag (make-pos-tagger (str bin-dir "/" "en-pos-maxent.bin")))

(def ^:dynamic *permitted-tags* #{"NN", "NNS", "NNP", "NNPS"})

(def ^:dynamic *stop-words*
     (let [result (->> (slurp "./stop_words")
		       (split #"\r\n")
		       (set))
	   additional-stop-words ["+" "-" "(" ")" "." "," "'s" "%" "n't" "'m" "'ve" "'re" "does"]
	   added (reduce conj result additional-stop-words)]
       added))

(defn permitted-tag? [[word tag]]
  (contains? *permitted-tags* tag))

(defn stop-word? [word]
  (contains? *stop-words* word))

(defn get-bag-of-words [doc-str]
  (->> (for [sentence (get-sentences doc-str)]
	 (->> (tokenize sentence)
	      (pos-tag)
	      (filter permitted-tag?)
	      (map first)
	      (map lower-case)
	      (filter (complement stop-word?))))
       (remove empty?)
       (flatten)))
