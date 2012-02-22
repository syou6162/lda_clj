(ns lda_clj.preprocess
  (:use [clojure.contrib.import-static :only (import-static)])
  (:use [clojure.contrib.duck-streams :only (reader read-lines)])
  (:use [clojure.contrib.string :only (split)])
  (:use [opennlp.nlp])
  (:import (org.apache.commons/math.random.MersenneTwister)))

(def bin-dir "./model_files")
(def get-sentences (make-sentence-detector (str bin-dir "/" "en-sent.bin")))
(def tokenize (make-tokenizer (str bin-dir "/" "en-token.bin")))
(def pos-tag (make-pos-tagger (str bin-dir "/" "en-pos-maxent.bin")))

(def permitted-tags #{"NN", "NNS", "NNP", "NNPS"})

(def stop-words (let [result (->> (slurp "./stop_words")
                                  (split #"\r\n")
                                  (set))
                      additional-stop-words ["+" "-" "(" ")" "." "," "'s" "%" "n't" "'m" "'ve" "'re" "does"]
                      added (reduce conj result additional-stop-words)]
		  added))