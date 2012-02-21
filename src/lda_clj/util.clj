(ns lda_clj.util
  (:use [clojure.contrib.import-static :only (import-static)])
  (:use [clojure.contrib.duck-streams :only (reader read-lines)])
  (:use [clojure.contrib.string :only (split)])
  (:use [opennlp.nlp])
  (:import (org.apache.commons/math.random.MersenneTwister)))

(def my-mt (new org.apache.commons.math.random.MersenneTwister))

(def bin-dir "./model_files")
(def get-sentences (make-sentence-detector (str bin-dir "/" "en-sent.bin")))
(def tokenize (make-tokenizer (str bin-dir "/" "en-token.bin")))
(def pos-tag (make-pos-tagger (str bin-dir "/" "en-pos-maxent.bin")))

(def permitted-tags #{"NN", "NNS", "NNP", "NNPS"})

(use '[clojure.contrib.string :only (split)])
(def stop-words (let [result (->> (slurp "/Users/yasuhisa/Dropbox/lda_clj/stop_words")
                                  (split #"\r\n")
                                  (set))
                      additional-stop-words ["+" "-" "(" ")" "." "," "'s" "%" "n't" "'m" "'ve" "'re" "does"]
                      added (reduce conj result additional-stop-words)]
		  added))

(defn ^Integer my-sample [^doubles xs]
  (let [r (* (reduce + xs) (rand))]
    (loop [idx 0, cum 0.0]
      (let [val (+ cum (xs idx))]
	(if (>= val r)
	  idx
	  (recur (inc idx) val))))))

(defn get-word-id [word2id word]
  (assoc word2id word (get word2id word (count word2id))))

(defn get-words-ids [word2id words]
  (reduce (fn [m w] (get-word-id m w)) word2id words))

(defn read-raw-docs [filename]
  (for [line (read-lines filename)]
    (map first (filter (fn [[word tag]]
			 (and
			  (not (contains? stop-words word))
			  (contains? permitted-tags tag)))
		       (pos-tag (tokenize line))))))
