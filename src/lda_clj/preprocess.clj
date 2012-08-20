(ns lda_clj.preprocess
  (:use [clojure.string :only (split lower-case)])
  (:use [opennlp.nlp]))

(def bin-dir "./model_files")
(def get-sentences (make-sentence-detector (str bin-dir "/" "en-sent.bin")))
(def tokenize (make-tokenizer (str bin-dir "/" "en-token.bin")))
(def pos-tag (make-pos-tagger (str bin-dir "/" "en-pos-maxent.bin")))

(def ^:dynamic *permitted-tags* #{"NN", "NNS", "NNP", "NNPS"})

(defn flip [f x y] (f y x))

(def ^:dynamic *stop-words*
     (let [result (->> (slurp "./stop_words")
		       (flip split #"\r\n")
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
