(ns lda_clj.test.log_likelihood
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.core])
  (:use [lda_clj.util])
  (:use [lda_clj.corpora])
  (:use [lda_clj.document])
  (:use [clojure.test]))

(use '[clojure.contrib.duck-streams :only (reader)])
(use '[clojure.contrib.string :only (split)])

(def wsj-filename "/Users/yasuhisa/nCRP/wsj.txt")
(def word2id-map (atom {}))

(defn read-raw-documents [filename]
  (let [lines (line-seq (reader filename))]
    (for [line (map #(split #"\s" %) lines)]
			(for [word line]
			  (let [[new-map word-id] (get-word-id [@word2id-map word])]
			    (do
			      (reset! word2id-map new-map)
			      word-id))))))

(deftest test-log-likelihood
  (let [raw-documents (read-raw-documents wsj-filename)
	documents (map #(create-document-with-random-topic-assignments %)
		       raw-documents)]
  (is (neg?
       (calc-prior-term documents)))
  (is (neg?
       (calc-likelihood-term 
	(create-corpora-with-random-topic-assignments
	  raw-documents (count @word2id-map)))))))