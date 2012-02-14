(ns lda_clj.test.log_likelihood
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.util])
  (:use [lda_clj.corpora])
  (:use [lda_clj.sampler])
  (:use [lda_clj.document])
  (:use [clojure.test]))

(use '[clojure.contrib.duck-streams :only (reader)])
(use '[clojure.contrib.string :only (split)])

(deftest test-log-likelihood
  (let [wsj-filename "./tmp.txt"
	word2id {}]
    (letfn [(read-raw-docs [filename]
				(let [lines (line-seq (reader filename))]
				  (for [line (map #(split #"\s" %) lines)]
				    line)))]
      (let [raw-docs (read-raw-docs wsj-filename)
	    word2id (get-words-ids {} (flatten raw-docs))
	    docs (for [doc raw-docs] (map #(get-in word2id %) doc))
	    corpora (create-corpora-with-random-topic-assignments docs (count word2id))]
	(is (neg?
	     (calc-prior-term corpora)))
	(is (neg?
	     (calc-likelihood-term corpora)))))))
