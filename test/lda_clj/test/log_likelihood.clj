(ns lda_clj.test.log_likelihood
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.util])
  (:use [lda_clj.corpora])
  (:use [lda_clj.sampler])
  (:use [lda_clj.document])
  (:use [clojure.test]))

(deftest test-log-likelihood
  (let [wsj-filename "./tmp.txt"
	word2id {}
	alpha 0.1
	beta 0.1
	raw-docs (read-raw-docs wsj-filename)
	word2id (get-words-ids {} (flatten raw-docs))
	docs (for [doc raw-docs] (map #(get-in word2id %) doc))
	num-of-topic 3
	corpora (create-corpora-with-random-topic-assignments docs (count word2id) num-of-topic)]
    (is (neg?
	 (calc-prior-term corpora alpha)))
    (is (neg?
	 (calc-likelihood-term corpora beta)))))
