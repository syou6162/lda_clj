(ns lda_clj.util
  (:use [clojure.contrib.import-static :only (import-static)])
  (:use [clojure.contrib.duck-streams :only (reader read-lines)])
  (:use [clojure.contrib.string :only (split)])
  (:use [opennlp.nlp])
  (:use [lda_clj.preprocess])
  (:use [lda_clj.random]))

(use '[clojure.contrib.string :only (split)])

(defn ^Integer my-sample [^doubles xs]
  (let [r (* (reduce + xs) (.nextDouble *r*))]
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
