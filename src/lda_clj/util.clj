(ns lda_clj.util
  (:import (org.apache.commons/math.random.MersenneTwister)))

(use '[clojure.contrib.import-static :only (import-static)])

(def my-mt (new org.apache.commons.math.random.MersenneTwister))

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

(use '[clojure.contrib.duck-streams :only (reader read-lines)])
(use '[clojure.contrib.string :only (split)])

(defn read-raw-docs [filename]
  (for [line (read-lines filename)]
    (split #"\s" line)))
