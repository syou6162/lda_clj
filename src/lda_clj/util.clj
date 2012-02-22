(ns lda_clj.util
  (:use [lda_clj.preprocess])
  (:use [clojure.contrib.duck-streams :only (reader read-lines)]))

(defn get-word-id [word2id word]
  (assoc word2id word (get word2id word (count word2id))))

(defn get-words-ids [word2id words]
  (reduce (fn [m w] (get-word-id m w)) word2id words))

(defn read-raw-docs [filename]
  (for [line (read-lines filename)] (get-bag-of-words line)))