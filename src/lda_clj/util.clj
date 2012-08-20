(ns lda_clj.util
  (:use [clojure.string :only (split)])
  (:use [clojure.java.io :only (reader)])
  (:use [lda_clj.preprocess]))

(defn get-word-id [word2id word]
  (assoc word2id word (get word2id word (count word2id))))

(defn get-words-ids [word2id words]
  (reduce (fn [m w] (get-word-id m w)) word2id words))

(defn read-raw-docs [filename]
  (map #(get-bag-of-words %) (split (slurp filename) #"\n")))
