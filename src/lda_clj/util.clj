(ns lda_clj.util)

(defn get-word-id [word2id word]
  (assoc word2id word (get word2id word (count word2id))))
