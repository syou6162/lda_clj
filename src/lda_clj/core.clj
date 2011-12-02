(ns lda_clj.core)

(defn get-word-id [vec]
  (let [word2id-map (first vec)
	word (second vec)
	word-id (get word2id-map word)
	new-word-id (count word2id-map)]
    (if word-id
      [word2id-map word-id]
      [(assoc word2id-map word new-word-id) new-word-id])))

(defn get-words-ids [word2id-map words]
  (second (reduce (fn [result word]
		    (let [word2id-map (first result)
			  words-ids (second result)
			  tmp-result (get-word-id [word2id-map word])
			  word2id-map (first tmp-result)
			  new-word-id (second tmp-result)]
		      [(first tmp-result) (conj words-ids new-word-id)]))
		  [word2id-map []] words)))

