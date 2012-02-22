(ns lda_clj.corpora
  (:use [lda_clj.document]))

(defstruct corpora :documents :Nz :Nzw :V :K)

(defn create-corpora [documents V K]
  (struct corpora
	  (vec (for [d documents]
		 (create-document d K)))
	  (vec (for [idx (range K)] 0))
	  (vec (for [v (range K)] ;; topic loop
		 (vec (for [z (range V)] ;; vocabulary loop
			0))))
	  V K))

(defn gen-count-table [seq]
  (->> seq
       (group-by first)
       (map (fn [[w lis]] {w (map second lis)}))
       (apply merge)
       (map (fn [[w lis]] {w (frequencies lis)}))
       (apply merge)))

(defn create-corpora-with-random-topic-assignments [documents V K]
  (let [documents (vec (for [d documents]
			 (create-document-with-random-topic-assignments d K)))
	z-w-seq (vec (for [d documents
			   idx (range (count (d :w)))]
		       (let [w ((d :w) idx)
			     z ((d :z) idx)]
			 [z w])))
	z-freq (frequencies (map first z-w-seq))
	Nwz (let [count-table (gen-count-table z-w-seq)]
	      (vec (for [t (range K)]
		     (vec (for [v (range V)]
			    (get-in count-table [t v] 0))))))]
    (struct corpora
	    (vec documents)
	    (->> (range K)
		 (map (fn [z] (get-in z-freq [z] 0)))
		 (vec))
	    Nwz V K)))

(defn valid-corpora? [corp]
  (letfn [(valid-topic-num? [topic-id]
			    (= ((corp :Nz) topic-id)
			       (reduce + ((corp :Nzw) topic-id)))
			    )]
    (every? #(and true %) (map #(valid-topic-num? %) (range (corp :K))))))