(ns lda_clj.corpora
  (:use [lda_clj.document])
  (:use [lda_clj.util]))

(defstruct corpora :documents :Nz :Nwz :V :K)

(defn create-corpora [documents V K]
  (struct corpora
	  (vec (for [d documents]
		 (create-document d K)))
	  (vec (for [idx (range K)] 0))
	  (vec (for [t (range V)] ;; vocabulary loop
		 (vec (for [v (range K)] ;; topic loop
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
	w-z-seq (vec (for [d documents
			   idx (range (count (d :w)))]
		       (let [w ((d :w) idx)
			     z ((d :z) idx)]
			 [w z])))
	z-freq (frequencies (map second w-z-seq))
	Nwz (let [count-table (gen-count-table w-z-seq)]
	      (vec (for [w (range V)]
		     (vec (for [t (range K)]
			    (get-in count-table [w t] 0))))))]
    (struct corpora
	    (vec documents)
	    (->> (range K)
		 (map (fn [z] (get-in z-freq [z] 0)))
		 (vec))
	    Nwz V K)))

(defn corpora-map [f corp]
  (let [documents (vec (map (fn [d]
			      (document-map f d))
			    (corp :documents)))
	Nz (vec (map f (corp :Nz)))
	Nwz (vec (map (fn [Nz] (vec (map f Nz))) (corp :Nwz)))
	V (corp :V)]
    (struct corpora documents Nz Nwz V)))

(defn valid-corpora? [corp]
  (letfn [(valid-topic-num? [topic-id]
			    (= ((corp :Nz) topic-id)
			       (reduce + ((corp :Nzw) topic-id)))
			    )]
    (every? #(and true %) (map #(valid-topic-num? %) (range (corp :K))))))