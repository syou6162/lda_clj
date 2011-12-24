(ns lda_clj.corpora
  (:use [lda_clj.document])
  (:use [lda_clj.util]))

(defstruct corpora :documents :Nz :Nwz :V)

(defn create-corpora [documents V]
  (struct corpora
	  (vec (for [d documents]
	    (create-document d)))
	  (vec (for [idx (range @K)] (atom 0)))
	  (vec (for [t (range V)] ;; vocabulary loop
		 (vec (for [v (range @K)] ;; topic loop
			(atom 0)))))
	  V))

(defn gen-count-table [seq]
  (->> seq
       (group-by first)
       (map (fn [[w lis]] {w (map second lis)}))
       (apply merge)
       (map (fn [[w lis]] {w (frequencies lis)}))
       (apply merge)))

(defn create-corpora-with-random-topic-assignments [documents V]
  (let [documents (for [d documents]
		    (create-document-with-random-topic-assignments d))
	w-z-seq (for [d documents
		      idx (range (count (d :w)))]
		  (let [w ((d :w) idx)
			z @((d :z) idx)]
		    [w z]))
	z-freq (frequencies (map second w-z-seq))
	Nwz (let [count-table (gen-count-table w-z-seq)]
	      (vec (for [w (range V)]
		     (vec (for [t (range @K)]
			    (let [cnt (count-table w)
				  n (if cnt (cnt t) 0)]
			      (atom (if n n 0))))))))]
    (struct corpora
	    (vec documents)
	    (->> (range @K)
		 (map #(let [cnt (z-freq %)]
			 (atom (if cnt cnt 0))))
		 (vec))
	    Nwz @K)))

; (create-corpora-with-random-topic-assignments '[[1 2 2] [1 1 1]] 3)

(defn corpora-map [f corp]
  (let [documents (vec (map (fn [d]
			      (document-map f d))
			    (corp :documents)))
	Nz (vec (map f (corp :Nz)))
	Nwz (vec (map (fn [Nz] (vec (map f Nz))) (corp :Nwz)))
	V (corp :V)]
    (struct corpora documents Nz Nwz V)))

; (create-corpora '[[1 2] [3]] 4)
;; (create-corpora-with-random-topic-assignments '[[1 2] [3]] 4)
;; (corpora-map deref (create-corpora-with-random-topic-assignments '[[1 2] [3]] 4))

(defn valid-corpora? [corpora]
  (letfn [(valid-topic-num? [topic-id]
			    (= ((corpora :Nz) topic-id)
			       (reduce + ((corpora :Nzw) topic-id)))
			    )]
    (every? #(and true %) (map #(valid-topic-num? %) (range @K)))))