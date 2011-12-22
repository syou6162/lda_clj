(ns lda_clj.corpora
  (:use [lda_clj.document])
  (:use [lda_clj.util]))

(defstruct corpora :documents :Nz :Nwz :V)

(defn create-corpora [documents V]
  (struct corpora
	  (vec (for [d documents]
	    (create-document d)))
	  (atom (vec (for [idx (range @K)] 0)))
	  (vec (for [t (range V)] ;; vocabulary loop
		 (atom (vec (for [v (range @K)] ;; topic loop
			      0)))))
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
			z ((deref (d :z)) idx)]
		    [w z]))
	Nwz (let [count-table (gen-count-table w-z-seq)]
	      (vec (for [w (range V)]
		     (vec (for [t (range @K)]
			    (let [n ((count-table w) t)]
			      (if n n 0)))))))]
    (struct corpora
	    (vec documents)
	    (atom (vec (for [v (range V)]
			 (reduce + (Nwz v)))))
	    (vec (map #(atom %) Nwz)) @K)))

(defn corpora-map [f corp]
  (let [documents (vec (map (fn [d]
			      (document-map f d))
			    (corp :documents)))
	Nz (f (corp :Nz))
	Nwz (vec (map (fn [Nz] (f Nz)) (corp :Nwz)))
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