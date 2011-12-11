(ns lda_clj.corpora
  (:use [lda_clj.document])
  (:use [lda_clj.util]))

(defstruct corpora :documents :Nz :Nzw :V)

(defn create-corpora [documents V]
  (struct corpora
	  (vec (for [d documents]
	    (create-document d)))
	  (atom (vec (for [idx (range @K)] 0)))
	  (vec (for [t (range @K)] ;; topic loop
		 (atom (vec (for [v (range V)] ;; vocabulary loop
			      0)))))
	  V))

(defn create-corpora-with-random-topic-assignments [documents V]
  (let [documents (for [d documents]
		    (create-document-with-random-topic-assignments d))
	z-w-seq (for [d documents
		      idx (range (count (d :w)))]
		  (let [z ((deref (d :z)) idx)
			w ((d :w) idx)]
		    [z w]))
	Nzw (reduce (fn [count v]
		      (let [[z w] v
			    Nz (count z)
			    Nzw (Nz w)]
			(assoc count z (assoc Nz w (inc Nzw)))))
		    (vec (repeat @K (vec (repeat V 0)))) z-w-seq)]
    (struct corpora
	    (vec documents)
	    (atom (vec (for [z (range @K)]
			 (reduce + (Nzw z)))))
	    (vec (map #(atom %) Nzw)) V)))

(defn corpora-map [f corp]
  (let [documents (vec (map (fn [d]
			      (document-map f d))
			    (corp :documents)))
	Nz (f (corp :Nz))
	Nzw (vec (map (fn [Nz] (f Nz)) (corp :Nzw)))
	V (corp :V)]
    (struct corpora documents Nz Nzw V)))

; (create-corpora '[[1 2] [3]] 4)
;; (create-corpora-with-random-topic-assignments '[[1 2] [3]] 4)
;; (corpora-map deref (create-corpora-with-random-topic-assignments '[[1 2] [3]] 4))

(defn valid-corpora? [corpora]
  (letfn [(valid-topic-num? [topic-id]
			    (= ((corpora :Nz) topic-id)
			       (reduce + ((corpora :Nzw) topic-id)))
			    )]
    (every? #(and true %) (map #(valid-topic-num? %) (range @K)))))