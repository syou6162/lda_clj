(ns lda_clj.corpora
  (:use [lda_clj.document])
  (:use [lda_clj.util]))

(defstruct corpora :documents :Nz :Nzw :V)

(defn create-corpora [documents V]
  (struct corpora
	  (vec (for [d documents]
	    (create-document d)))
	  (vec (for [idx (range @K)] 0))
	  (vec (for [t (range @K)] ;; topic loop
		 (vec (for [v (range V)] ;; vocabulary loop
			0))))
	  V))

(defn create-corpora-with-random-topic-assignments [documents V]
  (let [documents (for [d documents]
		    (create-document-with-random-topic-assignments d))
	z-w-seq (for [d documents
		      idx (range (count (d :w)))]
		  (let [z ((d :z) idx)
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
	    (vec (for [z (range @K)]
		   (reduce + (Nzw z))))
	    Nzw V)))

(defn valid-corpora? [corpora]
  (letfn [(valid-topic-num? [topic-id]
			    (= ((corpora :Nz) topic-id)
			       (reduce + ((corpora :Nzw) topic-id)))
			    )]
    (every? #(and true %) (map #(valid-topic-num? %) (range @K)))))