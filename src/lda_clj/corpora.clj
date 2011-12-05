(ns lda_clj.corpora
  (:use [lda_clj.document])
  (:use [lda_clj.util]))

(defstruct corpora :documents :Nz :Nzw :V)

(def V 10)

(defn create-corpora [documents V]
  (struct corpora
	  (for [d documents]
	    (create-document d))
	  (for [idx (range @K)] nil)
	  (for [t (range @K)] ;; topic loop
	    (for [v (range V)] ;; vocabulary loop
	      nil))
	  V))

(defn create-corpora-with-random-topic-assignments [documents V]
  (let [documents (for [d documents]
		    (create-document-with-random-topic-assignments d))
	z-w-seq (for [d documents
		      idx (range (count (d :w)))]
		  (let [z (nth (d :z) idx)
			w (nth (d :w) idx)]
		    [z w]))
	Nzw (reduce (fn [count v]
		      (let [z (first v)
			    w (second v)
			    Nz (count z)
			    Nzw (Nz w)]
			(assoc count z (assoc Nz w (inc Nzw)))))
		    (vec (repeat @K (vec (repeat V 0)))) z-w-seq)]
    (struct corpora
	    documents
	    (vec (for [z (range @K)]
		   (reduce + (get Nzw z))))
	    Nzw V)))