(ns lda_clj.sampler
  (:use [lda_clj.document])
  (:use [lda_clj.corpora]))

(defn dec-topic-in-document [doc idx]
  (let [w (doc :w)
	z (doc :z)
	Nz (doc :Nz)
	current-topic-id (z idx)]
    (struct document
	    w
	    (assoc z idx nil)
	    (assoc Nz current-topic-id (dec (Nz current-topic-id))))))

(defn inc-topic-in-document [doc idx new-topic-id]
  (let [w (doc :w)
	z (doc :z)
	Nz (doc :Nz)]
    (struct document
	    w
	    (assoc z idx new-topic-id)
	    (assoc Nz new-topic-id (inc (Nz new-topic-id))))))

(defn gen-likelihood-prob [corpora word-id topic-id beta]
  (let [Nz ((corpora :Nz) topic-id)
	Nzw (((corpora :Nzw) topic-id) word-id)
	V (corpora :V)]
    (/ (+ Nzw beta) (+ Nz (* beta V)))))

(defn gen-prior-prob [document topic-id K alpha]
  (let [Nz ((document :Nz) topic-id)
	N (count (document :w))]
    (/ (+ Nz alpha) (+ N (* alpha K)))))
