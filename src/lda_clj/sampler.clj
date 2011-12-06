(ns lda_clj.sampler
  (:use [lda_clj.document])
  (:use [lda_clj.corpora]))

(defn gen-likelihood-prob [corpora word-id topic-id beta]
  (let [Nz ((corpora :Nz) topic-id)
	Nzw (((corpora :Nzw) topic-id) word-id)
	V (corpora :V)]
    (/ (+ Nzw beta) (+ Nz (* beta V)))))

(defn gen-prior-prob [document topic-id K alpha]
  (let [Nz ((document :Nz) topic-id)
	N (count (document :w))]
    (/ (+ Nz alpha) (+ N (* alpha K)))))
