(ns lda_clj.sampler
  (:use [lda_clj.document])
  (:use [lda_clj.corpora]))

(defn dec-topic-in-document [doc ^Integer idx]
  (let [w (doc :w)
	z (doc :z)
	Nz (doc :Nz)
	^Integer current-topic-id (z idx)]
    (struct document
	    w
	    (assoc z idx nil)
	    (update-in Nz [current-topic-id] dec))))

(defn inc-topic-in-document [doc ^Integer idx ^Integer new-topic-id]
  (let [w (doc :w)
	z (doc :z)
	Nz (doc :Nz)]
    (struct document
	    w
	    (assoc z idx new-topic-id)
	    (update-in Nz [new-topic-id] inc))))

(defn dec-topic-in-corpora [corp ^Integer doc-idx ^Integer word-idx]
  (let [documents (corp :documents)
	Nz (corp :Nz)
	Nzw (corp :Nzw)
	^Integer V (corp :V)
	^Integer K (corp :K)
	current-doc (documents doc-idx)
	^Integer current-word-id ((current-doc :w) word-idx)
	^Integer current-topic-id ((current-doc :z) word-idx)]
    (struct corpora
	    (assoc documents doc-idx
		   (dec-topic-in-document current-doc word-idx))
	    (update-in Nz [current-topic-id] dec)
	    (update-in Nzw [current-topic-id current-word-id ] dec)
	    V K)))

(defn inc-topic-in-corpora [corp ^Integer doc-idx ^Integer word-idx ^Integer new-topic-id]
  (let [documents (corp :documents)
	Nz (corp :Nz)
	Nzw (corp :Nzw)
	^Integer V (corp :V)
	^Integer K (corp :K)
	current-doc (documents doc-idx)
	^Integer current-word-id ((current-doc :w) word-idx)]
    (struct corpora
	    (assoc documents doc-idx
		   (inc-topic-in-document current-doc word-idx new-topic-id))
	    (update-in Nz [new-topic-id] inc)
	    (update-in Nzw [new-topic-id current-word-id] inc)
	    V K)))

(defn ^Double gen-likelihood-prob [^Integer Nz ^Integer Nzw ^Integer V ^Double beta]
  (/ (+ Nzw beta) (+ Nz (* beta V))))

(defn ^Double gen-prior-prob [^Integer Nz ^Double alpha]
  (+ Nz alpha))

(defn ^Double gen-post-prob
  [^Integer Nz ^Integer Nzw ^Integer V ^Double beta
   ^Integer Ndz ^Double alpha]
  (* (gen-likelihood-prob Nz Nzw V beta)
     (gen-prior-prob Ndz alpha)))
