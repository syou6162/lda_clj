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
	Nwz (corp :Nwz)
	^Integer V (corp :V)
	^Integer K (corp :K)
	current-doc (documents doc-idx)
	^Integer current-word-id ((current-doc :w) word-idx)
	^Integer current-topic-id ((current-doc :z) word-idx)]
    (struct corpora
	    (assoc documents doc-idx
		   (dec-topic-in-document current-doc word-idx))
	    (update-in Nz [current-topic-id] dec)
	    (update-in Nwz [current-word-id current-topic-id] dec)
	    V K)))

(defn inc-topic-in-corpora [corp ^Integer doc-idx ^Integer word-idx ^Integer new-topic-id]
  (let [documents (corp :documents)
	Nz (corp :Nz)
	Nwz (corp :Nwz)
	^Integer V (corp :V)
	^Integer K (corp :K)
	current-doc (documents doc-idx)
	^Integer current-word-id ((current-doc :w) word-idx)]
    (struct corpora
	    (assoc documents doc-idx
		   (inc-topic-in-document current-doc word-idx new-topic-id))
	    (update-in Nz [new-topic-id] inc)
	    (update-in Nwz [current-word-id new-topic-id] inc)
	    V K)))

(defn ^Double gen-likelihood-prob [corpora ^Integer word-id ^Integer topic-id ^Double beta]
  (let [^Integer Nz ((deref (corpora :Nz)) topic-id)
	^Integer Nwz ((deref ((corpora :Nwz) word-id)) topic-id)
	^Integer V (corpora :V)]
    (/ (+ Nwz beta) (+ Nz (* beta V)))))

(defn ^Double gen-prior-prob [document ^Integer topic-id ^Integer K ^Double alpha]
  (let [^Integer Nz ((deref (document :Nz)) topic-id)]
    ;; 分母は定数なので割らない。比率だけ分かればよい
    (+ Nz alpha)))

(defn ^Double my-gen-likelihood-prob [^Integer Nz ^Integer Nzw ^Integer V ^Double beta]
  (/ (+ Nzw beta) (+ Nz (* beta V))))

(defn ^Double my-gen-prior-prob [^Integer Nz ^Double alpha]
  (+ Nz alpha))

(defn ^Double my-gen-post-prob
  [^Integer Nz ^Integer Nzw ^Integer V ^Double beta
   ^Integer Ndz ^Double alpha]
  (* (my-gen-likelihood-prob Nz Nzw V beta)
     (my-gen-prior-prob Ndz alpha)))
