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
	    (assoc Nz current-topic-id (dec (Nz current-topic-id))))))

(defn inc-topic-in-document [doc ^Integer idx ^Integer new-topic-id]
  (let [w (doc :w)
	z (doc :z)
	Nz (doc :Nz)]
    (struct document
	    w
	    (assoc z idx new-topic-id)
	    (assoc Nz new-topic-id (inc (Nz new-topic-id))))))

(defn dec-topic-in-corpora [corp ^Integer doc-idx ^Integer word-idx]
  (let [documents (corp :documents)
	Nz (corp :Nz)
	Nzw (corp :Nzw)
	^Integer V (corp :V)
	current-doc (documents doc-idx)
	^Integer current-word-id ((current-doc :w) word-idx)
	^Integer current-topic-id ((current-doc :z) word-idx)
	^Integer Nzw-elem ((Nzw current-topic-id) current-word-id)]
    (struct corpora
 	    (assoc documents doc-idx
		   (dec-topic-in-document current-doc word-idx))
	    (assoc Nz current-topic-id (dec (Nz current-topic-id)))
	    (assoc Nzw current-topic-id
		   (assoc (Nzw current-topic-id) current-word-id (dec Nzw-elem)))
	    V)))

(defn inc-topic-in-corpora [corp ^Integer doc-idx ^Integer word-idx ^Integer new-topic-id]
  (let [documents (corp :documents)
	Nz (corp :Nz)
	Nzw (corp :Nzw)
	^Integer V (corp :V)
	current-doc (documents doc-idx)
	^Integer current-word-id ((current-doc :w) word-idx)
	^Integer Nzw-elem ((Nzw new-topic-id) current-word-id)]
    (struct corpora
	    (assoc documents doc-idx
		   (inc-topic-in-document current-doc word-idx new-topic-id))
	    (assoc Nz new-topic-id (inc (Nz new-topic-id)))
	    (assoc Nzw new-topic-id
		   (assoc (Nzw new-topic-id) current-word-id (inc Nzw-elem)))
	    V)))

(defn ^Double gen-likelihood-prob [corpora ^Integer word-id ^Integer topic-id ^Double beta]
  (let [^Integer Nz ((deref (corpora :Nz)) topic-id)
	^Integer Nzw ((deref ((corpora :Nzw) topic-id)) word-id)
	^Integer V (corpora :V)]
    (/ (+ Nzw beta) (+ Nz (* beta V)))))

(defn ^Double gen-prior-prob [document ^Integer topic-id ^Integer K ^Double alpha]
  (let [^Integer Nz ((deref (document :Nz)) topic-id)]
    ;; 分母は定数なので割らない。比率だけ分かればよい
    (+ Nz alpha)))
