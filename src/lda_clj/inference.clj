(ns lda_clj.inference
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler]))

(defn inference-for-word-level
  ([corp doc-idx alpha beta]
     (inference-for-word-level corp doc-idx alpha beta false))
  ([corp doc-idx alpha beta init-flag]
     (let [V (corp :V)]
       (loop [current-corp corp, word-idx 0]
	 (if (= word-idx (count (((current-corp :documents) doc-idx) :w)))
	   current-corp
	   (recur (let [old-corpora (if init-flag
				      current-corp
				      (dec-topic-in-corpora current-corp doc-idx word-idx))
			current-doc ((old-corpora :documents) doc-idx)
			current-word-id ((current-doc :w) word-idx)
			Ndz (current-doc :Nz)
			Nz (old-corpora :Nz)
			next-z (-> (loop [topic-id 0, v []]
				     (if (= topic-id (dec (old-corpora :K)))
				       v
				       (let [posterior (gen-post-prob (Nz topic-id)
									 (get-in old-corpora [:Nzw topic-id current-word-id])
									 V beta
									 (Ndz topic-id) alpha)]
					 (recur
					  (inc topic-id)
					  (conj v posterior)))))
				   (my-sample))
			new-corpora (inc-topic-in-corpora old-corpora doc-idx word-idx next-z)]
		    new-corpora)
		  (inc word-idx)))))))

(defn inference
  ([corp alpha beta]
     (inference corp alpha beta false))
  ([corp alpha beta init-flag]
     (loop [current-corp corp
	    doc-idx 0]
       (if (= doc-idx (count (current-corp :documents)))
	 current-corp
	 (recur (inference-for-word-level current-corp doc-idx alpha beta init-flag)
		(inc doc-idx))))))