(ns lda_clj.log_likelihood
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler])
  (:use [lda_clj.document]))

(use '[clojure.contrib.import-static :only (import-static)])
(import-static org.apache.commons.math.special.Gamma logGamma)

(defn ^Double calc-prior-term [documents]
  (let [N (count documents)]
    (+ (- (* N (logGamma (* @alpha @K)))
	  (* (* N @K) (logGamma @alpha)))
       (reduce + (for [d documents]
		   (- (reduce + (for [z (range @K)]
				  (logGamma (+ ((d :Nz) z) @alpha))))
		      (logGamma (+ (count (d :w)) (* @alpha @K)))))))))

(defn ^Double calc-likelihood-term [corpora]
  (let [V (corpora :V)]
    (+ (- (* @K (logGamma (* @beta V)))
	  (* (* @K V) (logGamma @beta)))
       (reduce + (for [z (range @K)]
		 (- (reduce + (for [v (range V)]
				(logGamma (+ (((corpora :Nwz) v) z) @beta))))
		    (logGamma (+ ((corpora :Nz) z) (* @beta V)))))))))

(defn ^Double log-likelihood [corpora]
  (+ (calc-prior-term (corpora :documents))
     (calc-likelihood-term corpora)))

(defn inference-for-word-level
  ([corp doc-idx init-flag]
     (loop [current-corp corp
	    word-idx 0]
       (if (= word-idx (count (((current-corp :documents) doc-idx) :w)))
	 current-corp
	 (recur (let [old-corpora (if init-flag
				    current-corp
				    (dec-topic-in-corpora current-corp doc-idx word-idx))
		      current-doc ((old-corpora :documents) doc-idx)
		      current-word-id ((current-doc :w) word-idx)
		      Ndz (current-doc :Nz)
		      Nz (old-corpora :Nz)
		      Nwz ((old-corpora :Nwz) current-word-id)
		      next-z (my-sample (vec (map #(*
						    (my-gen-prior-prob
						     (Ndz %)
						     @alpha)
						    (my-gen-likelihood-prob
						     (Nz %)
						     (Nwz %)
						     (old-corpora :V) @beta))
						  (range @K))))
		      new-corpora (inc-topic-in-corpora old-corpora doc-idx word-idx next-z)]
		  new-corpora)
		(inc word-idx)))))
  ([corp doc-idx]
     (inference-for-word-level corp doc-idx false)))

(defn inference
  ([corp init-flag]
     (loop [current-corp corp
	    doc-idx 0]
       (if (= doc-idx (count (current-corp :documents)))
	 current-corp
	 (recur (inference-for-word-level current-corp doc-idx init-flag)
		(inc doc-idx)))))
  ([corp]
     (inference corp false)))
