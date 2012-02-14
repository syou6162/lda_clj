(ns lda_clj.log_likelihood
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler])
  (:use [lda_clj.document]))

(use '[clojure.contrib.import-static :only (import-static)])
(import-static org.apache.commons.math.special.Gamma logGamma)

(defn ^Double calc-prior-term [corpora alpha]
  (let [documents (corpora :documents)
	N (count documents)]
    (+ (- (* N (logGamma (* alpha @K)))
	  (* (* N @K) (logGamma alpha)))
       (reduce + (for [d documents]
		   (- (reduce + (for [z (range @K)]
				  (logGamma (+ ((d :Nz) z) alpha))))
		      (logGamma (+ (count (d :w)) (* alpha @K)))))))))

(defn ^Double calc-likelihood-term [corpora beta]
  (let [V (corpora :V)]
    (+ (- (* @K (logGamma (* beta V)))
	  (* (* @K V) (logGamma beta)))
       (reduce + (for [z (range @K)]
		   (- (reduce + (for [v (range V)]
				  (logGamma (+ (get-in corpora [:Nwz v z]) beta))))
		      (logGamma (+ (get-in corpora [:Nz z]) (* beta V)))))))))

(defn ^Double log-likelihood [corpora]
  (+ (calc-prior-term corpora)
     (calc-likelihood-term corpora)))

(defn sample-by-java-util [lis]
  ; http://java.sun.com/j2se/1.5.0/ja/docs/ja/api/java/util/Collections.html
  ; キーがない場合は (-(挿入ポイント) - 1)が返ってくる
  (let [position (java.util.Collections/binarySearch lis (* (rand) (lis (dec (count lis)))))]
    (if (> position 0.0) 
      position ; 見つけた場合。実数なのでほぼないけど...
      (dec (Math/abs position)))))

(defn inference-for-word-level
  ([corp doc-idx]
     (inference-for-word-level corp doc-idx false))
  ([corp doc-idx init-flag]
     (let [alpha @alpha, beta @beta, V (corp :V)]
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
			Nwz ((old-corpora :Nwz) current-word-id)
			next-z (-> (loop [topic-id 0, v []]
				     (if (= topic-id @K)
				       v
				       (let [prev-val (if (= topic-id 0) 0.0 (v (dec topic-id)))
					     posterior (my-gen-post-prob (Nz topic-id) (Nwz topic-id) V beta
									 (Ndz topic-id) alpha)
					     new-cum (+ prev-val posterior)]
					 (recur
					  (inc topic-id)
					  (conj v new-cum)))))
				   (sample-by-java-util))
			new-corpora (inc-topic-in-corpora old-corpora doc-idx word-idx next-z)]
		    new-corpora)
		  (inc word-idx)))))))

(defn inference
  ([corp]
     (inference corp false))
  ([corp init-flag]
     (loop [current-corp corp
	    doc-idx 0]
       (if (= doc-idx (count (current-corp :documents)))
	 current-corp
	 (recur (inference-for-word-level current-corp doc-idx init-flag)
		(inc doc-idx))))))
