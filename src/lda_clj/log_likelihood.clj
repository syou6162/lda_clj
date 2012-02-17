(ns lda_clj.log_likelihood
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler])
  (:use [lda_clj.document])
  (:use [clojure.contrib.import-static :only (import-static)]))

(import-static org.apache.commons.math.special.Gamma logGamma)

(defn ^Double calc-prior-term [corp alpha]
  (let [documents (corp :documents)
	N (count documents)
	K (corp :K)]
    (+ (- (* N (logGamma (* alpha K)))
	  (* (* N K) (logGamma alpha)))
       (reduce + (for [d documents]
		   (- (reduce + (for [z (range K)]
				  (logGamma (+ ((d :Nz) z) alpha))))
		      (logGamma (+ (count (d :w)) (* alpha K)))))))))

(defn ^Double calc-likelihood-term [corp beta]
  (let [V (corp :V)
	K (corp :K)]
    (+ (- (* K (logGamma (* beta V)))
	  (* (* K V) (logGamma beta)))
       (reduce + (for [z (range K)]
		   (- (reduce + (for [v (range V)]
				  (logGamma (+ (get-in corp [:Nzw z v]) beta))))
		      (logGamma (+ (get-in corp [:Nz z]) (* beta V)))))))))

(defn ^Double log-likelihood [corp alpha beta]
  (+ (calc-prior-term corp alpha)
     (calc-likelihood-term corp beta)))

(defn sample-by-java-util [lis]
  ; http://java.sun.com/j2se/1.5.0/ja/docs/ja/api/java/util/Collections.html
  ; キーがない場合は (-(挿入ポイント) - 1)が返ってくる
  (let [position (java.util.Collections/binarySearch lis (* (rand) (lis (dec (count lis)))))]
    (if (> position 0.0) 
      position ; 見つけた場合。実数なのでほぼないけど...
      (dec (Math/abs position)))))

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
				       (let [posterior (my-gen-post-prob (Nz topic-id)
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
