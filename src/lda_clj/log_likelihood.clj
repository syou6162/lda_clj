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
