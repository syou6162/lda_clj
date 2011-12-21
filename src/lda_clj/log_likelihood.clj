(ns lda_clj.log_likelihood
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
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
				(logGamma (+ (((corpora :Nzw) z) v) @beta))))
		    (logGamma (+ ((corpora :Nz) z) (* @beta V)))))))))

(defn ^Double log-likelihood [corpora]
  (+ (calc-prior-term (corpora :documents))
     (calc-likelihood-term corpora)))