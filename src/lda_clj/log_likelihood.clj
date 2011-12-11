(ns lda_clj.log_likelihood
  (:import (org.apache.commons.math.special.Gamma.logGamma))
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.document]))

(defn ^Double calc-prior-term [documents]
  (let [N (count documents)]
    (+ (- (* N (org.apache.commons.math.special.Gamma/logGamma (* @alpha @K)))
	  (* (* N @K) (org.apache.commons.math.special.Gamma/logGamma @alpha)))
       (reduce + (for [d documents]
		   (- (reduce + (for [z (range @K)]
				  (org.apache.commons.math.special.Gamma/logGamma (+ ((d :Nz) z) @alpha))))
		      (org.apache.commons.math.special.Gamma/logGamma (+ (count (d :w)) (* @alpha @K)))))))))

(defn ^Double calc-likelihood-term [corpora]
  (let [V (corpora :V)]
    (+ (- (* @K (org.apache.commons.math.special.Gamma/logGamma (* @beta V)))
	  (* (* @K V) (org.apache.commons.math.special.Gamma/logGamma @beta)))
     (reduce + (for [z (range @K)]
		 (- (reduce + (for [v (range V)]
				(org.apache.commons.math.special.Gamma/logGamma (+ (((corpora :Nzw) z) v) @beta))))
		    (org.apache.commons.math.special.Gamma/logGamma (+ ((corpora :Nz) z) (* @beta V)))))))))

(defn ^Double log-likelihood [corpora]
  (+ (calc-prior-term (corpora :documents))
     (calc-likelihood-term corpora)))

