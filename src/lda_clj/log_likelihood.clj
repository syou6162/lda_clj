(ns lda_clj.log_likelihood
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler])
  (:use [lda_clj.document]))

(defn logGamma [x]
  (org.apache.commons.math.special.Gamma/logGamma x))

(defmacro sum [bindings expr]
  `(reduce + (for ~bindings ~expr)))

(defn ^Double calc-prior-term [corp alpha]
  (let [documents (corp :documents)
	N (count documents)
	K (corp :K)]
    (+ (- (* N (logGamma (* alpha K)))
	  (* (* N K) (logGamma alpha)))
       (sum [d documents]
	    (- (sum [z (range K)]
		    (logGamma (+ ((d :Nz) z) alpha)))
	       (logGamma (+ (count (d :w)) (* alpha K))))))))

(defn ^Double calc-likelihood-term [corp beta]
  (let [V (corp :V)
	K (corp :K)]
    (+ (- (* K (logGamma (* beta V)))
	  (* (* K V) (logGamma beta)))
       (sum [z (range K)]
	    (- (sum [v (range V)]
		    (logGamma (+ (get-in corp [:Nzw z v]) beta)))
	       (logGamma (+ (get-in corp [:Nz z]) (* beta V))))))))

(defn ^Double log-likelihood [corp alpha beta]
  (+ (calc-prior-term corp alpha)
     (calc-likelihood-term corp beta)))
