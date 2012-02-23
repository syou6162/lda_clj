(ns lda_clj.hyperparameter-optimization
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.random])
  (:use [lda_clj.sampler]))

(use '[clojure.contrib.import-static :only (import-static)])
(import-static org.apache.commons.math.special.Gamma digamma)

(defn get-new-alpha [corp alpha]
  (let [documents (corp :documents)
	D (count documents)
	K (corp :K)]
    (* alpha
       (/ (- (reduce + (for [doc documents, z (range K)]
			 (digamma (+ (get-in doc [:Nz z]) alpha))))
	     (* D K (digamma alpha)))
	  (- (* K
		(reduce + (for [doc documents]
			    (digamma (+ (count (doc :w)) (* alpha K))))))
	     (* D K (digamma (* alpha K))))))))

(defn get-new-beta [corp beta]
  (let [documents (corp :documents)
	V (corp :V)
	K (corp :K)]
    (* beta
       (/ (- (reduce + (for [z (range K), v (range V)]
			 (digamma (+ (get-in corp [:Nzw z v]) beta))))
	     (* V K (digamma beta)))
	  (- (* V
		(reduce + (for [z (range K)]
			    (digamma (+ (get-in corp [:Nz z]) (* beta V))))))
	     (* V K (digamma (* beta V))))))))
