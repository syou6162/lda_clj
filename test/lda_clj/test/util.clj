(ns lda_clj.test.util
  (:use [lda_clj.util])
  (:use [clojure.test]))

(deftest test-sample
  (let [tmp [2.5 3.0 1.0 2.0 1.5]
	N 100000
	samples (for [i (range N)] (sample tmp))
	table (frequencies samples)]
    (map #(is (and (<= 0 %) (< % 5))) samples)
    ;; Chi_square_test with K – 1 degrees of freedom
    ;; http://kusuri-jouhou.com/statistics/bunpuhyou.html
    (is (< (reduce + (for [key (keys table)]
		       (let [Ok (table key)
			     Ek (/ (* N (tmp key)) 10.0)]
			 (/ (* (- Ok Ek) (- Ok Ek)) Ek))))
	   9.488))))
