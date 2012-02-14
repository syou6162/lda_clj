(ns lda_clj.test.util
  (:use [lda_clj.util])
  (:use [clojure.test]))

(deftest test-sample0
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

(deftest test-sample1
  (let [tmp [0.2 0.5 0.3]
	N 100000
	samples (for [i (range N)] (sample tmp))
	table (frequencies samples)]
    (map #(is (and (<= 0 %) (< % 3))) samples)
    ;; Chi_square_test with K – 1 degrees of freedom
    ;; http://kusuri-jouhou.com/statistics/bunpuhyou.html
    (is (< (reduce + (for [key (keys table)]
		       (let [Ok (table key)
			     Ek (/ (* N (tmp key)) 1.0)]
			 (/ (* (- Ok Ek) (- Ok Ek)) Ek))))
	   9.488))))

(deftest test-get-word-id-init
  (is (= {"aaa" 0} (get-word-id {} "aaa"))))

(deftest test-get-word-id-seq0
  (is (= {"aaa" 0 "bbb" 1}
	 (get-word-id (get-word-id {} "aaa") "bbb"))))

(deftest test-get-word-id-seq1
  (is (= {"aaa" 0}
	 (get-word-id (get-word-id {} "aaa") "aaa"))))

(deftest test-get-words-ids
  (is (= {"c" 2, "b" 1, "a" 0}
	 (get-words-ids {} ["a" "b" "c" "a"]))))
