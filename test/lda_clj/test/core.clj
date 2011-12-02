(ns lda_clj.test.core
  (:use [lda_clj.core])
  (:use [clojure.test]))

(deftest test-get-word-id-init
  (is (= [{"aaa" 0} 0] (get-word-id [{} "aaa"]))))

(deftest test-get-word-id-seq0
  (is (= [{"aaa" 0 "bbb" 1} 1]
	   (get-word-id [(first (get-word-id [{} "aaa"])) "bbb"]))))

(deftest test-get-word-id-seq1
  (is (= [{"aaa" 0 } 0]
	   (get-word-id [(first (get-word-id [{} "aaa"])) "aaa"]))))

(deftest test-gen-word2id-map
  (is (= {"a" 0, "b" 1, "c" 2}
	 (reduce #(first (get-word-id [%1 %2])) {} '("a" "b" "c" "a")))))

(deftest test-get-words-ids
  (is (= [0 1 2 0]
	   (get-words-ids {} ["a" "b" "c" "a"]))))
