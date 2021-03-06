(ns lda_clj.test.util
  (:use [lda_clj.util])
  (:use [clojure.test]))

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
