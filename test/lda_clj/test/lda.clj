(ns lda_clj.test.lda
  (:use [lda_clj.lda])
  (:use [clojure.test]))

(deftest test-get-words-ids
  (is (= {"c" 2, "b" 1, "a" 0}
	 (get-words-ids {} ["a" "b" "c" "a"]))))
