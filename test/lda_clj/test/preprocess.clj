(ns lda_clj.test.preprocess
  (:use [lda_clj.preprocess])
  (:use [clojure.test]))

(deftest test-get-bag-of-words
  (is (= (get-bag-of-words "The story of Alice in Wonderland.

Hi!

The story of Alice in Wonderland.")
	 (list "story" "alice" "wonderland" "story" "alice" "wonderland"))))
