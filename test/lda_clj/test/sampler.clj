(ns lda_clj.test.sampler
  (:use [lda_clj.corpora])
  (:use [lda_clj.document])
  (:use [lda_clj.sampler])
  (:use [clojure.test]))

(deftest test-dec-topic-in-document
  (let [d-before (struct document
		  [1 1 1 1 1]
		  [0 0 0 1 1]
		  [3 2 0])
	d-after (struct document
		  [1 1 1 1 1]
		  [0 nil 0 1 1]
		  [2 2 0])
	]
    (is (= d-after (dec-topic-in-document d-before 1)))))

(deftest test-inc-topic-in-document
  (let [d-before (struct document
		  [1 1 1 1 1]
		  [0 nil 0 1 1]
		  [2 2 0])
	d-after (struct document
		  [1 1 1 1 1]
		  [0 0 0 1 1]
		  [3 2 0])
	]
    (is (= d-after (inc-topic-in-document d-before 1 0)))))

(deftest test-dec-topic-in-corpora
  (let [d0 (struct document
		   [0 1 2]
		   [0 0 0]
		   [3 0 0])
	d1-before (struct document
			  [1 2 0]
			  [0 1 2]
			  [1 1 1])
	d1-after (struct document
			 [1 2 0]
			 [0 nil 2]
			 [1 0 1])
	d2 (struct document
		   [2 0 1]
		   [2 2 2]
		   [0 0 3])
	corpora-before (struct corpora
			       [d0 d1-before d2]
			       [4 1 4]
			       [[1 2 1] [0 0 1] [2 1 1]]
			       3 3)
	corpora-after (struct corpora
			      [d0 d1-after d2]
			      [4 0 4]
			      [[1 2 1] [0 0 0] [2 1 1]]
			      3 3)]
    (is (= corpora-after (dec-topic-in-corpora corpora-before 1 1)))))

(deftest test-inc-topic-in-corpora
  (let [d0 (struct document
		   [0 1 2]
		   [0 0 0]
		   [3 0 0])
	d1-before (struct document
			  [1 2 0]
			  [0 nil 2]
			  [1 0 1])
	d1-after (struct document
			 [1 2 0]
			 [0 1 2]
			 [1 1 1])
	d2 (struct document
		   [2 0 1]
		   [2 2 2]
		   [0 0 3])
	corpora-before (struct corpora
			       [d0 d1-before d2]
			       [4 0 4]
			       [[1 2 1] [0 0 0] [2 1 1]]
			       3)
	corpora-after (struct corpora
			      [d0 d1-after d2]
			      [4 1 4]
			      [[1 2 1] [0 0 1] [2 1 1]]
			      3)]
    (is (= corpora-after (inc-topic-in-corpora corpora-before 1 1 1)))))
