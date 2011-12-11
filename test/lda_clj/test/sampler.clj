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
			       3)
	corpora-after (struct corpora
			      [d0 d1-after d2]
			      [4 0 4]
			      [[1 2 1] [0 0 0] [2 1 1]]
			      3)]
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

(deftest test-gen-prior-prob
  (let [topic-dimension 3
	alpha 0.1
	test-document (struct document
			      [1 1 1 1 1]
			      (atom [0 0 0 1 1])
			      (atom [3 2 0]))]
  (is (> 0.001 (Math/abs (- (+ 3 0.1) ;; 3 + 0.1
			    (gen-prior-prob test-document 0 topic-dimension alpha)))))
  (is (> 0.001 (Math/abs (- (+ 2 0.1) ;; 2 + 0.1
			    (gen-prior-prob test-document 1 topic-dimension alpha)))))
  (is (> 0.001 (Math/abs (- (+ 0 0.1) ;; 0 + 0.1
			    (gen-prior-prob test-document 2 topic-dimension alpha)))))
  ;; sum to 1
  (is (> 0.001 (Math/abs (- (+ (count (test-document :w)) (* alpha topic-dimension))
			    (reduce + (for [topic-id (range 3)]
					(gen-prior-prob test-document topic-id topic-dimension alpha)))))))))

(deftest test-gen-likelihood-prob
  (let [d0 (struct document
		   [0 1 2]
		   (atom [0 0 0])
		   (atom [3 0 0]))
	d1 (struct document
		   [1 2 0]
		   (atom [0 1 2])
		   (atom [1 1 1]))
	d2 (struct document
		   [2 0 1]
		   (atom [2 2 2])
		   (atom [0 0 3]))
	corpora	(struct corpora
			[d0 d1 d2]
			(atom [4 1 4])
			[(atom [1 2 1]) (atom [0 0 1]) (atom [2 1 1])]
			3)
	beta 0.1]
    ;; gen-likelihood-prob [corpora word-id topic-id beta]
    ;; topic-id = 0
    (is (> 0.001 (Math/abs (- (/ (+ 1 0.1) (+ 4 (* 3 0.1))) ;; (1 + 0.1) / (4 + 3 * 0.1)
			      (gen-likelihood-prob corpora 0 0 beta)))))
    (is (> 0.001 (Math/abs (- (/ (+ 2 0.1) (+ 4 (* 3 0.1))) ;; (2 + 0.1) / (4 + 3 * 0.1)
			      (gen-likelihood-prob corpora 1 0 beta)))))
    (is (> 0.001 (Math/abs (- (/ (+ 1 0.1) (+ 4 (* 3 0.1))) ;; (1 + 0.1) / (4 + 3 * 0.1)
			      (gen-likelihood-prob corpora 2 0 beta)))))
    
    ;; topic-id = 1
    (is (> 0.001 (Math/abs (- (/ (+ 0 0.1) (+ 1 (* 3 0.1))) ;; (0 + 0.1) / (1 + 3 * 0.1)
			      (gen-likelihood-prob corpora 0 1 beta)))))
    (is (> 0.001 (Math/abs (- (/ (+ 0 0.1) (+ 1 (* 3 0.1))) ;; (0 + 0.1) / (1 + 3 * 0.1)
			      (gen-likelihood-prob corpora 1 1 beta)))))
    (is (> 0.001 (Math/abs (- (/ (+ 1 0.1) (+ 1 (* 3 0.1))) ;; (1 + 0.1) / (1 + 3 * 0.1)
			      (gen-likelihood-prob corpora 2 1 beta)))))
    
    ;; topic-id = 2
    (is (> 0.001 (Math/abs (- (/ (+ 2 0.1) (+ 4 (* 3 0.1))) ;; (2 + 0.1) / (4 + 3 * 0.1)
			      (gen-likelihood-prob corpora 0 2 beta)))))
    (is (> 0.001 (Math/abs (- (/ (+ 1 0.1) (+ 4 (* 3 0.1))) ;; (1 + 0.1) / (4 + 3 * 0.1)
			      (gen-likelihood-prob corpora 1 2 beta)))))
    (is (> 0.001 (Math/abs (- (/ (+ 1 0.1) (+ 4 (* 3 0.1))) ;; (1 + 0.1) / (4 + 3 * 0.1)
			      (gen-likelihood-prob corpora 2 2 beta)))))

    ;; sum to 1
    (for [topic-id (range 3)]
      (is (> 0.001 (Math/abs (- 1.0 (reduce + (for [word-id (range 3)]
						(gen-likelihood-prob corpora word-id topic-id beta))))))))))