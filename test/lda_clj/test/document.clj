(ns lda_clj.test.document
  (:use [lda_clj.document])
  (:use [clojure.test]))

(deftest test-create-document
  (is (= {:w '[1 2 3 4 2], :z '(nil nil nil nil nil), :Nz '(nil nil nil)}
	 (create-document '[1 2 3 4 2]))))

(deftest test-valid-document?
  (let [valid-document (struct document
			       [1 1 1 1 1]
			       [0 0 0 1 1]
			       [3 2 0])
	invalid-document0 (struct document
				  [1 1 1 1 1]
				  [0 0 0 1] ;; length is invalid!
				  [3 2 0])
	invalid-document1 (struct document
				  [1 1 1 1 1]
				  [0 0 1 1 1]
				  [3 2 0])] ;; sum is invalid!
    (is (valid-document? valid-document))
    (is (not (valid-document? invalid-document0)))
    (is (not (valid-document? invalid-document1)))))