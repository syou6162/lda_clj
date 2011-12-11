(ns lda_clj.test.document
  (:use [lda_clj.document])
  (:use [lda_clj.util])
  (:use [clojure.test]))

(deftest test-create-document
  (is (= (document-map deref (create-document '[1 2 3 4 2]))
	 (struct document '[1 2 3 4 2] '[nil nil nil nil nil] '[0 0 0]))))

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
