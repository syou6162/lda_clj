(ns lda_clj.test.document
  (:use [lda_clj.document])
  (:use [clojure.test]))

(deftest test-create-document
  (is (= {:w '[1 2 3 4 2], :z '(nil nil nil nil nil), :Nz '(nil nil nil)}
	 (create-document '[1 2 3 4 2]))))
