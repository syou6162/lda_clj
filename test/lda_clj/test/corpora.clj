(ns lda_clj.test.corpora
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.corpora])
  (:use [clojure.test]))

(deftest test-create-corpora
	 (is (= {:documents [{:w '[0 1 2]
			      :z '(nil nil nil)
			      :Nz '(nil nil nil)}
			     {:w '[1 1 4]
			      :z '(nil nil nil)
			      :Nz '(nil nil nil)}]
		   :Nz '(nil nil nil)
		   :Nzw '((nil nil nil nil nil)
			  (nil nil nil nil nil)
			  (nil nil nil nil nil))
		   :V 5}
		(create-corpora [[0 1 2]
				 [1 1 4]]
				5))))
