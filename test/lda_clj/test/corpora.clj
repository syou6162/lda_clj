(ns lda_clj.test.corpora
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler])
  (:use [clojure.test]))

(deftest test-create-corporao
	 (is (= {:documents [{:w '[0 1 2]
			      :z '[nil nil nil]
			      :Nz '[0 0 0]}
			     {:w '[1 1 4]
			      :z '[nil nil nil]
			      :Nz '[0 0 0]}]
		 :Nz '[0 0 0]
		 :Nzw '[[0 0 0 0 0]
			[0 0 0 0 0]
			[0 0 0 0 0]]
		 :V 5
		 :K 3}
		(create-corpora [[0 1 2]
				 [1 1 4]]
				5
				3))))
