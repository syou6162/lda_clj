(ns lda_clj.test.corpora
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.corpora])
  (:use [lda_clj.util])
  (:use [lda_clj.sampler])
  (:use [clojure.test]))

(reset! K 3)

(deftest test-create-corporao
	 (is (= {:documents [{:w '[0 1 2]
			      :z '[nil nil nil]
			      :Nz '[0 0 0]}
			     {:w '[1 1 4]
			      :z '[nil nil nil]
			      :Nz '[0 0 0]}]
		 :Nz '[0 0 0]
		 :Nwz '[[0 0 0]
			[0 0 0]
			[0 0 0]
			[0 0 0]
			[0 0 0]]
		 :V 5}
		(corpora-map deref (create-corpora [[0 1 2]
						    [1 1 4]]
						   5)))))