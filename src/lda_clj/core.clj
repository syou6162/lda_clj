(ns lda_clj.core
  (:use [lda_clj.corpora])
  (:use [lda_clj.document])
  (:use [lda_clj.sampler])
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.util])
  (:gen-class))

(use '[clojure.contrib.command-line :only (with-command-line)])
(use '[clojure.contrib.duck-streams :only (reader)])
(use '[clojure.contrib.string :only (split)])

(defn -main [& args]
  (with-command-line args "comment"
    [[file "File name of training"]
     [topic "Number of topic dimension"]
     [alpha "Hyperparameter for topic prior" 0.1]
     [beta "Hyperparameter for word prior" 0.1]
     [max-iter "Number of maximum iterations" 10]
     rest]
    (do
      (if (not (nil? topic))
	(reset! K (Integer/parseInt topic))))
    (let [raw-docs (read-raw-docs file)
	  word2id (get-words-ids {} (flatten raw-docs))
	  docs (for [doc raw-docs] (map (fn [w] (get-in word2id [w])) doc))]
      (loop [corp (create-corpora docs (count word2id)) ; (create-corpora-with-random-topic-assignments docs (count word2id))
	     iter 0]
	(if (= (Integer/parseInt max-iter) iter)
	  corp
	  (do
	    (if (not (= 0 iter))
	      (println (str "Iter:" iter ", "
			    (calc-prior-term corp alpha) ", "
			    (calc-likelihood-term corp beta) ", "
			    (log-likelihood corp alpha beta))))
	    (recur (inference corp alpha beta (= 0 iter)) (inc iter)))))))
  nil)
