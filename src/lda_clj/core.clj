(ns lda_clj.core
  (:use [cheshire.core])
  (:use [lda_clj.corpora])
  (:use [lda_clj.document])
  (:use [lda_clj.sampler])
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.inference])
  (:use [lda_clj.util])
  (:use [lda_clj.preprocess])
  (:gen-class))

(import '(java.io FileWriter))
(use '[clojure.contrib.command-line :only (with-command-line)])
(use '[clojure.contrib.duck-streams :only (reader read-lines)])
(use '[clojure.contrib.string :only (split)])

(defn get-topic-words [corp id2word topic-id n]
  (let [Nzw (get-in corp [:Nzw topic-id])]
    (->> Nzw
	 (map-indexed (fn [idx cnt] [(id2word idx) cnt]))
	 (sort-by second >)
	 (take n)
	 (map first))))

(defn -main [& args]
  (with-command-line args "comment"
    [[file "File name of training"]
     [topic "Number of topic dimension"]
     [alpha "Hyperparameter for topic prior" 0.1]
     [beta "Hyperparameter for word prior" 0.01]
     [max-iter "Number of maximum iterations" 10]
     [model-file "File name of trained model file"]
     rest]
    (let [raw-docs (read-raw-docs file)
	  id2word (vec (set (flatten raw-docs)))
	  word2id (get-words-ids {} id2word)
	  docs (for [doc raw-docs] (map (fn [w] (get-in word2id [w])) doc))
	  K (Integer/parseInt topic)]
      (loop [corp (create-corpora docs (count word2id) K)
	     iter 0]
	(if (= (Integer/parseInt max-iter) iter)
	  (generate-stream {:corp corp
			    :alpha alpha
			    :beta beta
			    :id2word id2word}
			   (clojure.java.io/writer model-file))
	  (do
	    (if (not (= 0 iter))
	      (do
		(dotimes [topic-id K]
		  (println topic-id (get-topic-words corp id2word topic-id 10)))
		(println (str "Iter:" iter ", "
			    (calc-prior-term corp alpha) ", "
			    (calc-likelihood-term corp beta) ", "
			    (log-likelihood corp alpha beta)))))
	    (recur (inference corp alpha beta (= 0 iter)) (inc iter)))))))
  nil)