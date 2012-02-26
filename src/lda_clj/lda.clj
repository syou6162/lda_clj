(ns lda_clj.lda
  (:use [cheshire.core])
  (:use [lda_clj.corpora])
  (:use [lda_clj.document])
  (:use [lda_clj.sampler])
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.inference])
  (:use [lda_clj.util])
  (:use [lda_clj.preprocess])
  (:use [lda_clj.hyperparameter-optimization])
  (:gen-class))

(import '(java.io FileWriter))

(use '[clojure.contrib.duck-streams :only (reader read-lines)])
(use '[clojure.contrib.string :only (split)])

(defn get-words-ids [word2id words]
  (reduce (fn [m w] (get-word-id m w)) word2id words))

(defn read-raw-docs [filename]
  (for [line (read-lines filename)] (get-bag-of-words line)))

(defn get-topic-words [corp id2word topic-id n]
  (let [Nzw (get-in corp [:Nzw topic-id])]
    (->> Nzw
	 (map-indexed (fn [idx cnt] [(id2word idx) cnt]))
	 (sort-by second >)
	 (take n)
	 (map first)
	 (vec))))

(defn write-model-file [corp alpha beta id2word model-file]
  (generate-stream {:corp corp
		    :alpha alpha
		    :beta beta
		    :id2word id2word}
		   (clojure.java.io/writer model-file)))

(defn print-stat [corp alpha beta iter id2word]
  (do
    (dotimes [topic-id (corp :K)]
      (binding [*out* *err*]
	(println (str "Iteration: " iter
		      ", Topic id: " topic-id
		      ", " (get-topic-words corp id2word topic-id 10)))))
    (println (str "Iter:" iter ", "
		  (calc-prior-term corp alpha) ", "
		  (calc-likelihood-term corp beta) ", "
		  (log-likelihood corp alpha beta) ", "
		  alpha ", " beta))))

(defn run [opts]
  (loop [iter 0
	 corp (create-corpora (opts :docs) (count (opts :word2id)) (opts :K))
	 alpha (opts :alpha)
	 beta (opts :beta)]
    (if (= (opts :max-iter) iter)
      (write-model-file corp alpha beta (opts :id2word) (opts :model-file))
      (do
	(if (not (= 0 iter))
	  (print-stat corp alpha beta iter (opts :id2word)))
	(recur (inc iter)
	       (inference corp alpha beta (= 0 iter))
	       (if (= iter 0) alpha (get-new-alpha corp alpha))
	       (if (= iter 0) beta (get-new-beta corp beta)))))))
