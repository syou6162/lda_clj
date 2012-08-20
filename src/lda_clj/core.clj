(ns lda_clj.core
  (:use [cheshire.core])
  (:require [clojure.tools.cli :as cli])
  (:use [lda_clj.corpora])
  (:use [lda_clj.document])
  (:use [lda_clj.sampler])
  (:use [lda_clj.log_likelihood])
  (:use [lda_clj.inference])
  (:use [lda_clj.util])
  (:use [lda_clj.preprocess])
  (:gen-class))

(import '(java.io FileWriter))
(use '[clojure.string :only (split)])

(defn get-topic-words [corp id2word topic-id n]
  (let [Nzw (get-in corp [:Nzw topic-id])]
    (->> Nzw
	 (map-indexed (fn [idx cnt] [(id2word idx) cnt]))
	 (sort-by second >)
	 (take n)
	 (map first))))

(defn- get-cli-opts [args]
  (cli/cli args
	   ["--file" "File name of training" :default "wsj.txt"]
	   ["--topic" "Number of topic dimension" :default 30 :parse-fn #(Integer. %)]
	   ["--alpha" "Hyperparameter for topic prior" :default 0.1 :parse-fn #(Double. %)]
	   ["--beta" "Hyperparameter for word prior" :default 0.01 :parse-fn #(Double. %)]
	   ["--max-iter" "Number of maximum iterations" :default 10 :parse-fn #(Integer. %)]
	   ["--model-file" "File name of trained model file" :default "model.txt"]))

(defn -main [& args]
  (let [[opts args banner] (get-cli-opts args)
	raw-docs (read-raw-docs (opts :file))
	id2word (vec (set (flatten raw-docs)))
	word2id (get-words-ids {} id2word)
	docs (for [doc raw-docs] (map (fn [w] (get-in word2id [w])) doc))]
    (loop [corp (create-corpora docs (count word2id) (opts :topic))
	   iter 0]
      (if (= (opts :max-iter) iter)
	(generate-stream {:corp corp
			  :alpha (opts :alpha)
			  :beta (opts :beta)
			  :id2word id2word}
			 (clojure.java.io/writer (opts :model-file)))
	(do
	  (if (not (= 0 iter))
	    (do
	      (dotimes [topic-id (opts :topic)]
		(println topic-id (get-topic-words corp id2word topic-id 10)))
	      (println (str "Iter:" iter ", "
			    (calc-prior-term corp (opts :alpha)) ", "
			    (calc-likelihood-term corp (opts :beta)) ", "
			    (log-likelihood corp (opts :alpha) (opts :beta))))))
	  (recur (inference corp (opts :alpha) (opts :beta) (= 0 iter)) (inc iter))))))
  nil)