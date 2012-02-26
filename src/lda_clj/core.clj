(ns lda_clj.core
  (:use [lda_clj.lda])
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:gen-class))

(defn -main [& args]
  (with-command-line args "ex: lein run --max-iter 1000 --file wsj.txt --topic 10 --model-file result.model --alpha 0.1 --beta 0.1"
    [[file "File name of training"]
     [topic "Number of topic dimension"]
     [alpha "Hyperparameter for topic prior"]
     [beta "Hyperparameter for word prior"]
     [max-iter "Number of maximum iterations"]
     [model-file "File name of trained model file"]
     rest]
    (let [raw-docs (read-raw-docs file)
	  id2word (vec (set (flatten raw-docs)))
	  word2id (get-words-ids {} id2word)
	  docs (for [doc raw-docs] (map (fn [w] (get-in word2id [w])) doc))]
      (run {:file file
	    :K (Integer/parseInt topic)
	    :alpha (Double/parseDouble alpha)
	    :beta (Double/parseDouble beta)
	    :max-iter (Integer/parseInt max-iter)
	    :model-file model-file
	    :id2word id2word
	    :word2id word2id
	    :docs docs})))
  nil)
