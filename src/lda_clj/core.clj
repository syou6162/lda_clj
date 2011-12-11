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
(use '[incanter.stats :only (sample-multinomial)])

(def filename (atom "wsj.txt"))
(def word2id-map (atom {}))
(def max-iter (atom 1000))

(defn gen-raw-documents [filename]
  (doall (map (fn [line]
		(doall (map (fn [word]
			      (let [[new-map word-id] (get-word-id [@word2id-map word])]
				(do
				  (reset! word2id-map new-map)
				  word-id)))
			    (split #"\s" line))))
	      (with-open [r (reader filename)]
		(doall (line-seq r))))))

(defn normalize [xs]
  (let [sum (reduce + xs)]
    (map #(/ % sum) xs)))

(defn gen-corpora [raw-documents]
  (create-corpora ;; create-corpora-with-random-topic-assignments
    raw-documents (count @word2id-map)))

(defn -main [& args]
  (do
    (with-command-line args "comment"
      [[file "File name of training"]
       [topic "Number of topic dimension"]
       [a "Hyperparameter for topic prior"]
       [b "Hyperparameter for word prior"]
       rest]
      (do
	(if (not (nil? file))
	  (reset! filename file))
	(if (not (nil? topic))
	  (reset! K (Integer/parseInt topic)))
	(if (not (nil? a))
	  (reset! alpha (Double/parseDouble a)))
	(if (not (nil? b))
	  (reset! beta (Double/parseDouble b)))
	(println "# Alpha:" @alpha)
	(println "# Beta:" @beta)
	(println "# K:" @K)))
    (let [raw-documents (gen-raw-documents @filename)
	  corpora (atom (gen-corpora raw-documents))
	  num-of-docs (count (@corpora :documents))]
      (dotimes [iter @max-iter]
	(dorun
	 (for [^Integer doc-idx (range num-of-docs)]
	   (let [Nd (count (((@corpora :documents) doc-idx) :w))]
	     (dorun
	      (for [^Integer word-idx (range Nd)]
		(do
		  (let [tmp-corpora (if (= iter 0) @corpora ;; 初回はデクリメントしない
					(reset! corpora (dec-topic-in-corpora @corpora doc-idx word-idx)))
			current-doc ((tmp-corpora :documents) doc-idx)
			^Integer word-id ((((tmp-corpora :documents) doc-idx) :w) word-idx)
			^Integer next-z (sample (vec (map #(* (gen-prior-prob current-doc % @K @alpha)
							   (gen-likelihood-prob tmp-corpora word-id % @beta))
							  (range @K))))]
		    (reset! corpora (inc-topic-in-corpora tmp-corpora doc-idx word-idx next-z)))))))))
	(println (str iter ", "
		      (calc-prior-term (@corpora :documents)) ", "
		      (calc-likelihood-term @corpora) ", "
		      (log-likelihood @corpora)))))))
