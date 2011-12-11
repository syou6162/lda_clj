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
(def max-iter (atom 5))

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

(defn doall-recur [s]
  (if (seq? s)
    (doall (map doall-recur
                s))
    s))

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
	  corpora (gen-corpora raw-documents)
	  num-of-docs (count (corpora :documents))]
      (dotimes [iter @max-iter]
	(dorun
	 (for [doc-idx (range num-of-docs)]
	   (let [Nd (count (((corpora :documents) doc-idx) :w))]
	     (dorun
	      (for [^Integer word-idx (range Nd)]
		(do
		  (if (not (= iter 0))
		    (let [current-doc ((corpora :documents) doc-idx)
			  word-id ((current-doc :w) word-idx)
			  current-topic-id ((deref (current-doc :z)) word-idx)
			  Ndz ((deref (current-doc :Nz)) current-topic-id)
			  Nz ((deref (corpora :Nz)) current-topic-id)
			  Nzw ((deref ((corpora :Nzw) current-topic-id)) word-id)]
		      (do
			(swap! (current-doc :z) assoc word-idx nil)
			(swap! (current-doc :Nz) assoc current-topic-id (dec Ndz))
			(swap! (corpora :Nz) assoc current-topic-id (dec Nz))
			(swap! ((corpora :Nzw) current-topic-id) assoc word-id (dec Nzw)))))
		  (let [current-doc ((corpora :documents) doc-idx)
			word-id ((current-doc :w) word-idx)
			next-z 
			(sample (vec (map #(* (gen-prior-prob current-doc % @K @alpha)
					      (gen-likelihood-prob corpora word-id % @beta))
					  (range @K))))
			word-id ((current-doc :w) word-idx)
			Ndz ((deref (current-doc :Nz)) next-z)
			Nz ((deref (corpora :Nz)) next-z)
			Nzw ((deref ((corpora :Nzw) next-z)) word-id)]
		    (do
		      (swap! (current-doc :z) assoc word-idx next-z)
		      (swap! (current-doc :Nz) assoc next-z (inc Ndz))
		      (swap! (corpora :Nz) assoc next-z (inc Nz))
		      (swap! ((corpora :Nzw) next-z) assoc word-id (inc Nzw))))))))))
	(println (str iter ", " (log-likelihood (corpora-map deref corpora))))))))