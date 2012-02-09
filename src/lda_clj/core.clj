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

(def word2id-map (atom {}))

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
       [max-iter "Number of maximum iterations"]
       rest]
      (do
	(if (not (nil? topic))
	  (reset! K (Integer/parseInt topic)))
	(if (not (nil? a))
	  (reset! alpha (Double/parseDouble a)))
	(if (not (nil? b))
	  (reset! beta (Double/parseDouble b)))
	(println "# Alpha:" @alpha)
	(println "# Beta:" @beta)
	(println "# K:" @K))
    (loop [corp (gen-corpora (gen-raw-documents file))
	   iter 0]
      (if (= (Integer/parseInt max-iter) iter)
	corp
	(do
	  (if (not (= 0 iter))
	    (println (str "Iter:" iter ", " (log-likelihood corp))))
	  (recur (inference corp (= 0 iter)) (inc iter))))))))
