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

; (set! *warn-on-reflection* true)

(def filename (atom "wsj.txt"))
(def word2id-map (atom {}))
(def max-iter (atom 100))

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

(defn ^Integer my-sample [^doubles xs]
  (let [r (* (reduce + xs) (rand))]
    (loop [idx 0, cum 0.0]
      (let [val (+ cum (xs idx))]
	(if (> val r)
	  idx
	  (recur (inc idx) val))))))

(defmacro set-new-stat [z Ndz Nz Nwz f]
  `(do
     (reset! ~'z-atom ~z)
     (reset! ~'Ndz-atom (~f ~Ndz))
     (reset! ~'Nz-atom (~f ~Nz))
     (reset! ~'Nwz-atom (~f ~Nwz))))

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
	  num-of-docs (count (corpora :documents))
	  Nz (corpora :Nz)
	  Nwz (corpora :Nwz)
	  V (corpora :V)]
      (dotimes [iter @max-iter]
	 (dorun
	 (for [doc-idx (range num-of-docs)]
	   (let [current-doc ((corpora :documents) doc-idx)
		 w (current-doc :w)
		 z (current-doc :z)
		 Ndz (current-doc :Nz)
		 Nd (count (current-doc :w))]
	     (dorun
	      (for [^Integer word-idx (range Nd)]
		(let [word-id (w word-idx)
		      Nwz (Nwz word-id)]
		  (do
		    (if (not (= iter 0))
		      (let [prev-z @(z word-idx)
			    z-atom (z word-idx)
			    Ndz-atom (Ndz prev-z)
			    Nz-atom (Nz prev-z)
			    Nwz-atom (Nwz prev-z)]
			(set-new-stat prev-z @Ndz-atom @Nz-atom @Nwz-atom dec)))
		    (let [
			  z-atom (z word-idx)
			  v (vec (map #(* (my-gen-prior-prob @(Ndz %) @alpha)
					  (my-gen-likelihood-prob
					   @(Nz %)
					   @(Nwz %)
					   V @beta))
				      (range @K)))
			  
			  next-z (sample-with-java (double-array v))
			  Ndz-atom (Ndz next-z)
			  Nz-atom (Nz next-z)
			  Nwz-atom (Nwz next-z)]
		      (set-new-stat next-z @Ndz-atom @Nz-atom @Nwz-atom inc)))))))))
	 (println (str iter ", " (log-likelihood (corpora-map deref corpora))))
	 ))))