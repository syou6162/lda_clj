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
; (use '[incanter.stats :only (sample-multinomial)])

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

(defn doall-recur [s]
  (if (seq? s)
    (doall (map doall-recur
                s))
    s))

;; (defn make-cum-sum [xs]
;;   (reduce #(let [[v cum] %1
;; 		 val %2
;; 		 new-val (+ val cum)]
;; 	     [(conj v new-val) new-val])
;; 	  [[] 0.0] xs))

;; (defn my-sample [xs]
;;   (let [[v cum] (make-cum-sum xs)
;; 	r (* (rand) cum)]
;;     (first (first (drop-while #(let [[idx val] %]
;; 				 (< val r))
;; 			      (map-indexed vector (vec v)))))))

(defn my-sample [xs]
  (let [r (* (reduce + xs) (rand))]
    (loop [idx 0, cum 0.0]
      (let [val (+ cum (xs idx))]
	(if (> val r)
	  idx
	  (recur (inc idx) val))))))

;; (defn ^Integer my-sample [^doubles xs]
;;   (let [^Double r (* (reduce + xs) (rand))]
;;     (loop [^Integer idx 0
;; 	   ^Double cum 0.0]
;;       (let [^Double val (+ cum (xs idx))]
;; 	(if (> val r)
;; 	idx
;; 	(recur (inc idx) val))))))

; (frequencies (for [_ (range 10000)] (my-sample '[0.1 0.3 0.6])))
; (time (dotimes [_ 500000] (my-sample '[0.1 0.3 0.6])))

; (time (dotimes [_ 500000] (my-sample '(0.1 0.3 0.6))))

;; (defn make-cum-sum [^doubles xs]
;;   (areduce xs idx ret (object-array '[(double-array (count xs)) 0.0])
;; 	   #(let [v (aget 0 %1)
;; 		  cum (aget 1 %1)
;; 		  val %2
;; 		  new-val (+ val cum)]
;; 	      (do 
	       
;; 	     [(conj v new-val) new-val]))
;; 	   ))



;; (defn ^Integer my-sample [^doubles xs]
;;   (let [cum (atom 0.0)]
;;     (letfn [(^doubles make-cum-sum [^doubles xs]
;; 		      (amap ^doubles xs idx ^doubles _ (reset! cum (+ @cum (aget ^doubles xs idx)))))]
;;       (let [v (vec (make-cum-sum xs))
;; 	    r (* (rand) @cum)]
;; 	(first (first (drop-while #(let [[idx val] %]
;; 				     (< val r))
;; 				  (map-indexed vector v))))))))

; (for [_ (range 30)] (my-sample (double-array '(0.1 0.3 0.6))))

(defmacro set-new-stat [word-idx word-id z Ndz Nz Nzw f]
  `(do
     (swap! ~'z-atom assoc ~word-idx ~z)
     (swap! ~'Ndz-atom assoc ~z (~f ~Ndz))
     (swap! ~'Nz-atom assoc ~z (~f ~Nz))
     (swap! (~'Nzw-atom ~z) assoc ~word-id (~f ~Nzw))))

; (println (macroexpand `(set-new-stat 1 2 1 1 9 2 inc)))

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
	  Nz-atom (corpora :Nz)
	  Nzw-atom (corpora :Nzw)]
      (dotimes [iter @max-iter]
	(dorun
	 (for [doc-idx (range num-of-docs)]
	   (let [current-doc ((corpora :documents) doc-idx)
		 w-atom (current-doc :w)
		 z-atom (current-doc :z)
		 Ndz-atom (current-doc :Nz)
		 Nd (count (current-doc :w))]
	     (dorun
	      (for [^Integer word-idx (range Nd)]
		(let [word-id (w-atom word-idx)]
		  (do
		    (if (not (= iter 0))
		      (let [current-topic-id ((deref z-atom) word-idx)
			    Ndz ((deref Ndz-atom) current-topic-id)
			    Nz ((deref Nz-atom) current-topic-id)
			    Nzw ((deref (Nzw-atom current-topic-id)) word-id)]
			(set-new-stat word-idx word-id current-topic-id Ndz Nz Nzw dec)))
		    (let [
			  next-z (my-sample (vec (map #(* (my-gen-prior-prob ((deref Ndz-atom) %) @alpha)
							  (my-gen-likelihood-prob
							   ((deref Nz-atom) %)
							   ((deref (Nzw-atom %)) word-id)
							   (corpora :V) @beta)
							  )
						      (range @K))))
			  Ndz ((deref Ndz-atom) next-z)
			  Nz ((deref Nz-atom) next-z)
			  Nzw ((deref (Nzw-atom next-z)) word-id)]
		      (set-new-stat word-idx word-id next-z Ndz Nz Nzw inc)))))))))
	(println (str iter ", " (log-likelihood (corpora-map deref corpora))))))))