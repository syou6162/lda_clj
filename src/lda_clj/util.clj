(ns lda_clj.util
  (:import (org.apache.commons/math.random.MersenneTwister))
  ; (:use [com.hackinghat.common-library.mersenne-twister])
  )

; (use '[LogsumexpBasedSampler])
; (new LogsumexpBasedSampler (to-array '(0.1 0.3)))
; (use '[clojure.contrib.generic.math-functions :only (exp log)])
(use '[clojure.contrib.import-static :only (import-static)])
(import-static org.apache.commons.math.util.FastMath log exp)

(def K (atom 3))
(def alpha (atom 0.1))
(def beta (atom 0.1))

(def my-mt (new org.apache.commons.math.random.MersenneTwister))

(defn ^Double logsumexp [^Double x ^Double y ^Boolean flg]
  (if (boolean flg)
    (double y)
    (if (== (double x) (double y))
      (+ (double x) (double 0.69314718055)) ;; log(2)
      (let [vmin (double (min x y))
	    vmax (double (max x y))]
	(if (> (double vmax) (+ (double vmin) (double 50.0)))
	  (double vmax)
	  (+ (double vmax)
	     (Math/log (+ (exp (- (double vmin) (double vmax))) (double 1.0)))))))))

;; (defn logsumexp-without-type [x y flg]
;;   (if flg
;;     y
;;     (if (= x y)
;;       (+ x 0.69314718055) ;; log(2)
;;       (let [vmin (min x y)
;; 	    vmax (max x y)]
;; 	(if (> vmax (+ vmin 50))
;; 	  vmax
;; 	  (+ vmax (Math/log (+ (Math/exp (- vmin vmax)) 1.0))))))))

;; (def N 1000000)

;; (dotimes [_ 10]
;;   (let [N N
;; 	rand-seq-x (doall (vec (map (fn [_] (rand)) (range N))))
;; 	rand-seq-y (doall (vec (map (fn [_] (rand)) (range N))))]
;;     (do
;;       (reduce + rand-seq-x)
;;       (reduce + rand-seq-y)
;;       (println "without type")
;;       (time (map #(logsumexp-without-type (rand-seq-x %) (rand-seq-y %) false) (range N)))
;;       (println "with type")
;;       (time (map #(logsumexp (rand-seq-x %) (rand-seq-y %) false) (range N)))
;;       )))

(defn calc-posts-vectors-and-psum [^doubles unscaled-prop]
  (reduce (fn [result idx]
	    (let [[posts psum] result
		  item (Math/log (unscaled-prop idx))]
	      [(conj posts item) (logsumexp psum item (= 0 idx))]))
	  [[] 0.0] (range (count unscaled-prop))))

(defn calc-posts-vectors-and-psum! [^doubles posts]
  (let [psum-atom (atom 0.0)]
    (do
      (dorun (for [idx (range (count posts))]
	       (let [item (Math/log @(posts idx))]
		 (do
		   (reset! (posts idx) item)
		   (reset! psum-atom (logsumexp @psum-atom item (= 0 idx)))))))
      @psum-atom)))

(defn ^Double calc-posts-vectors-and-psum-with-java [^doubles posts]
  (areduce
   ^ints (int-array (range (alength posts)))
   idx
   psum
   0.0
   (logsumexp (double psum)
	      (aset-double
	       ^doubles posts
	       idx
	       (Math/log (aget ^doubles posts (int idx))))
	      (== (int 0) (int idx)))))

; (calc-posts-vectors-and-psum-with-java (double-array (range 10)))

(calc-posts-vectors-and-psum '[1 2 3])
(calc-posts-vectors-and-psum! [(atom 1) (atom 2) (atom 3)])
(calc-posts-vectors-and-psum-with-java (double-array '[1.0 2.0 3.0]))

(defn ^Double decode-logsumexp [^Double psum ^doubles posts ^Integer idx]
  (assoc posts idx (+ (Math/exp (- (posts idx) psum)) (posts (dec idx)))))

(defn ^Double decode-logsumexp! [^Double psum ^doubles posts ^Integer idx]
  (let [val (+ (Math/exp (- @(posts idx) psum)) @(posts (dec idx)))]
    (reset! (posts idx) val)))

(defn ^Double decode-logsumexp-with-java [^Double psum ^doubles posts ^Integer idx]
  (let [val (+ (exp (- (aget ^doubles posts (int idx)) (double psum)))
	       (aget ^doubles posts (dec (int idx))))]
    (aset-double ^doubles posts (int idx) (double val))))

(let [[posts psum] (calc-posts-vectors-and-psum '[1 2 3])]
  (decode-logsumexp psum posts 1))

(let [posts [(atom 1) (atom 2) (atom 3)]
      psum (calc-posts-vectors-and-psum! posts)]
  (decode-logsumexp! psum posts 1))

(let [posts (double-array '[1.0 2.0 3.0])
      psum (calc-posts-vectors-and-psum-with-java posts)]
  (decode-logsumexp-with-java psum posts 1))

(defn ^Integer sample [^doubles unscaled-prop]
  (let [r (rand) ; (. my-mt nextDouble) ; (rand)
	[posts-tmp psum] (calc-posts-vectors-and-psum unscaled-prop)
	^doubles posts (assoc posts-tmp 0 (Math/exp (- (posts-tmp 0) psum)))]
    (if (<= r (posts 0))
      0
      (loop [idx 1
	     ^doubles posts (decode-logsumexp psum posts idx)]
	(if (= idx (count posts))
	  (dec idx) ;; 末尾
	  (if (<= r (posts idx))
	    idx
	    (recur (inc idx) (decode-logsumexp psum posts (inc idx)))))))))

(defn ^Integer sample-with-atom [^doubles posts]
  (let [r (rand) ; (. my-mt nextDouble) ; (rand)
	psum (calc-posts-vectors-and-psum! posts)
	N (count posts)
	init (reset! (posts 0) (Math/exp (- @(posts 0) psum)))]
    (if (< r init) 0
	(let [result (last (for [idx (range 1 N)
				 :when (> r (decode-logsumexp! psum posts idx))]
			     idx))]
	  (if (nil? result) (dec N) result)))))


(defn ^Integer sample-with-java [^doubles posts]
  (let [r (rand) ; (rand)
	psum (calc-posts-vectors-and-psum-with-java ^doubles posts)
	N (alength posts)
	init (aset-double ^doubles posts
			  (int 0)
			  (exp (- (aget ^doubles posts (int 0)) (double psum))))]
    (if (< r init) (int 0)
	(let [result (last (for [idx (range (int 1) (int N))
				 :when (> (double r)
					  (decode-logsumexp-with-java
					    (double psum)
					    ^doubles posts
					    (int idx)))]
			     (int idx)))]
	  (if (nil? result) (int (dec N)) (int result))))))

; (frequencies (map (fn [_] (sample-with-java (double-array '[0.1 0.3 0.6]))) (range 1000)))

(defn- ^Integer my-sample [^doubles xs]
  (let [r (* (reduce + xs) (rand))]
    (loop [idx 0, cum 0.0]
      (let [val (+ cum (xs idx))]
	(if (>= val r)
	  idx
	  (recur (inc idx) val))))))

(defn ^Integer my-sample-with-java [^doubles xs]
  (let [r (* (areduce ^doubles xs
		      idx
		      ret
		      0.0
		      (+ ret (aget ^doubles xs (int idx))))
	     (rand))]
    (loop [idx (int 0) cum (double 0.0)]
      (let [val (+ cum (aget ^doubles xs idx))]
	(if (>= val r)
	  (int idx)
	  (recur (inc idx) (double val)))))))

;; (dotimes [_ 5]
;;   (let [N 5000000
;; 	v (vec (doall (map (fn [_] (rand)) (range N))))]
;;     (println "=====(exp)=====")
;;     (time (dorun (map #(Math/exp %) v)))
;;     (time (dorun (map #(exp %) v)))
;;     (println "=====(log)=====")
;;     (time (dorun (map #(Math/log %) v)))
;;     (time (dorun (map #(log %) v)))
;;     ))

(def N 10000)

;; (let [num-of-topic 100
;;       rand-seq (doall (vec (map (fn [_] (rand)) (range num-of-topic))))
;;       rand-seq-with-atom (doall (vec (map (fn [_] (atom (rand))) (range num-of-topic))))
;;       rand-seq-with-java (double-array (doall (vec (map (fn [_] (rand)) (range num-of-topic)))))]
;;   (do
;;     (reduce + rand-seq)
;;     (println "==================================================")
;;     (println "my-sample")
;;     (time
;;      (dotimes [_ N]
;;        (my-sample rand-seq))
;;      )
;;     (println "my-sample-with-java")
;;     (time
;;      (dotimes [_ N]
;;        (my-sample-with-java ^doubles rand-seq-with-java))
;;      )
;;     (println "sample-with-java")
;;     (time
;;      (dotimes [_ N]
;;        (sample-with-java ^doubles rand-seq-with-java))
;;      )
;;     (println "sample-with-atom")
;;     (time
;;      (dotimes [_ N]
;;        (sample-with-atom rand-seq-with-atom))
;;      )
;;     (println "sample")
;;     (time
;;      (dotimes [_ N]
;;        (sample rand-seq))
;;      )
;;     (println "==================================================")    
;;     (println "sample")
;;     (time
;;      (dotimes [_ N]
;;        (sample rand-seq))
;;      )
;;     (println "my-sample-with-java")
;;     (time
;;      (dotimes [_ N]
;;        (my-sample-with-java ^doubles rand-seq-with-java))
;;      )
;;     (println "sample-with-atom")
;;     (time
;;      (dotimes [_ N]
;;        (sample-with-atom rand-seq-with-atom))
;;      )
;;     (println "sample-with-java")
;;     (time
;;      (dotimes [_ N]
;;        (sample-with-java ^doubles rand-seq-with-java))
;;      )
;;     (println "my-sample")
;;     (time
;;      (dotimes [_ N]
;;        (my-sample rand-seq))
;;      )
;;     (println "==================================================")
;;     (println "my-sample-with-java")
;;     (time
;;      (dotimes [_ N]
;;        (my-sample-with-java ^doubles rand-seq-with-java))
;;      )
;;     (println "sample")
;;     (time
;;      (dotimes [_ N]
;;        (sample rand-seq))
;;      )
;;     (println "sample-with-atom")
;;     (time
;;      (dotimes [_ N]
;;        (sample-with-atom ^doubles rand-seq-with-atom))
;;      )
;;     (println "sample-with-java")
;;     (time
;;      (dotimes [_ N]
;;        (sample-with-java ^doubles rand-seq-with-java))
;;      )
;;     (println "my-sample")
;;     (time
;;      (dotimes [_ N]
;;        (my-sample rand-seq))
;;      )
;;     ))

(defn get-word-id [vec]
  (let [[word2id-map word] vec
	word-id (word2id-map word)
	new-word-id (count word2id-map)]
    (if word-id
      [word2id-map word-id]
      [(assoc word2id-map word new-word-id) new-word-id])))

(defn get-words-ids [word2id-map words]
  (second (reduce (fn [result word]
		    (let [word2id-map (first result)
			  words-ids (second result)
			  tmp-result (get-word-id [word2id-map word])
			  word2id-map (first tmp-result)
			  new-word-id (second tmp-result)]
		      [(first tmp-result) (conj words-ids new-word-id)]))
		  [word2id-map []] words)))
