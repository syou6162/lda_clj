(ns lda_clj.util)

(defn logsumexp [x y flg]
  (if flg
    y
    (if (= x y)
      (+ x 0.69314718055) ;; log(2)
      (let [vmin (min x y)
	    vmax (max x y)]
	(if (> vmax (+ vmin 50))
	  vmax
	  (+ vmax (Math/log (+ (Math/exp (- vmin vmax)) 1.0))))))))

(defn calc-posts-vectors-and-psum [unscaled-prop]
  (reduce (fn [result idx]
	    (let [posts (first result)
		  psum (second result)
		  item (Math/log (unscaled-prop idx))]
	      [(conj posts item) (logsumexp psum item (= 0 idx))]))
	  [[] 0.0] (range (count unscaled-prop))))

(defn decode-logsumexp [psum posts idx]
  (assoc posts idx (+ (Math/exp (- (posts idx) psum)) (posts (- idx 1)))))

(use '[clojure.contrib.probabilities.finite-distributions :only (uniform)])

(defn sample [unscaled-prop]
  (let [r (rand)
	tmp (calc-posts-vectors-and-psum unscaled-prop)
	psum (second tmp)
	posts-tmp (first tmp)
	posts (assoc posts-tmp 0 (Math/exp (- (posts-tmp 0) psum)))]
    (if (<= r (posts 0))
      0
      (loop [idx 1
	     p (decode-logsumexp psum posts idx)]
	(if (= idx (count p))
	  (- idx 1)
	  (if (<= r (p idx))
	    idx
	    (recur (inc idx) (decode-logsumexp psum p (inc idx)))))))))