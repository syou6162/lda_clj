(ns lda_clj.util
  (:import (org.apache.commons/math.random.MersenneTwister))
  (:use [com.hackinghat.common-library.mersenne-twister]))

(def K (atom 3))
(def alpha (atom 0.1))
(def beta (atom 0.1))

(def my-mt (new org.apache.commons.math.random.MersenneTwister))

(defn ^Double logsumexp [^Double x ^Double y ^Boolean flg]
  (if flg
    y
    (if (= x y)
      (+ x 0.69314718055) ;; log(2)
      (let [vmin (min x y)
	    vmax (max x y)]
	(if (> vmax (+ vmin 50))
	  vmax
	  (+ vmax (Math/log (+ (Math/exp (- vmin vmax)) 1.0))))))))

(defn calc-posts-vectors-and-psum [^doubles unscaled-prop]
  (reduce (fn [result idx]
	    (let [[posts psum] result
		  item (Math/log (unscaled-prop idx))]
	      [(conj posts item) (logsumexp psum item (= 0 idx))]))
	  [[] 0.0] (range (count unscaled-prop))))

(defn ^Double decode-logsumexp [^Double psum ^doubles posts ^Integer idx]
  (assoc posts idx (+ (Math/exp (- (posts idx) psum)) (posts (dec idx)))))

(defn ^Integer sample [^doubles unscaled-prop]
  (let [r (. my-mt nextDouble) ; (rand)
	[posts-tmp psum] (calc-posts-vectors-and-psum unscaled-prop)
	posts (assoc posts-tmp 0 (Math/exp (- (posts-tmp 0) psum)))]
    (if (<= r (posts 0))
      0
      (loop [^Integer idx 1
	     posts (decode-logsumexp psum posts idx)]
	(if (= idx (count posts))
	  (dec idx) ;; 末尾
	  (if (<= r (posts idx))
	    idx
	    (recur (inc idx) (decode-logsumexp psum posts (inc idx)))))))))

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
