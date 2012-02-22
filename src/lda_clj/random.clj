(ns lda_clj.random)

(import '(org.apache.commons.math.random MersenneTwister))

(def ^:dynamic *seed* 12345)
(def ^:dynamic *r* (new MersenneTwister *seed*))

(defn ^Integer my-sample [^doubles xs]
  (let [r (* (reduce + xs) (.nextDouble *r*))]
    (loop [idx 0, cum 0.0]
      (let [val (+ cum (xs idx))]
	(if (>= val r)
	  idx
	  (recur (inc idx) val))))))