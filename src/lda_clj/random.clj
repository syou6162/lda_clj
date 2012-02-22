(ns lda_clj.random)

(import '(org.apache.commons.math.random MersenneTwister))

(def seed 12345)
(def ^:dynamic *r* (new MersenneTwister seed))
