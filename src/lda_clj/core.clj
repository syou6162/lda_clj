(ns lda_clj.core)

(defstruct document :w :z :Nz)
(def w [1 2 3 4 5])

(def z (loop [z [] x (count w)] ;; init condition for loop
	(if (zero? x)
	  z
	  (recur (conj z x) (dec x)))))

(struct document w z (frequencies z))

(def word2id-map (atom {}))
(defn get-word-id! [word]
  (if (get @word2id-map word)
    (get @word2id-map word)
    (do
      (swap! word2id-map assoc word
	     (count @word2id-map))
      (get @word2id-map word))))

(def wsj-filename "/Users/yasuhisa/nCRP/wsj.txt")
(use '[clojure.contrib.duck-streams])

(def corpos (let [wsj-filename "/Users/yasuhisa/nCRP/wsj.txt"
		  lines (line-seq (reader wsj-filename))
		  lines (for [line lines]
			  (map #(get-word-id! %) line))]
	      lines))
