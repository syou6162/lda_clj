(ns lda_clj.document
  (:use [lda_clj.util]))

(defstruct document :w :z :Nz)

(defn create-document [w]
  (struct document w
	  (vec (for [idx (range (count w))] nil))
	  (vec (for [idx (range @K)] nil))))

(defn create-document-with-random-topic-assignments [w]
  (let [z (vec (for [idx (range (count w))] (rand-int @K)))]
    (struct document w z
	    (reduce (fn [count v]
		      (assoc count v (inc (nth count v))))
		    (vec (repeat @K 0)) z)
	    )))

(defn valid-document? [document]
  (letfn [(length-equal? []
			 (= (count (document :w)) (count (document :z))))
	  (valid-topic-num? [topic-id]
			    (= ((document :Nz) topic-id)
			       (count (filter #(= topic-id %) (document :z)))))
	  ]
    (and 
     (every? #(and true %) (map #(valid-topic-num? %) (range (count (document :Nz))))) ;; all-topic-num-valid?
     (length-equal?))))

;; (create-document-with-random-topic-assignments [1 2 3])

;; (def wsj-filename "/Users/yasuhisa/nCRP/wsj.txt")
;; (use '[clojure.contrib.duck-streams :only (reader)])
;; (use '[clojure.contrib.string :only (split)])

;; (use '[lda_clj.core])

;; (def word2id-map (atom {}))
;; (def documents (let [lines (line-seq (reader wsj-filename))]
;; 		 (for [line (take 3 (map #(split #"\s" %) lines))]
;; 		   (create-document
;; 		    (for [word (take 3 line)]
;; 		      (let [tmp (get-word-id [@word2id-map word])
;; 			    new-map (reset! word2id-map (first tmp))
;; 			    word-id (second tmp)]
;; 			word-id))))))

;; (let [lines (line-seq (reader wsj-filename))]
;;   (for [line (map #(split #"\s" %) lines)]
;;     (for [word line]
;;       (let [tmp (get-word-id [@word2id-map word])
;; 	    new-map (reset! word2id-map (first tmp))
;; 	    word-id (second tmp)]
;; 	word-id))))

;; (println (count @word2id-map)) ;; V

