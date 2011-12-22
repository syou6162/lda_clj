(ns lda_clj.document
  (:use [lda_clj.util]))

(defstruct document :w :z :Nz)

(defn create-document [w]
  (struct document (vec w)
	  (atom (vec (for [idx (range (count w))] nil)))
	  (atom (vec (for [idx (range @K)] 0)))))

(defn create-document-with-random-topic-assignments [w]
  (let [z (vec (for [idx (range (count w))] (rand-int @K)))]
    (struct document (vec w) (atom z)
	    (atom (reduce (fn [count v]
			    (assoc count v (inc (nth count v))))
			  (vec (repeat @K 0)) z)))))

; (create-document-with-random-topic-assignments '[[1 2] [3]])
; (document-map deref (create-document-with-random-topic-assignments '[[1 2] [3]]))

(defn document-map [f d]
  (struct document (d :w) (f (d :z)) (f (d :Nz))))

(defn valid-document? [document]
  (letfn [(length-equal? []
			 (= (count (document :w)) (count (document :z))))
	  (valid-topic-num? [topic-id]
			    (= ((document :Nz) topic-id)
			       (count (filter #(= topic-id %) (document :z)))))
	  ]
    (and 
     (->> (map #(valid-topic-num? %) (range (count (document :Nz))))
	  (every? #(and true %))) ;; all-topic-num-valid?
     (length-equal?))))
