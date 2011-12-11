(ns lda_clj.document
  (:use [lda_clj.util]))

(defstruct document :w :z :Nz)

(defn create-document [w]
  (struct document (vec w)
	  (vec (for [idx (range (count w))] nil))
	  (vec (for [idx (range @K)] 0))))

(defn create-document-with-random-topic-assignments [w]
  (let [z (vec (for [idx (range (count w))] (rand-int @K)))]
    (struct document (vec w) z 
	    (reduce (fn [count v]
		      (assoc count v (inc (nth count v))))
		    (vec (repeat @K 0)) z))))

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
