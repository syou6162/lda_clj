(ns lda_clj.document
  (:use [lda_clj.util]))

(defstruct document :w :z :Nz)

(defn create-document [w]
  (struct document (vec w)
	  (vec (for [idx (range (count w))] (atom nil)))
	  (vec (for [idx (range @K)] (atom 0)))))

(defn create-document-with-random-topic-assignments [w]
  (let [z (vec (for [idx (range (count w))] (atom (rand-int @K))))
	freq (->> z
		  (map deref)
		  (frequencies))]
    (struct document (vec w) z
	    (vec (for [z (range @K)]
		   (let [num (freq z)]
		     (atom (if num num 0))))))))

; (create-document-with-random-topic-assignments '[1 2])
; (document-map deref (create-document-with-random-topic-assignments '[[1 2] [3]]))

(defn document-map [f d]
  (struct document (d :w) (vec (map f (d :z))) (vec (map f (d :Nz)))))

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
