(ns lda_clj.document
  (:use [lda_clj.random]))

(defstruct document :w :z :Nz)

(defn create-document [w K]
  (struct document (vec w)
	  (vec (for [idx (range (count w))] nil))
	  (vec (for [idx (range K)] 0))))

(defn create-document-with-random-topic-assignments [w K]
  (let [z (vec (for [idx (range (count w))] (.nextInt *r* K)))
	freq (frequencies z)]
    (struct document (vec w) z
	    (vec (for [z (range K)]
		   (get-in freq [z] 0))))))

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
