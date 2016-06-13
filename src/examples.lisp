;;; Example

(defun count-number-of-1 (chromosome)
  (* (count-if (lambda (x) (= x 1)) chromosome) 1d0))
	        
(defparameter prob1
  (make-random-problem 100  ; Population size
                       50   ; Chromosome size
                       5    ; Migration interval
                       #'count-number-of-1 ; Evaluation function
                       #'converge?         ; Detect convegence function
                       :direction-of-optimization 'minimize))

;; CL-DUDGA> (run-problem prob1)
;; QUIT
;; CL-DUDGA> (best/worst-individual prob1)
;; #S(INDIVIDUAL
;;    :CHROMOSOME-SIZE 50
;;    :CHROMOSOME #*00000000000000000000000000000000000000000000000000
;;    :EVALUATED-VALUE 0.0d0)
;; #S(INDIVIDUAL
;;    :CHROMOSOME-SIZE 50
;;    :CHROMOSOME #*00000000000000000000000010000000000000000000000000
;;    :EVALUATED-VALUE 1.0d0)

(defparameter prob2
  (make-random-problem 100 50 5 #'count-number-of-1 #'converge?
                       :direction-of-optimization 'maximize))

;; CL-DUDGA> (run-problem prob2)
;; QUIT
;; CL-DUDGA> (best/worst-individual prob2)
;; #S(INDIVIDUAL
;;    :CHROMOSOME-SIZE 50
;;    :CHROMOSOME #*11111111111111111111111111111111111111111111111111
;;    :EVALUATED-VALUE 50.0d0)
;; #S(INDIVIDUAL
;;    :CHROMOSOME-SIZE 50
;;    :CHROMOSOME #*11111111111111111111111111111111111111111111011111
;;    :EVALUATED-VALUE 49.0d0)

(defun froid-eval (bit-vec)
  (labels ((calc-score (i sum-a sum-b)
	     (if (> i (length bit-vec))
	       (abs (- sum-a sum-b))
	       (if (= (aref bit-vec (1- i)) 1)
		 (calc-score (1+ i) (+ sum-a (sqrt i)) sum-b)
		 (calc-score (1+ i) sum-a (+ sum-b (sqrt i)))))))
    (calc-score 1 0d0 0d0)))

(defparameter prob3
  (make-random-problem 300 50 5 #'froid-eval #'converge?
		:direction-of-optimization 'minimize))

(defparameter prob3
  (make-random-problem 300 50 5 #'froid-eval #'converge?
                       :direction-of-optimization 'minimize
                       :max-generation 1000
                       :converge-threshold 0d0))

;; CL-DUDGA> (run-problem prob3)
;; QUIT
;; CL-DUDGA> (best/worst-individual prob3)
;; #S(INDIVIDUAL
;;    :CHROMOSOME-SIZE 50
;;    :CHROMOSOME #*00101111010111010100000110010001111000100100111101
;;    :EVALUATED-VALUE 4.870891571044922d-4)
;; #S(INDIVIDUAL
;;    :CHROMOSOME-SIZE 50
;;    :CHROMOSOME #*00101111000111010100000110010000111000100100111101
;;    :EVALUATED-VALUE 17.637776613235474d0)

;; 10bit * dimension
(defun rastrign (x-list)
  (let ((n (length x-list)))
    (+ (* 10 n)
       (loop for xi in x-list summing
	 (- (* xi xi) (* 10 (cos (* 2 pi xi))))))))

(wiz:splot-list
 (lambda (x y)
   (rastrign (list x y)))
 (wiz:seq -5.12 5.12 :by 0.1) (wiz:seq -5.12 5.12 :by 0.1) :map t)

(wiz:splot-list
 (lambda (x y)
   (rastrign (list x y)))
 (wiz:seq -5.12 5.12 :by 0.1) (wiz:seq -5.12 5.12 :by 0.1) :map nil :style 'dots)

(defun bit-subseq->number (bit-vec start-index end-index)
  (let ((n (- end-index start-index)))
    (labels ((bin2dec (acc i n)
	       (if (> i end-index)
		 acc
		 (if (= (bit bit-vec i) 1)
		   (bin2dec (+ acc (expt 2 n)) (1+ i) (1- n))
		   (bin2dec acc (1+ i) (1- n))))))
      (bin2dec 0 start-index n))))

(defun rastrign-eval (bit-vec)
  (let* ((dimension 20)
	 (x-bit-length 10)
	 (x-list
	  (loop for i from 0 to (1- dimension) collect
	    (bit-subseq->number bit-vec (* i x-bit-length) (1- (* (1+ i) x-bit-length))))))
    (rastrign (mapcar (lambda (x) (* (- x 511.5) 0.01)) x-list))))

(defparameter prob4
  (make-random-problem 300 200 5 #'rastrign-eval #'converge?
                       :direction-of-optimization 'minimize
                       :converge-max-count 20
                       :converge-threshold 0.001d0))

(defparameter prob5
  (make-random-problem 300 200 5 #'rastrign-eval #'converge?
                       :direction-of-optimization 'minimize
                       :max-generation 5000
                       :converge-threshold 0d0))

(defparameter *ave-list* nil)
(defparameter *ave-list-sma* nil)
(defparameter *result-best* nil)

(wiz:plot-lists (list (reverse *ave-list*)
                      (reverse *ave-list-sma*)
                      (reverse *result-best*))
                :y-range '(0 30)
                :x-label "Genarations"
                :y-label "Evaluated value"
                :title-list '("Average" "SMA30" "Best"))
