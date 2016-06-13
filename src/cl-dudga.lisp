;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-user)
(defpackage cl-dudga
  (:use :cl :lparallel))
(in-package :cl-dudga)

;;; Setting for lparallel
(defparameter *kernel* (make-kernel 4))

;;; Structures

;; Ring buffer
(defstruct ring-buffer
  (size 0)
  (buffer nil)
  (tail 0))

(defun rb-push! (ring-buffer datum)
  (let ((size (ring-buffer-size ring-buffer)))
    (cond ((zerop size)
           (error "ring-buffer size must be set initially."))
          ((null (ring-buffer-buffer ring-buffer))
           (setf (ring-buffer-buffer ring-buffer)
                 (make-array size :initial-element datum))
           (setf (ring-buffer-tail ring-buffer) (1- size))
           datum)
          (t
           (let* ((pop-datum-position (mod (1+ (ring-buffer-tail ring-buffer)) size))
                  (pop-datum (svref (ring-buffer-buffer ring-buffer) pop-datum-position)))
             (setf (svref (ring-buffer-buffer ring-buffer) pop-datum-position) datum) ; push
             (setf (ring-buffer-tail ring-buffer) pop-datum-position)
             pop-datum)))))

(defun rb-average (rb)
  (/ (loop for elem across (ring-buffer-buffer rb) summing elem)
     (ring-buffer-size rb)))

(defstruct individual
  (chromosome-size 4 :type integer)
  (chromosome #*0000 :type simple-bit-vector)
  (evaluated-value most-negative-double-float :type double-float))

(defun make-random-individual (chromosome-size direction-of-optimization)
  (check-type chromosome-size integer)
  (let ((bv (make-array chromosome-size :element-type 'bit)))
    ;; initialize bit vector random
    (loop for i from 0 to (1- chromosome-size) do
      (setf (aref bv i) (random 2)))
    (make-individual :chromosome-size chromosome-size
		     :chromosome bv
		     :evaluated-value (ecase direction-of-optimization
					(maximize most-negative-double-float)
					(minimize most-positive-double-float)))))

(defun copy-random-individual (org)
  (let ((bv (copy-seq (individual-chromosome org)))
        (clone (copy-individual org)))
    (setf (individual-chromosome clone) bv)
    clone))

(defstruct island
  (generation 1 :type integer)
  (population #() :type simple-vector)
  (child-population #() :type simple-vector))

(defun make-random-island (chromosome-size direction-of-optimization)
  (let ((parent1 (make-random-individual chromosome-size direction-of-optimization))
        (parent2 (make-random-individual chromosome-size direction-of-optimization)))
    (make-island :population (vector parent1 parent2)
                 :child-population (vector (copy-random-individual parent1) (copy-random-individual parent2)))))

(defstruct problem
  (population-size 1 :type integer)
  (migration-interval 1 :type integer)
  islands
  evaluate-function
  end-condition-predicate
  direction-of-optimization
  better-predicate
  worse-predicate
  ;; for detect convergence
  max-generation
  average-value-rb
  previous-average-value-sma
  converge-threshold
  converge-count
  converge-max-count)

(defun make-random-problem (population-size chromosome-size migration-interval
                            evaluate-function end-condition-predicate
                            &key (direction-of-optimization 'maximize)
                              (max-generation most-positive-fixnum)
                              (evaluated-value-rb-size 30)
                              (converge-threshold 0.01d0)
                              (converge-max-count 5))
  (assert (evenp population-size))
  (assert (or (eq direction-of-optimization 'maximize)
	      (eq direction-of-optimization 'minimize)))
  (let* ((n-islands (/ population-size 2))
	 (islands (make-array n-islands)))
    (loop for i from 0 to (1- n-islands) do
      (setf (aref islands i) (make-random-island chromosome-size direction-of-optimization)))
    (make-problem :population-size population-size
		  :migration-interval migration-interval
		  :islands islands
		  :evaluate-function evaluate-function
		  :end-condition-predicate end-condition-predicate
		  :direction-of-optimization direction-of-optimization
		  :better-predicate (if (eq direction-of-optimization 'maximize) #'> #'<)
		  :worse-predicate  (if (eq direction-of-optimization 'maximize) #'< #'>)
                  :max-generation max-generation
                  :average-value-rb (make-ring-buffer :size evaluated-value-rb-size)
                  :previous-average-value-sma 0d0
                  :converge-threshold converge-threshold
                  :converge-count 0
                  :converge-max-count converge-max-count)))

;;; Problem properties

(defun problem-population (problem)
  (let* ((islands (problem-islands problem))
	 (len (length islands)))
    (labels ((iter (i product)
	       (if (= i len)
		 (nreverse product)
		 (let ((island (aref islands i)))
		   (iter (1+ i)
			 (cons (aref (island-population island) 1)
			       (cons (aref (island-population island) 0)
				     product)))))))
      (iter 0 nil))))

(defmacro foreach-individual (problem org-var &body body)
  (let ((island (gensym)))
    `(loop for ,island across (problem-islands ,problem) do
      (loop for ,org-var across (island-population ,island) do
            ,@body))))

(defun best/worst-individual (problem)
  (let* ((maximize? (eq (problem-direction-of-optimization problem) 'maximize))
         (best-value  (if maximize? most-negative-double-float most-positive-double-float))
         (worst-value (if maximize? most-positive-double-float most-negative-double-float))
         (first-org (aref (island-population (aref (problem-islands problem) 0)) 0))
         (best-org  first-org)
         (worst-org first-org))
    (foreach-individual problem org
      (let ((val (individual-evaluated-value org)))
        (cond ((funcall (problem-better-predicate problem) val best-value)
               (setf best-value val
                     best-org org))
              ((funcall (problem-worse-predicate problem) val worst-value)
               (setf worst-value val
                     worst-org org)))))
    (values best-org worst-org)))
 
;;; Crossover & Mutation

;; 1-point crossover
(defun crossover! (island)
  (let* ((size (individual-chromosome-size (aref (island-population island) 0)))
	 (pivot (random (1- size))))
    (loop for i from 0 to pivot do
      (setf (aref (individual-chromosome (aref (island-child-population island) 0)) i)
	    (aref (individual-chromosome (aref (island-population island) 0)) i)
	    (aref (individual-chromosome (aref (island-child-population island) 1)) i)
	    (aref (individual-chromosome (aref (island-population island) 1)) i)))
    (loop for i from (1+ pivot) to (1- size) do
      (setf (aref (individual-chromosome (aref (island-child-population island) 0)) i)
	    (aref (individual-chromosome (aref (island-population island) 1)) i)
	    (aref (individual-chromosome (aref (island-child-population island) 1)) i)
	    (aref (individual-chromosome (aref (island-population island) 0)) i)))))

;; 2-point crossover
;; (defun crossover! (island)
;;   (let* ((size (individual-chromosome-size (aref (island-population island) 0)))
;; 	 (pivot1 (random (1- size)))
;;          (pivot2 (random (1- size)))
;;          pivot-smaller
;;          pivot-bigger)
;;     (if (> pivot1 pivot2)
;;       (setf pivot-smaller pivot2 pivot-bigger pivot1)
;;       (setf pivot-smaller pivot1 pivot-bigger pivot2))
;;     (loop for i from pivot-smaller to pivot-bigger do
;;       (setf (aref (individual-chromosome (aref (island-child-population island) 0)) i)
;;             (aref (individual-chromosome (aref (island-population island) 1)) i)
;;             (aref (individual-chromosome (aref (island-child-population island) 1)) i)
;;             (aref (individual-chromosome (aref (island-population island) 0)) i)))))

(defun flip-1bit! (arr posi)
  (setf (aref arr posi)
	(if (= (aref arr posi) 1) 0 1)))

(defun mutation! (island)
  (let* ((size (individual-chromosome-size (aref (island-population island) 0)))
	 (mutation-position (random size))
	 (mutation-position-1bit-shift (if (= mutation-position (1- size))
					 0
					 (1+ mutation-position))))
    (flip-1bit! (individual-chromosome (aref (island-child-population island) 0))
		mutation-position)
    (flip-1bit! (individual-chromosome (aref (island-child-population island) 1))
		mutation-position-1bit-shift)))

(defmacro overwrite-individual! (org1 org2)
  `(progn
     (loop for i from 0 to (1- (individual-chromosome-size ,org1)) do
       (setf (aref (individual-chromosome ,org1) i)
	     (aref (individual-chromosome ,org2) i)))
     (setf (individual-evaluated-value ,org1)
	   (individual-evaluated-value ,org2))))

;;; Main processes

(defun island-one-generation-process! (island problem)
  ;; Generate children
  (crossover! island)
  (mutation! island)
  ;; Evaluate children
  (let ((eval-func (problem-evaluate-function problem)))
    (setf (individual-evaluated-value (aref (island-child-population island) 0))
	  (funcall eval-func (individual-chromosome (aref (island-child-population island) 0))))
    (setf (individual-evaluated-value (aref (island-child-population island) 1))
	  (funcall eval-func (individual-chromosome (aref (island-child-population island) 1)))))
  ;; Selection
  (let ((worse-parent
	 (if (funcall (problem-worse-predicate problem)
		      (individual-evaluated-value (aref (island-population island) 0))
		      (individual-evaluated-value (aref (island-population island) 1)))
	   0 1))
	(better-child
	 (if (funcall (problem-better-predicate problem)
		      (individual-evaluated-value (aref (island-child-population island) 0))
		      (individual-evaluated-value (aref (island-child-population island) 1)))
	   0 1)))
    (overwrite-individual!
     (aref (island-population island) worse-parent)
     (aref (island-child-population island) better-child)))
  (incf (island-generation island)))

(defun island-unit-generation-process-and-select-migrant! (island problem)
  (island-one-generation-process! island problem)
  (if (zerop (mod (island-generation island) (problem-migration-interval problem)))
    (aref (island-population island) (random 2)) ; select migrant
    (island-unit-generation-process-and-select-migrant! island problem)))
  
;; Fisher–Yates shuffle
(defun shuffle-vector! (vec)
  (loop for i from (1- (length vec)) downto 1 do
    (let* ((j (random (1+ i)))
	   (tmp (svref vec i)))
      (setf (svref vec i) (svref vec j))
      (setf (svref vec j) tmp)))
  vec)

(defun import-migrants! (problem migrant-vector)
    (loop for island across (problem-islands problem)
	  for migrant across migrant-vector do
	    (let ((worse-parent
		   (if (funcall (problem-worse-predicate problem)
				(individual-evaluated-value (aref (island-population island) 0))
				(individual-evaluated-value (aref (island-population island) 1)))
		     0 1)))
	      (overwrite-individual! (aref (island-population island) worse-parent) migrant))))
  
(defun run-problem (problem)
  (if (funcall (problem-end-condition-predicate problem) problem)
    'quit
    (let ((migrant-vector
	   (pmap 'vector #'(lambda (island)
			     (island-unit-generation-process-and-select-migrant! island problem))
		 (problem-islands problem))))
      (shuffle-vector! migrant-vector)
      (import-migrants! problem migrant-vector)
      (run-problem problem))))

;;; Detect convergence
(defun problem-average-value (problem)
  (let ((sum 0d0))
    (handler-case
        (foreach-individual problem org
          (incf sum (individual-evaluated-value org)))
      (FLOATING-POINT-OVERFLOW (c)
        (declare (ignore c))
        (if (eq (problem-direction-of-optimization problem) 'maximize)
          most-negative-double-float
          most-positive-double-float)))
    (/ sum (problem-population-size problem))))

(defun converge? (problem)
  ;; (push (problem-average-value problem) *ave-list*)
  ;; (push (problem-previous-average-value-sma problem) *ave-list-sma*)
  ;; (push (individual-evaluated-value (best/worst-individual problem)) *result-best*)

  (rb-push! (problem-average-value-rb problem) (problem-average-value problem))

  (or (> (island-generation (aref (problem-islands problem) 0)) (problem-max-generation problem))
      (let ((ave (rb-average (problem-average-value-rb problem))))
        (when (< (abs (- ave (problem-previous-average-value-sma problem)))
                 (problem-converge-threshold problem))
          (incf (problem-converge-count problem)))
        (setf (problem-previous-average-value-sma problem) ave)
        (>= (problem-converge-count problem) (problem-converge-max-count problem)))))
  
;;; Report

(defun print-individual (org)
  (format t "org: ~A ~A~%"
	  (individual-chromosome org)
	  (individual-evaluated-value org)))

(defun print-island (island)
  (format t "parent1: ~A ~A~%parent2: ~A ~A~%child1: ~A ~A~%child2: ~A ~A~%"
	  (individual-chromosome (aref (island-population island) 0))
	  (individual-evaluated-value (aref (island-population island) 0))
	  (individual-chromosome (aref (island-population island) 1))
	  (individual-evaluated-value (aref (island-population island) 1))
	  (individual-chromosome (aref (island-child-population island) 0))
	  (individual-evaluated-value (aref (island-child-population island) 0))
	  (individual-chromosome (aref (island-child-population island) 1))
	  (individual-evaluated-value (aref (island-child-population island) 1))))

(defun print-population (problem)
  (foreach-individual problem org
    (print-individual org)))
