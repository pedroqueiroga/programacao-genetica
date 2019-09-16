(load "xlsx-package.lisp")
(load "xlsx.asd")
(load "xlsx.lisp")
(load "utils.lisp")

(declaim (sb-ext:muffle-conditions style-warning))

(setf *random-state* (make-random-state t))

(defvar *global-median-error* '())

(defvar *global-program-length* '())

(defvar *global-best-error* '())

(defvar *function-table* (zipmap '(+  -  * /)
                                 '(2  2  2 2)))

;;(defun define-target-data ()

(defvar *target-data* '());; (0.0112558035966573 0.00987444022147894 0.533699367702128 0.544384208135945)
                        ;; (0.0101708421306285 0.00885034596011529 0.544384208135945 0.559177041733996)
                        ;; (0.0155032055675006 0.0190304101939686 0.559177041733996 0.58187837672216)
                        ;; (0.0270704723342726 0.0323007911284284 0.58187837672216 0.61058733418044)
                        ;; (0.0365896732749262 0.0339778576482311 0.61058733418044 0.643567833442774)
                        ;; (0.0406992856331682 0.032385522692645 0.643567833442774 0.679223894124382)
                        ;; (0.0412599484172405 0.034057914225275 0.679223894124382 0.71609756075775)))

(defun random-function ()
  (rand-nth (hash-keys *function-table*)))

(defun random-terminal ()
  (rand-nth '(MLP SVM ARIMA)))

(defun random-code (depth)
  (if (or (zerop depth)
          (zerop (random 2)))
      (random-terminal)
      (let ((f (random-function)))
        (cons f (repeatedly (gethash f *function-table*)
                            (lambda () (random-code (- depth 1))))))))

(defun mutate (p)
  (replace-random-subtree p (random-code 2)))

(defun crossover (p q)
  (replace-random-subtree p (random-subtree q)))

(defun p-error (p)
  (let ((value-function (eval (list 'lambda '(MLP SVM ARIMA) p))))
    (reduce #'+ (map 'list
                     (lambda (l)
                       (destructuring-bind (MLP SVM ARIMA correct_output) l
                         (handler-case (abs (- (funcall value-function MLP SVM ARIMA)
                                               correct_output))
                           (error () 99999))))
                     *target-data*))))

(defun sort-by-error (population)
  (map 'list
       #'second
       (sort (map 'list
                  (lambda (x) (list (p-error x) x))
                  population)
             (lambda (l1 l2)
               (let ((err1 (car l1))
                     (err2 (car l2)))
                 (< err1 err2))))))

(defun select (sorted-population tournament-size)
  (let ((size (length sorted-population)))
    (nth (apply #'min (repeatedly tournament-size (lambda () (random size))))
         sorted-population)))


(defun evolve (popsize)
  (format t "Starting evolution...~%")
  (loop for generation = 0 then (+ generation 1)
     for population = (sort-by-error
                       (repeatedly popsize
                                   (lambda ()
                                     (random-code 2))))
     then (sort-by-error
           (append
            (repeatedly (* 1/10 popsize)
                        (lambda () (mutate (select population 5))))
            (repeatedly (* 8/10 popsize)
                        (lambda () (crossover (select population 5)
                                              (select population 5))))
            (repeatedly (* 1/10 popsize)
                        (lambda () (select population 5)))))
     for best = (car population)
     for best-error = (p-error best)
     for last-average-program-length = 
       (float (/ (reduce #'+ (map 'list
                                  #'length
                                  (map 'list
                                       #'flatten population)))
                 (length population)))
       
     for last-median-error =
       (p-error
        (nth (floor popsize 2)
             population))
       
     until (> generation 1000)
     do (progn
          (format t "=========================~%")
          (format t "Generation: ~a~%" generation)
          (format t "Best error: ~a~%" best-error)
          ;;(format t "Best program: ~a~%" best)
          (format t "     Median error: ~a~%"
                  last-median-error)
          (format t "     Average program size: ~a~%"
                  last-average-program-length)
          (if (or (< best-error 0.001) (is-worsening
                                        last-median-error
                                        last-average-program-length
                                        best-error))
              (progn
                (format t "This is the end, friend: ~a~%" best)
                (loop-finish))
              (progn
                (setf *global-program-length*
                      (fixed-append *global-program-length*
                                    last-average-program-length
                                    20))
                (setf *global-median-error*
                      (fixed-append *global-median-error*
                                    last-median-error
                                    20))
                (setf *global-best-error*
                      (fixed-append *global-best-error* best-error 20)))))))
  
(defun main (sheet popsize)
  (setq *target-data* (xlsx:read-from-sheet sheet))
  (evolve popsize))

(defun set-sheet (sheet)
  (setq *target-data* (xlsx:read-from-sheet sheet)))


(defun fixed-append (l v max-len)
  (cond ((= (length l) max-len) (append (cdr l) (list v)))
        ((= (length l) 0) (list v))
        (t (append l (list v)))))


(defun average (l)
  (if (= 0 (length l))
      -9999
      (/ (reduce '+ l) (length l))))

(defun is-worsening (last-median last-length last-best)
  ;; if the gain from last iteration was too small, too small being 0.1 for now
  (and (<
        (- (average *global-median-error*)
           last-median)
        0.1)
       (>
        (- last-length
           (average *global-program-length*))
        10)
       (<
        (- (average *global-best-error*)
           last-best)
        0.1)
       (> (length *global-median-error*) 10)
       (> (length *global-program-length*) 10)))
