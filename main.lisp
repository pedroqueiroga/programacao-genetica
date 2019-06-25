(load "xlsx-package.lisp")
(load "xlsx.asd")
(load "xlsx.lisp")
(load "utils.lisp")

(declaim (sb-ext:muffle-conditions style-warning))

(setf *random-state* (make-random-state t))

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

     until (> generation 1000)
     do (progn
          (format t "=========================~%")
          (format t "Generation: ~a~%" generation)
          (format t "Best error: ~a~%" best-error)
          ;;(format t "Best program: ~a~%" best)
          (format t "     Median error: ~a~%" (p-error
                                               (nth (floor popsize 2)
                                                    population)))
          (format t "     Average program size: ~a~%"
                  (float (/ (reduce #'+ (map 'list
                                             #'length
                                             (map 'list
                                                  #'flatten population)))
                            (length population))))
          (if (< best-error 0.001)
              (progn
                (format t "Success: ~a~%" best)
                (loop-finish))))))

(defun main (string popsize)
  (setq *target-data* (xlsx:read-from-sheet string))
  (evolve popsize))