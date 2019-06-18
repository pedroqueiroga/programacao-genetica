(declaim (sb-ext:muffle-conditions style-warning))

;; utils.lisp, enquanto nao sabemos separar os arquivos

(defun zipmap (x y)
  (let ((ht (make-hash-table)))
    (mapcar #'(lambda (a b)
                (setf (gethash a ht) b))
            x y)
    ht))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun rand-nth (l)
  (nth (random (length l)) l))

(defun repeatedly (n f)
  (loop repeat n
     collect (funcall f)))

(defun range (start end)
  (loop for x from start to end collect x))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom  l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun codesize (p)
  (cond ((null p) 0)
        ((atom p) 1)
        (t (reduce #'+ (mapcar #'codesize p)))))

(defun rpt (n x)
  (loop repeat n collect x))

(defun random-subtree (p)
  (if (zerop (random (codesize p)))
      p
      (random-subtree
       (rand-nth (apply #'append
                        (map 'list
                             (lambda (x) (rpt (codesize x) x))
                             (cdr p)))))))

(defun replace-random-subtree (p replacement)
  (if (zerop (random (codesize p)))
      replacement
      (let ((position-to-change
             (rand-nth (apply #'append
                              (map 'list
                                   (lambda (x y) (rpt (codesize x) y))
                                   (cdr p)
                                   (range 1 (length (cdr p))))))))
        (map 'list
             (lambda (x y)
               (if x
                   (replace-random-subtree y replacement) y))
             (loop for x in (range 0 (length p))
                  collect (= x position-to-change))
             p))))
              
                     

;; EOF ---------------------------------------------------------------
;; resto.lisp

(setf *random-state* (make-random-state t))

(defvar *function-table* (zipmap '(+  -  * /)
                                 '(2  2  2 2)))

(defvar *target-data* '((0.0112558035966573 0.00987444022147894 0.533699367702128 0.544384208135945)
                        (0.0101708421306285 0.00885034596011529 0.544384208135945 0.559177041733996)
                        (0.0155032055675006 0.0190304101939686 0.559177041733996 0.58187837672216)
                        (0.0270704723342726 0.0323007911284284 0.58187837672216 0.61058733418044)))

(defun random-function ()
  (rand-nth (hash-keys *function-table*)))

(defun random-terminal ()
  (rand-nth '(in1 in2 in3)))

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
  (let ((value-function (eval (list 'lambda '(in1 in2 in3) p))))
    (reduce #'+ (map 'list
                     (lambda (l)
                       (destructuring-bind (in1 in2 in3 correct_output) l
                         (handler-case (abs (- (funcall value-function in1 in2 in3)
                                               correct_output))
                           (division-by-zero () 99999))))
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
          (format t "Best program: ~a~%" best)
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
