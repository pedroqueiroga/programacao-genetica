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
