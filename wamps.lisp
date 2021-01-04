(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

(defun know-city-nodes ()
   (mapcar (lambda (node)
             (if (member node *visited-nodes*)
                 (let ((n (assoc node *congestion-city-nodes*)))
                   (if (eql node *player-pos*)
                       (append n '(*))
                       n))
                 (list node '?)))
           (remove-duplicates
             (append *visited-nodes*
                     (mapcan (lambda (node)
                               (mapcar #'car
                                       (cdr (assoc node
                                                   *congestion-city-nodes*))))
                             *visited-nodes*)))))
