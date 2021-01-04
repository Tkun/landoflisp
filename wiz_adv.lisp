(defun game-read()
(let ((cmd (read-from-string
                (concatenate 'string "(" (read-line) ")" ))))
  (flet ((quote-it (x)
                (list 'quote x)))
        (cons (car cmd) (mapcar #'quote-it (cdr cmd))))
        ))


(defun tweak-text (lst caps lit)
        (when lst
                (let ((item (car lst))
                (rest (cdr lst)))
        (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
              ((member item '(#\! #\? "\.)) (cons item (tweak-text rest t lit)))
              ((eql item #\") (tweak-text rest caps (not lit))
              (lit (cons item (tweak-text rest nillit)))
              (caps (cons (char-upcase item) (tewak-text rest nil lit)))
              (t (cons (char-downcase item) (tweak-text rest nil nil))))))))

(defun game-print (lst)
        (princ (coerce (tweak-text (coerce (string-trim "() "
                (prin1-to-string lst))
                'list)
                t
                nil)
        'string))
        (fresh-line))
