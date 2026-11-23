;; ANCHOR: hy
;; Hy: a Lisp that becomes Python. A call is a call of Python, so the
;; limit of 1000 is there, too.
;; Hy：Python になる Lisp。呼び出しは Python の呼び出しなので、1000 の
;; 上限も、そのまま
(defn count-down [n total]
  (if (= n 0)
    total
    (count-down (- n 1) (+ total n))))
;; ANCHOR_END: hy

(assert (= (count-down 900 0) 405450))
(try
  (count-down 100000 0)
  (assert False)
  (except [RecursionError]
    (print "ok")))
