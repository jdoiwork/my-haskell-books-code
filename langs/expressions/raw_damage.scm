; Raw damage, for the chapter "Expressions, types and functions".
; 素のダメージ。「式・型・関数」の章

(define (check ok)
  (if (not ok) (error "wrong result")))

; ANCHOR: define
; Scheme (a member of the Lisp family)
; Scheme（Lisp の仲間）
(define (raw-damage attack defense)
  (- attack defense))
; ANCHOR_END: define

(check (= (raw-damage 30 12) 18))

; The same name again, written with lambda.
; 同じ名前を、lambda で定義し直す
; ANCHOR: lambda
; Scheme (a member of the Lisp family)
; Scheme（Lisp の仲間）
(define raw-damage
  (lambda (attack defense)
    (- attack defense)))
; ANCHOR_END: lambda

(check (= (raw-damage 30 12) 18))
(display "ok")
(newline)
