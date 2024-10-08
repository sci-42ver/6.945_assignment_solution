;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/REPL-Environment.html#index-ge-1
;; https://docs.scheme.org/schintro/schintro_122.html
;; > We say that a procedure is closed in the environment where it is created.
(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps02/code")
(load "load.scm")

;;; Problem 2.1 Warmup 

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
                   (lambda (name)
                     (case name
                       ((additive-identity) #f)
                       ((multiplicative-identity) #t)
                       (else (default-object))))
                   (lambda (operator)
                     (let ((procedure
                             (case operator
                               ((+) (lambda (x y) (or x y)))
                               ((-) (lambda (x) (not x)))
                               ((*) (lambda (x y) (and x y)))
                               ((negate) (lambda (x) (not x)))
                               (else
                                 (lambda args
                                   (error "Operator undefined in Boolean" operator))))))
                       (and procedure
                            (simple-operation operator boolean? procedure))))))

(install-arithmetic! boolean-arithmetic)

; (install-arithmetic! symbolic-arithmetic-1)

;;; Tests of 2.1:


(+ #t #f)
;Value: #t

(* #t #t)
;Value: #t

(* #t #f)
;Value: #f

(- #t)
;Value: #f

(- #f)
;Value: #t


;;; Problem 2.2
; (a)

(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps02/code")
(load "load.scm")

(register-predicate! vector? 'vector)

(define (ensure-vector-lengths-match vecs)
  ( let (( first-vec-length ( vector-length ( car vecs ))))
        ( if ( any ( lambda ( v)
                            ( not (n:= ( vector-length v)
                                       first-vec-length )))
                   vecs)
             (error "Vector dimension mismatch:" vecs))))

(define (vector-element-wise element-procedure)
  (lambda vecs
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

((vector-element-wise (lambda (x) (n:* x 2))) #(1 2 3))
;Value 153: #(2 4 6)

(define (sum l) (reduce + 0 l))

;; we can also use vector-element-wise
(define (v:+ vector1 vector2)
  (ensure-vector-lengths-match (list vector1 vector2))
  (list->vector
    (map sum (zip (vector->list vector1) (vector->list vector2)))))

(v:+ #(1 2 3) #(4 5 6))
;Value 181: #(5 7 9)

;; same as sci-42ver/SDF_exercise_solution
(define (v:* scalar vec) ((vector-element-wise (lambda (x) (n:* x scalar))) vec))

(v:* -1 #(1 2 3))
;Value 184: #(-1 -2 -3)

(define (v:negate vec) (v:* -1 vec))

(v:negate #(1 2 4))
;Value 193: #(-1 -2 -4)

(define (v:- vector1 vector2)
  (v:+ vector1 (v:negate vector2)))

(v:- #(1 2 5) #(3 4 6))
;Value 203: #(-2 -2 -1)

(define (v:dot vector1 vector2)
  (ensure-vector-lengths-match (list vector1 vector2))
  (let ((product (lambda (l) (reduce * 1 l))))
    (sum
      (map product (zip (vector->list vector1) (vector->list vector2))))))

(define (v:magnitude vector)
  (sqrt (v:dot vector vector)))

;; similar to symbolic-extender
(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vector? (list base-arithmetic)
                   (lambda (name base-constant)
                     base-constant)
                   (let ((base-predicate
                           (arithmetic-domain-predicate base-arithmetic)))
                     (lambda (operator base-operation)
                       (simple-operation
                         operator
                         vector?
                         (case operator
                           ;; IMHO using base-operation is more appropriate which may allows symbol. 
                           ((+) (lambda (x y) (v:+ x y)))
                           ((-) (lambda (x y) (v:- x y)))
                           ((*) (lambda (x y) (v:dot x y)))
                           ((negate) (lambda (x) (v:negate x)))
                           ((magnitude) (lambda (x) (v:magnitude x)))
                           (else
                             (lambda args
                               (error "Operator undefined in Vector" operator)))))))))

(define vector-arithmetic
  (extend-arithmetic vector-extender numeric-arithmetic))

(install-arithmetic! vector-arithmetic)

;(+ #(1 2) #(3 4))
;Value 366: #(4 6)

;(+ 1 2)
;Value: 3

;(- #(1 2 3) #(4 5 6))
;Value 239: #(-3 -3 -3)

;(- #(1 2 3))
;Value 240: #(-1 -2 -3)

(define symbolic-vector-arithmetic 
  (extend-arithmetic symbolic-extender vector-arithmetic))

(install-arithmetic! symbolic-vector-arithmetic)

;; Tests of symbolic vector arithmetic:

(+ 1 2)
;Value: 3

(+ #(1 2) #(3 4))
;Value 420: #(4 6)

(- #(1 2 3))
;Value 421: #(-1 -2 -3)

(- #(1 2 3) #(4 5 6))
;Value 422: #(-3 -3 -3)

(+ 'a 'b)
;Value 423: (+ a b)

(+ 'a 1)
;Value 424: (+ a 1)

(+ #(1 2 3) #(4 b 6))
;Value 427: #(5 (+ b 2) 9)

; 2.2 (c) Dot products

(* #(1 2 3) #(4 5 6))
;Value: 32

(* #(1 2 3) #(4 5 a))
;Value 604: (+ (* a 3) 14)

; 2.2(d) Vector magnitude

(magnitude #(1 1))
;Value: 1.4142135623730951





