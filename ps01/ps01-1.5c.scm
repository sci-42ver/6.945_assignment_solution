; (load "~/Dropbox (MIT)/Classes/6.945/ps01/regexp.scm") ; change this to reflect the directory you are running this from!
; (load "~/SICP_SDF/SDF_exercises/software/sdf/regular-expressions/regexp.scm")
(load "regexp.scm") ; with compatible r:grep

; Problem 1.5 (c)

; This code relies on some custom modifications I made to regexp.scm to allow running with grep vs. egrep

;; only adds standard arg.
(define (r:dot standard) ".")
(define (r:bol standard) "^")
(define (r:eol standard) "$")

; Quotes a character if needed, according to the standard (BRE or ERE)
;; capture ordinary character. Same as chebert.
(define (quote-char standard char)
  (case char
    ;; `echo "$" | grep -E '\$' -` works.
    ; ((#\. #\[ #\] #\* #\^) (list #\\ char))
    ; ((#\$) (list #\[ #\$ #\]))
    ((#\. #\[ #\] #\* #\^ #\$) (list #\\ char))
    ((#\( #\) #\? #\+ #\{ #\})
     (if (eq? standard 'bre)
       char
       (list #\\ char)))
    (else (list char))))

(define (r:quote standard string)
  (list->string
    (append-map (lambda (char)
                  (quote-char standard char))
                (string->list string))))

;; only adds standard arg.
(define (r:char-from standard string)
  (case (string-length string)
    ((0) (r:seq standard))
    ((1) (r:quote standard string))
    (else
      (bracket string
               (lambda (members)
                 (if (lset= eqv? '(#\- #\^) members)
                   '(#\- #\^)
                   (quote-bracketed-contents members)))))))

;; only adds standard arg.
(define (r:char-not-from standard string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

;; same as code base
(define (bracket string procedure)
  (list->string
    (append '(#\[)
            (procedure (string->list string))
            '(#\]))))

;; same as code base
(define (quote-bracketed-contents members)
  (let ((optional
          (lambda (char) (if (memv char members) (list char) '()))))
    (append (optional #\])
            (remove (lambda (c)
                      (memv c chars-needing-quoting-in-brackets))
                    members)
            (optional #\^)
            (optional #\-))))


;;; Means of combination for patterns

;; addition based on code base.
(define (make-special standard char)
  (list->string
    (case char
      ;; | is assumed to be supported by BRE.
      ((#\( #\) #\{ #\} #\|) (case standard
                               ('bre (list #\\ char))
                               ('ere (list char))))
      (else (list char)))))

;; only adds standard arg.
(define (r:seq . (standard . exprs))
  (string-append 
    (make-special standard #\()
    (apply string-append exprs)
    (make-special standard #\))))

(define parenthesize r:seq)

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.

;; only adds standard arg.
(define (r:alt . (standard . exprs))
  (if (pair? exprs)
    (apply r:seq
           (cons (car exprs)
                 (append-map (lambda (expr)
                               (list (make-special standard #\|)
                                     expr))
                             (cdr exprs))))
    (r:seq)))

;; See 2.8: beginning
(define (parenthesized? standard expr)
  (case standard
    ('ere
     (and (string=? "(" (string-head expr 1))
          (string=? ")" (string-tail expr (- (string-length expr) 1)))))
    ('bre
     (and (string=? "\\(" (string-head expr 2))
          (string=? "\\)" (string-tail expr (- (string-length expr) 2)))))))

(define (bracketed? standard expr)
  (and (string=? "[" (string-head expr 1))
       (string=? "]" (string-tail expr (- (string-length expr) 1)))))

(define (parenthesize-if-needed standard expr)
  (if (or
        (= (string-length expr) 1)
        (parenthesized? standard expr)
        (bracketed? standard expr))
    expr
    (parenthesize standard expr)))

(define (r:repeat standard min max expr)
  (string-append
    (parenthesize-if-needed standard expr)
    (make-special standard #\{)
    (number->string min)
    ","
    (if max
      (number->string max)
      "")
    (make-special standard #\})))
;; See 2.8: ending

;; See 2.6: beginning
(define (r:* standard expr)
  (r:repeat standard 0 #f expr))

(define (r:+ standard expr)
  (r:repeat standard 1 #f expr))
;; See 2.6: ending

;; ;; only adds standard arg.
(define (r:back-ref standard n)
  (string-append "\\" (number->string n)))

; Test
(define bre 'bre)
(define ere 'ere)
(define std bre)

(r:seq std
       (r:quote std "a") (r:dot std) (r:quote std "c") 
       (r:seq std (r:* std (r:quote std "xy")))
       (r:back-ref std 1))

; this appears to return correct regular expressions. Just annoying that you have to keep passing in `std'
; Below, I define a function that adds in the `std' argument everywhere it is needed, so that a user can just deal
; with code that doesn't have any ERE vs. BRE business, and then compile it later using `(add-args code standard)'
; where they will pass in their code and an argument indicating ERE vs. BRE.

(define (add-args code std)
  (cond
    ((eq? code '()) '())
    ((list? code)
     (if (memv (car code) '(r:+ r:* r:repeat r:char-not-from r:char-from r:quote r:seq r:alt r:back-ref r:dot r:bol r:eol))
       (append (list (car code) std)
               (map (lambda (expr) (add-args expr std))
                    (cdr code)))
       (cons (add-args (car code) std) ; Here list is thought as one minimal procedure. So we should not append somthing after it.
              (map (lambda (expr) (add-args expr std))
                  (cdr code)))))
    (else code)))

(define code
  '(r:seq 
     (r:quote  "a") (r:dot ) (r:quote  "c") 
     (r:seq  (r:*  (r:quote  "xy")))))


(eval (add-args code bre) (the-environment))
;Value 65: "\\(a.c\\(\\(xy\\)\\{0,\\}\\)\\)"

(eval (add-args code ere) (the-environment))
;Value 66: "(a.c((xy){0,}))"

(define expr1 (eval (add-args code bre) (the-environment)))
(define expr2 (eval (add-args code ere) (the-environment)))

(define filename "test.txt")

(r:grep bre expr1 filename)
;Value 69: ("abc" "a$c" "abcxyxy" "abcxy")

(r:grep ere expr2 filename)
;Value 70: ("abc" "a$c" "abcxyxy" "abcxy")

; et voilà! QED. Compiling a general expression into either ERE or BRE, which yield different syntaxes, indeed return the same result when run through grep vs egrep.
