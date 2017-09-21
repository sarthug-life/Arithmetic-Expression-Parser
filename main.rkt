#lang racket
(provide pred-p single-digit-p single-alphabet-p seq alt zero-or-more one-or-more
         whitespace-p number-p identifier-p variable-p term-p assignment-p
         epsilon-p expression-p)

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

(define (pred-p p)
  (lambda (str)
          (if (p (string-ref str 0)) (cons (string-ref str 0) (substring str 1))
              'fail)))

(define single-digit-p
  (lambda (str)
  ((pred-p char-numeric?) str)))
(define single-alphabet-p
    (lambda (str)
  ((pred-p char-alphabetic?) str)))

(define (combine-cc char1 char2)
  (list->string (list char1 char2)))
(define (combine-sc str char)
  (list->string (append (string->list str)
                (list char))))
(define (combine-cs char str)
  (list->string (cons char (string->list str))))
(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))
(define (seq p1 p2 f)
  (lambda (str)
    (if (equal? (p1 str) 'fail) 'fail
    (let* [(a (p1 str))
           (b (p2 (cdr a)))]
           (cons (f (car a) (car b)) (cdr b))))))
                        
(define (alt p1 p2)
  (lambda (str)
    (if (not (equal? 'fail (p1 str))) (p1 str)
        (p2 str))))

(define epsilon-p
  (lambda (str)
    (cons "" str)))

(define (string-car str)
  (substring str 0 1))
(define (string-cdr str)
  (substring str 1))

(define (zero-or-more p f)
  (lambda (str)
    (define (parsing-till n str)
      (if (or (equal? "" str) (equal? 'fail (p str))) n
          (parsing-till (+ n 1) (string-cdr str))))
    (define n (parsing-till 0 str))
    (cons (substring str 0 n) (substring str n))))
(define (one-or-more p f)
  (lambda (str)
  (if (equal? 'fail (p str)) 'fail
      ((zero-or-more p f) str))))
    
(define whitespace-p
  (lambda (str)
    (define (space-eater str)
      (if (not (equal? " " (string-car str))) (cons "" str)
          (space-eater (string-cdr str))))
    (space-eater str)))

(define number-p
  (lambda (str)
    (let* [(a (whitespace-p str))
           (b ((one-or-more single-digit-p combine-cs) (cdr a)))]
      (if (equal? b 'fail) 'fail
           (cons (num (string->number (car b))) (cdr b))))))

(define identifier-p
  (lambda (str)
    (let* [(a (cdr (whitespace-p str)))
           (b (single-alphabet-p a))]
      (if (equal? b 'fail) 'fail
          (let* [(result ((one-or-more
                          (alt single-digit-p single-alphabet-p) combine-ss) a))]
            (cons (ident (car result)) (cdr result)))))))

(define (char-finder str n c)
  (cond [(equal? n (string-length str)) 'fail]
        [(equal? (string-ref str n) c) n]
        [ else (char-finder str (+ n 1) c)]))
(define (f str)
  (let* [(a (string->list str))]
    (list->string (g #\space a))))
(define (g c l)
  (if (not (in c l)) l
      (g c (remove c l))))
(define (in c l)
  (cond [(null? l) #f]
        [(equal? c (car l)) #t]
        [else (in c (cdr l))]))           

(define variable-p
  (lambda (str)
    (let* [(a (identifier-p str))]
      (if (equal? a 'fail) 'fail
          (let* [(rest (cdr a))]
      (cond [(equal? rest "") a]
            [(not (equal? (string-car rest) "[")) a]
            [else
             (let* [(m (char-finder rest 0 #\[ ))
                    (m-in (char-finder rest (+ m 1) #\[ ))
                    (current-n (char-finder rest 0 #\] ))]
               (if (or (equal? m-in 'fail) (> m-in current-n))
                   (let* [(n (char-finder rest 0 #\]))]
               (cons (gnode 'ARRAY (list (car a)
                                (car (expression-p (substring rest (+ m 1) n)))))
                  (substring rest (+ n 1))))

                   (let* [(n-in (char-finder rest 0 #\] ))
                          (n (char-finder rest (+ n-in 1) #\]))]
                     (cons (gnode 'ARRAY (list (car a)
                                         (car (expression-p (substring rest (+ m 1) n)))))
                           (substring rest (+ n 1))))))]))))))
(define term-p
  (lambda (str)
    (if (not (equal? (number-p str) 'fail)) (number-p str)
        (if (not (equal? (variable-p str) 'fail)) (variable-p str)
            (let* [(new-str (cdr (whitespace-p str)))
                   (m (char-finder new-str 0 #\( ))          
                   (m-in (char-finder new-str (+ m 1) #\( ))]
              (if (equal? m-in 'fail)
                  (let* [(n (char-finder new-str 0 #\) ))
                         (parsed (expression-p (substring new-str (+ m 1) n)))]
                   (cons (car parsed) (substring new-str (+ n 1))))
                  
                   (let* [(n-in (char-finder new-str 0 #\) ))
                          (n (char-finder new-str (+ n-in 1) #\) ))
                          (parsed (expression-p (substring new-str (+ m 1) n)))]
                   (cons (car parsed) (substring new-str (+ n 1))))))))))

(define expression-p
  (lambda (str)
    (let* [(a (term-p str))]
      (if (equal? (cdr a) "") a
          (let* [(b (cdr (whitespace-p (cdr a))))]
      (cond [(equal? (string-car b) "+")
               (cons (gnode 'PLUS (list (car a) (car (expression-p (cdr (whitespace-p (string-cdr b)))))))
                     (cdr (expression-p (string-cdr b))))]))))))
 (define assignment-p
   (lambda (str)
     (let* [(lhs (variable-p (f str)))]
       (if (not (equal? "=" (string-car (cdr (whitespace-p (cdr lhs)))))) 'fail
           (let* [(rhs (expression-p (cdr (whitespace-p
                                           (string-cdr (cdr (whitespace-p (cdr lhs))))))))]
             (cons (gnode 'ASSIGN (list (car lhs) (car rhs))) (cdr rhs)))))))

