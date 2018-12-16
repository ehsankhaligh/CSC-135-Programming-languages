;------------- A ---------------

(define (ludicrous x)
  (or (allDigits isEven x) (allDigits isOdd x)))

(define (allDigits f x)
  (if (< x 10) (f (modulo x 10))
    (and (f (modulo x 10)) (allDigits f (quotient x 10)))))

(define (isEven x) (= 0 (modulo x 2)))
(define (isOdd x) (not (= 0 (modulo x 2))))

;------------- B --------------

(define (cappedSum x L)
  (if (null? (cdr L)) (capped x (car L))
                      (+ (capped x (car L)) (cappedSum x (cdr L)))))

(define (capped x n) (if (< x n) x n))

;------------- C --------------

(define (reverseShuffle L M)
  (forwardShuffle L (reverseList M)))

(define (reverseList L)
  (if (null? L) '()
                (appendList (car L) (reverseList (cdr L)))))

(define (appendList x L)
  (if (null? L) (cons x '())
                (cons (car L) (appendList x (cdr L)))))

(define (forwardShuffle L M)
  (if (null? L) '()
          (cons (car L) (cons (car M) (forwardShuffle (cdr L) (cdr M))))))

;-------------- D --------------

(define (biggestListSize L)
  (if (null? (cdr L)) (listSize (car L))
     (if (> (listSize (car L)) (biggestListSize (cdr L)))
        (listSize (car L))
        (biggestListSize (cdr L)))))

(define (listSize L)
  (if (null? L) 0
                (+ 1 (listSize (cdr L)))))

;------------- E ----------------

(define (functionPairs f L)
  (if (null? L) '()
    (if (null? (cdr L)) '()
      (if (equal? (f (car L)) (f (car (cdr L))))
        (cons (car L) (functionPairs f (cdr L)))
        (functionPairs f (cdr L))))))

;------------- F ----------------

(define (nestedCappedSum2 x L)
  (if (null? (cdr L))
    (if (list? (car L))
      (nestedCappedSum2 x (car L))
      (capped x (car L)))
    (if (list? (car L))
      (+ (nestedCappedSum2 x (car L)) (nestedCappedSum2 x (cdr L)))
      (+ (capped x (car L)) (nestedCappedSum2 x (cdr L))))))
	  
;------------- G ----------------

(define (makeExploder L)
  (define (E n) (expt (* (+ n (car L)) (car (cdr L))) (car (cdr (cdr L)))))
  E
)

