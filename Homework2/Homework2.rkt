; Ehsan Hosseinzadeh Khaligh
; Credit: have used some of the DrRacket Library turorials and lecture note a referance.

;------------ Helpers -----------
; Used in E and F
; Concatinate all the inner lists
; (concat_sub_lists '(6 4 ((9 2) 8) 5)) -> (6 4 9 2 8 5)
(define (concat_sub_lists L)
  (cond ((null? L) '())
        ((pair? L)
         (append (concat_sub_lists (car L)) (concat_sub_lists (cdr L))))
        (else (list L))))

; Used in B and F
; Check if entered list is empty or not. if not call ListItrRepl
(define (ListCheck C L)
  (if (equal? (null? L) #f) (ListItrRepl C L) "Enterd List is empty"))

; Used in B and F
; Sum all the elemts of a list
; (mySum '(1 1 1 1 1))
(define (listSum L)
  (apply + L))

;------- A -------------------
; My own referance:
; (if (equal? (isEven2 (modTen x))  #t)  "Even" "Odd" )
; (if (equal? (= (/ x 10) 0)  #f) "Hi" "bye")
; (if (equal? (lastDigit 2)  #f)  "MoreDigit" "LastDigit" ) output Lastdigit 

; (isEven2 3) #f
(define (isEven2 x)
  (equal? (modulo x 2) 0))

; get right most digit 
(define (modTen x)
  (modulo x 10))

; divide by 10 to reduce the number of digits  
(define (divByTen x)
  (floor (/ x 10)))

; return True if the number is last didgit
; (lastDigit 2) #t  (lastDigit 22)  #f
(define (lastDigit x)
  (equal? (floor (/ x 10)) 0))

; if the most recent digit is odd return false (odd). if number of digits more than one recursively call allEven
; and divByTen to reduce number of digits. If we have one digit left know that it the number is even.
; (allEven 22333)  #f   (allEven 22222) #t
(define (allEven x)
    (if  (equal? (isEven2 (modTen x))  #f) #f
                                          (if (equal? (lastDigit x)  #f) (allEven (divByTen x))  #t )  ))

; if the digit it returns true, the number is even and we dont want.
(define (allOdd x)
    (if  (equal? (isEven2 (modTen x))  #t) #f
                                          (if (equal? (lastDigit x)  #f) (allOdd (divByTen x))  #t )  ))

; Test case: > (ludicrous 22222444) #t, (ludicrous 2333) #f, (ludicrous 3333355), #t
(define (ludicrous x)
   (cond ((equal? (allOdd x)  #t) #t)
         ((equal? (allEven x) #t) #t)
         (else #f)))

;----------- B --------------------
; Recursively Iterate through  the list and replace elements greator than C with C
; (ListItrRepl 4 '(1 2 5 6 8 9 1 9)) -> (1 2 4 4 4 4 1 4)
(define (ListItrRepl C L)
  (if (null?  L)  L
                    (cons (if (> (car L) C) C (car L)) (ListItrRepl C (cdr L))) ))


; (cappedSum 7 '(6 4 9 2 8)) should return 26, because 6 + 4 + 7 + 2 + 7 = 26 
(define (cappedSum C L)
  (listSum (ListCheck C L)))

;----------- C --------------------
;Remove last element of list
(define (remove_last_element L)
    (if (null? (cdr L))
        '()
        (cons (car L) (remove_last_element (cdr L)))))

; (reverseShuffle_subList '(3 8 2 6) '(7 1 5 9)) -> ((3 9) (8 5) (2 1) (6 7))
(define (reverseShuffle_subList L M)
  (if  (and (null? L) (null? M)) '()
       (cons (list (car L) (list-ref M (- (length  M) 1))) (reverseShuffle_subList (cdr L) (remove_last_element M) )))   )

;(reverseShuffle '(3 8 2 6) '(7 1 5 9)) -> (3 9 8 5 2 1 6 7)
(define (reverseShuffle L M)
  (if  (and (null? L) (null? M)) '()
                   (concat_sub_lists (reverseShuffle_subList L M)) ))

;-----------  D --------------------
;My own referance:
;(length (car '(() (3 4 7) (2 1) (5 5 2) (8))))
;(length (car(cdr '(() (3 4 7) (2 1) (5 5 2) (8)))))

; Recursively get the length of each sublist and return a new list with the length of each sublists.
(define (subLst_len  M)
   (if  (null? M) '()
       (cons (length (car M)) (subLst_len (cdr M)) )))

; Sort the list of sublist length and car the first element(largest one)
; (biggestListSize '(() (3 4 7) (2 1) (5 5 2) (8))) -> 3
(define (biggestListSize M)
  (car (sort (subLst_len  M) >)))

;----------- E --------------------
(define (isEven x)
  (if (= (modulo x 2) 0) #t #f))

(define (square x)
  (* x x))

; list-ref accesses an element by passing index number
; Append empty list instead of void. Concatination in the next step to remove '().
(define (comparison F L)
  (if (= (length L) 1) '()
      (cons (if (equal? (F (list-ref  L 0))  (F (list-ref  L 1) )) (car L) '()) (functionPairs F (cdr L)))))

; The output is a list consisting of each ith element in L for which F(L[i])=F(L[i+1])
; (functionPairs square '(-2 2 4 5 -5)) -> (-2 5)
; (functionPairs isEven '(3 7 5 2 8)) -> (3 7 2)
(define (functionPairs F L)
  (if (null? L) "List null"
                 (concat_sub_lists (comparison F L))  ))
  
;----------- F --------------------
;(nestedCappedSum 7 '(6 4 ((9 2) 8) 5)) -> 31
(define (nestedCappedSum C L)
  (listSum (ListCheck C (concat_sub_lists L))))

;----------- G --------------------
; Exploring -> ((number + x) * y) ^ z
; x first element, y second element, z last element of list

; We assume the list length always is 3.
(define (makeExploder L)
  (define (exploder N) (if (= (length L) 3)
                           (expt  (* (+ N (list-ref  L 0)) (list-ref  L 1))  (list-ref  L 2)) 
                           "List Length != 3"))
  exploder
 )

; (P 4) -> 27000 , (P 5) -> 42875 ,
; if the the makeExploder list is less than/greator than length 3: (P 5) -> "List Length != 3"
;(define P (makeExploder '(2 5 3)))