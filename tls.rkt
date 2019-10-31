#lang racket

;  definitions for use with The Little Schemer


(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (λ (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define nonlat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) #f)
      (else (nonlat? (cdr l))))))

(define member?
  (λ (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define member-twice?
  (λ (a lat)
    (cond
      ((null? lat) #f)
      (else (or (and (eq? (car lat) a) (member? a (cdr lat)))
                (member-twice? a (cdr lat)))))))

            

(define member-cake?
  (λ (lat)
    (member? 'cake lat)))

(define rember
  (λ (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (λ (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (λ (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (λ (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (λ (new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat)))))))

;(define subst2
;  (λ (new o1 o2 lat)
;    (cond
;      ((null? lat) '())
;      ((eq? (car lat) o1) (cons new (cdr lat)))
;      ((eq? (car lat) o2) (cons new (cdr lat)))
;      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define subst2
  (λ (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (λ (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))


(define multiinsertR
  (λ (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multisubst
  (λ (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))


(define append
  (λ (xs ys)
    (cond
      ((null? xs) ys)
      (else (cons (car xs) (append (cdr xs) ys))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(define o+
  (λ (x y)
    (cond
      ((zero? y) x)
      (else (add1 (o+ x (sub1 y)))))))

(define o-
  (λ (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (o- x (sub1 y)))))))

(define addtup
  (λ (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define ox
  (λ (x y)
    (cond
      ((zero? y) 0)
      (else (o+ x (ox x (sub1 y)))))))

(define tup+
  (λ (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (λ (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (λ (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (λ (n m)
    (and (not (o> n m)) (not (o< n m)))))

(define o^
  (λ (n m)
    (cond
      ((zero? m) 1)
      (else (ox n (o^ n (sub1 m)))))))

(define o/
  (λ (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

(define length
  (λ (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(define eqan?
  (λ (x y)
    (cond
      ((and (number? x) (number? y)) (o= x y))
      ((or (number? x) (number? y)) #f)
      (else (eq? x y)))))

(define occur
  (λ (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define one?
  (λ (n)
    (o= n 1)))

(define rempick2
  (λ (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


(define rember*
  (λ (a l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      (else
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l)))))))))

(define insertR*
  (λ (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (λ (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
       (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (λ (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (λ (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (λ (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (λ (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (λ (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist2?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist2? (cdr l1) (cdr l2)))))))

; there follows rember after we replace lat by a list l
; of S-expressions and a by any S-expression
(define rember2
  (λ (s l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((equal? (car l) s) (cdr l))
         (else (cons (car l) (rember2 s (cdr l))))))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l) (rember2 s (cdr l)))))))))

; now to try to simplify this
(define rember3
  (λ (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember3 s (cdr l)))))))

;(define numbered?
;  (λ (aexp)
;    (cond
;      ((atom? aexp) (number? aexp))
;      ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
;      ((eq? (car (cdr aexp)) 'x) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
;      ((eq? (car (cdr aexp)) '^) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
;      (else #f))))

; now to simplify numbered?
(define numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((or (eq? (car (cdr aexp)) '+)
           (eq? (car (cdr aexp)) 'x)
           (eq? (car (cdr aexp)) '^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      (else #f))))

(define value
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x) (ox (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else (o^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define valueRP
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+) (o+ (valueRP (car (cdr nexp))) (valueRP (car (cdr (cdr nexp))))))
      ((eq? (car nexp) 'x) (ox (valueRP (car (cdr nexp))) (valueRP (car (cdr (cdr nexp))))))
      (else (o^ (valueRP (car (cdr nexp))) (valueRP (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (λ (aexp)
    (car aexp)))

(define valueNew
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (o+ (valueNew (1st-sub-exp nexp)) (valueNew (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (ox (valueNew (1st-sub-exp nexp)) (valueNew (2nd-sub-exp nexp))))
      (else (o^ (valueNew (1st-sub-exp nexp)) (valueNew (2nd-sub-exp nexp)))))))


; if we represent numbers another way
; () is zero
; (()) is one
; (()()) is two
; (()()()) is three and so on...

(define sero?
  (λ (n)
    (null? n)))

(define edd1
  (λ (n)
    (cons '() n)))

(define zub1
  (λ (n)
    (cdr n)))

(define u+
  (λ (x y)
    (cond
      ((sero? y) x)
      (else (edd1 (u+ x (zub1 y)))))))

; Chapter 7.
; Friends and Relations

(define set?
  (λ (lat)
    (cond
      ((null? lat) #t)
      (else (and (not (member? (car lat) (cdr lat))) (set? (cdr lat)))))))

(define makeset
  (λ (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (λ (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (λ (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (λ (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (λ (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (λ (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (λ (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (λ (sets)
    (cond
      ((null? (cdr sets)) (car sets))
      (else (intersect (car sets) (intersectall (cdr sets)))))))

(define a-pair?
  (λ (l)
    (cond
      ((or (atom? l) (null? l) (null? (cdr l))) #f)
      (else (null? (cdr (cdr l)))))))

(define first
  (λ (p)
    (car p)))

(define second
  (λ (p)
    (car (cdr p))))

(define third
  (λ (p)
    (car (cdr (cdr p)))))

(define build
  (λ (s1 s2)
    (cons s1 (cons s2 '()))))

(define rel?
  (λ (s)
    (cond
      ((null? s) #t)
      (else (and (a-pair? (car s)) (rel? (cdr s)))))))

(define fun?
  (λ (rel)
    (set? (firsts rel))))

(define revrel
  (λ (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel)))
                  (revrel (cdr rel)))))))
(define revpair
  (λ (pair)
    (build (second pair) (first pair))))

(define revrel2
  (λ (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel2 (cdr rel)))))))

(define seconds
  (λ (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel)) (seconds (cdr rel)))))))


(define fullfun?
  (λ (fun)
    (set? (seconds fun))))

; ASIDE
; Trying to create function to test for anagrmas
(define anagram-helper
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((member? (car l1) l2) (anagram-helper (cdr l1) (rember (car l1) l2)))
      (else #f))))

(define anagram?
  (λ (str1 str2)
    (let ((l1 (string->list str1)) (l2 (string->list str2)))
      (anagram-helper l1 l2))))

; Chapter 8
; Lambda the ultimate

;(define rember-f
;  (λ (test? a l)
;    (cond
;      ((null? l) '())
;      ((test? a (car l)) (cdr l))
;      (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (λ (a)
    (λ (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(define rember-f
  (λ (test?)
    (λ (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

;(define insert-g
;  (λ (test? direc)
;    (λ (new old l)
;      (cond
;        ((null? l) '())
;        ((and (eq? direc "l") (test? old (car l))) (cons new (cons old (cdr l))))
;        ((and (eq? direc "r") (test? old (car l))) (cons old (cons new (cdr l))))
;        (else (cons (car l) ((insert-g test? direc) new old (cdr l))))))))

(define seqL
  (λ (new old l)
    (cons new (cons old l))))

(define seqR
  (λ (new old l)
    (cons old (cons new l))))

(define insert-g
  (λ (seq)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define test1 (insert-g seqR))
(define test2 (insert-g seqL))
(define test3 (insert-g (λ (new old l) (cons new (cons old l)))))

(define seqS
  (λ (new old l)
    (cons new l)))

(define new-subst (insert-g seqS))
   
(define seqrem (λ (new old l) l))

(define yyy
  (λ (a l)
    ((insert-g seqrem) #f a l)))

(define atom-to-function
  (λ (x)
    (cond
      ((eq? x (quote o+)) o+)
      ((eq? x (quote ox)) ox)
      (else o^))))

(define new-value
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (new-value (1st-sub-exp nexp))
             (new-value (2nd-sub-exp nexp)))))))

(define multirember-f
  (λ (test?)
    (λ (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) ((multirember-f test?) a (cdr l)))
        (else (cons (car l) ((multirember-f test?) a (cdr l))))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (λ (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

; don't yet know WTF the below is!
(define multirember&co
  (λ (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (λ (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat)
                            (λ (newlat seen)
                              (col (cons (car lat) newlat) seen)))))))
; I think this goes through a lat and looks for a
; and it seems to have a collector, which constructs 2 lists
; first is basically original lat with all a removed
; so like normal multirember, then the 2nd list is all occurences of
; a
; so I'll try to run the following
; (multirember&co 'tuna '(shrimp salad and tuna salad and tuna))
; and I think it will return '((shrimp salad and salad and) (tuna tuna))
; No that didn't work
; But the following does
; (multirember&co 'tuna '(shrimp salad and tuna salad and tuna) cons)
; returns
;'((shrimp salad and salad and) tuna tuna)

(define a-friend
  (λ (x y)
    (null? y)))

; So for multirember&co the third arg col
; is short for collector but it's a function

(define new-friend
  (λ (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

; so what (multirember&co a lat f) does is
; it looks at every atom of the lat to see whether
; it is eq? to a. Those atoms that are not collect in
; one list ls1; the others for which the answer is
; #t are collected in a second list ls2. Finally, it
; determines the value of (f ls1 ls2).

(define mrcoll
  (λ (ls1 ls2)
    (cond
      ((null? ls2) (cons ls1 (cons (cons 0 '()) '())))
      (else (cons ls1 (cons (cons (car ls2) (cons (length ls2) '())) '()))))))

(define multiinsertLR
  (λ (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (λ (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (λ (newlat L R)
                                                 (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat)
                                                                   (λ (newlat L R)
                                                                     (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (λ (newlat L R)
                                (col (cons (car lat) newlat) L R)))))))

(define testf
  (λ (newlat L R)
    (cons 'left (cons L (cons 'right (cons R newlat))))))

(define even?
  (λ (n)
    (o= (ox (o/ n 2) 2) n)))

(define evens-only*
  (λ (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
       (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co
  (λ (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (λ (newl p s)
                            (col (cons (car l) newl) (ox (car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (λ (newl p s)
                                 (col newl p (o+ (car l) s)))))))
       (else (evens-only*&co (car l)
                             (λ (al ap as)
                               (evens-only*&co (cdr l)
                                               (λ (dl dp ds)
                                                 (col (cons al dl) (ox ap dp) (o+ as ds))))))))))

(define the-last-friend
  (λ (newl product sum)
    (cons sum (cons product newl))))

; Chapter 9
; ... and Again, and Again, and Again ...

(define keep-looking
  (λ (a p lat)
    (cond
      ((number? p) (keep-looking a (pick p lat) lat))
      (else (eq? a p)))))

(define looking
  (λ (a lat)
    (keep-looking a (pick 1 lat) lat)))

;(define shift
;  (λ (pair)
;    (cons (car (car pair)) (cons (cons (car (cdr (car pair))) (cdr pair)) '()))))

(define shift
  (λ (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))

(define align
  (λ (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

;(define length*
;  (λ (pora)
;    (cond
;      ((null? pora) 0)
;      ((atom? (car pora)) (add1 (length* (cdr pora))))
;      (else (o+ (length* (car pora)) (length* (cdr pora)))))))

(define length*
  (λ (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (length* (first pora)) (length* (second pora)))))))
