;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |#bst|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; bst struct with a key, value and two other bst structs left and right
; is either empty or a (make-bst k v l r) where
; k = key, a number
; v = value, a string
; left = a bst struct 
; right = a bst struct 
(define bst (lambda (key value left right) (list key value left right)))
(define my-fam (bst 52 "Beth" (bst 22 "Cora" (bst 19 "Abby" '() '()) '()) (bst 72 "Robert" '() '())))

;accesssing fields in a bst  
(define bst-k (lambda (bst) (first bst)))
(define bst-v (lambda (bst) (first (rest bst))))
(define bst-l (lambda (bst) (first (rest (rest bst)))))
(define bst-r (lambda (bst) (first (rest (rest (rest bst))))))

;inserting a key and value into a bst
(define bst-insert
  (lambda (tree key value)
    (cond
      [(null? tree) (bst key value '() '())] ;create a new node if empty
      [(< key (bst-k tree))
       (bst (bst-k tree) (bst-v tree) (bst-insert (bst-l tree) key value) (bst-r tree))]
      [(> key (bst-k tree))
       (bst (bst-k tree) (bst-v tree) (bst-l tree) (bst-insert (bst-r tree) key value))]
      [(= key (bst-k tree))
       (bst (bst-k tree) value (bst-l tree) (bst-r tree))]
      [else tree])))

;test
;(bst-insert my-fam 44 "Lara")

;returning a value given a key in a bst and the number of bsts it traversed to find it
(define bst-lookup
  (lambda (t key depth)
    (cond
      [(null? t) '()] ;return the empty tree
      [(< key (bst-k t)) (bst-lookup (bst-l t) key (+ 1 depth))]
      [(> key (bst-k t)) (bst-lookup (bst-r t) key (+ 1 depth))]
      [(= key (bst-k t)) (string-append (bst-v t) ", number of BSTs passed through: " (number->string depth))] 
      [else "key not found in bst"])))

;test
;(bst-lookup my-fam 19 0)

;go through list of values and keys
(define kees (list 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 12 11 10 9 46 47 48 8 7 55 6 5 4 3))
(define vals (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "AA" "BB" "CC" "DD" "EE" "FF"))

;creates a 32 element bst
(define big-bst
  (lambda (tree l1 l2)
    (cond
      [(null? l1) tree]
      [(null? l2) tree]
      [(list? l1) (big-bst (bst-insert tree (first l1) (first l2)) (rest l1) (rest l2))])))

;test
(define big1 (big-bst '() kees vals))
(bst-lookup big1 32 0)


