; Author: Loïc Faure-Lacroix
; =============================================== 
; Disjoint Set using hashtable
; We can add find and do a union on our sets
; Add an element to the set with itself as parent
(use srfi-69)

(define (ds-add self item)
  (hash-table-set! self item item))

; Find the real parent of an item
; by default each item is the root of its own tree
; but after doing some unions it should point to an other 
; parent
(define (ds-find self item)
  (let ((parent (hash-table-ref self item)))
    (if (equal? parent item)
      (begin
        (hash-table-set! self item parent)
        parent)
      (ds-find self parent))))

; Change the parent of one of the two items passed as
; parameters
(define (ds-union self item1 item2)
  (hash-table-set! self item2 (hash-table-ref self item1)))
