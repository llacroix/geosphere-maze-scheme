; Code depend on the disjoin set
(include "common/disjoint-set")

; Sort edges in increasing order
; the first element of each edges should be the 
; weight of the edge
; (5 "A" "B") is an edge of weight 5 connecting
; the node A and B
(define (sort-nodes nodes)
  (sort nodes (lambda (x y)
    (< (car x) (car y)))))

; Fill the forest of nodes in our DisjointSet
;
; Each node added to the tree points to itself
;
(define (fill-forest forest nodes)
  (for-each (lambda (node)
    (ds-add forest node)) nodes))


; Check if the object is inside a list
(define (element? x lst)
  (cond ((null? lst) #f)
        ((equal? x (car lst)) #t)
        (else (element? x (cdr lst)))))

; Subtract a list from an other list
(define (subtract a b)
  (cond ((null? a) '())
        ((element? (car a) b)
         (subtract (cdr a) b))
        (else (cons (car a) (subtract (cdr a) b)))))
