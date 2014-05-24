; Author: Loïc Faure-Lacroix
(use extras)
(use srfi-69)

(include "disjoint-set")
(include "list-util")

; Kruskal algorithm is done here
(define (kruskal nodes edges)
  ; Loop that will check each element in the graph and add them
  ; or not to the shortest path
  (define (kruskal-forest forest result edges)
    ; Get both node of the current edge
    (let* ((item (car edges))
           (t1 (ds-find forest (list-ref item 1)))
           (t2 (ds-find forest (list-ref item 2))))

      (if (null? (cdr edges))
        ; If there is no more edges we return the result set
        result
        (if (equal? t1 t2)
          ; We cannot add item to our result set if both
          ; nodes are in the same tree... 
          ;
          ; skip to the next edge if both nodes are in the tree
          (kruskal-forest forest result (cdr edges))
          ; or append item to the set otherwise and move to the next
          ; edge
          (begin
            (ds-union forest t1 t2)
            (kruskal-forest forest (append result (list item)) (cdr edges)))))))

  ; Sort our edges by increasing weight
  (set! edges (sort-nodes edges))

  (let ((forest (make-hash-table)) (mst (list)))
    ; Fill our DisjointSet with all nodes. Each
    ; nodes become an independent tree pointing to itself
    ; and at the end we should have only one tree
    (fill-forest forest nodes)
    ; Loop over all edges to be added to the result set and return
    ; when there is no more edges to watch
    (kruskal-forest forest mst edges)))
