(include "kruskal")
(include "prim")

(define (get-nodes edges)
  (apply append (map (lambda (v)
         (cdr v))
       edges))
  )

; Nodes are our points and edges are the distance
; between two nodes. 
(define edges '((7 "A" "B")
                (5 "A" "D")
                (8 "B" "C")
                (9 "B" "D")
                (7 "E" "B")
                (5 "C" "E")
                (15 "D" "E")
                (6 "D" "F")
                (8 "E" "F")
                (9 "E" "G")
                (11 "F" "G")))
(define nodes (get-nodes edges))

; Call the kruskal algorithm with the nodes and edges above
(print "Kruskal algo")
(time (print (kruskal nodes edges)))

; Call the kruskal algorithm with the nodes and edges above
(print "Prim algo")
(time (print (prim nodes edges)))

; (define nodes2 '(1 2 3 4))
(define edges2 '((1 1 2)
                 (1 1 3)
                 (1 2 4)
                 (1 1 4)
                 (1 3 4)))

; Call the kruskal algorithm with the nodes and edges above
(print "Prim algo")
(time (print (prim (get-nodes edges2) edges2)))

; (define nodes2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
(define edges2 '((10 1 5)
                 (11 2 6)
                 (31 3 7)
                 (12 4 5)
                 (41 5 6)
                 (1  6 7)
                 (31 7 8)
                 (21 9 10)
                 (31 10 11)
                 (41 11 12)
                 (51 12 13)
                 (51 5 10)
                 (51 6 11)
                 (51 7 12)
                 (51 14 15)
                 (51 15 16)
                 (51 16 17)
                 (51 17 18)
                 (51 10 15)
                 (51 11 16)
                 (51 12 17)
                 (51 15 19)
                 (51 16 20)
                 (15 17 21)))


; Call the kruskal algorithm with the nodes and edges above
(print "Prim algo")
(time (print (prim (get-nodes edges2) edges2)))
