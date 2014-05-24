(use srfi-69)

(include "base")
(include "prim")
(include "kruskal")


(define (for-eachi func lst)
  (let loop ((index 0)
             (current lst))
    (if (null? current)
      #t
      (begin 
        (func (sort (car current) <) index)
        (loop (add1 index)
              (cdr current))
        ))))

(define (getEdges faces)
  (define forest (make-hash-table))
  (define edges (make-hash-table))
  (define sedges (list))

  (for-eachi (lambda (val index)
     (for-each (lambda (edge)
       (if (hash-table-exists? forest edge)
         (let ((new-edge 
                (sort (list index (hash-table-ref forest edge)) <)))
           (set! sedges (cons new-edge sedges))
           (hash-table-set! edges new-edge edge)
         )
         (hash-table-set! forest edge index)))
       (list 
        (list (car val) (cadr val))
        (list (cadr val) (caddr val))
        (list (car val) (caddr val)))))
    faces)
  
    (list edges sedges)
    )

(time 

(let* ((geo (tessellate (make-geometry 1) 0))
       (vertices (car geo))
       (forest (getEdges (cadr geo)))
       (backup (car forest))
       (edges (cadr forest))
       (size (length edges))
       (m-edges (map (lambda (x) (cons (random size) x)) edges))
       (nodes (get-nodes m-edges))
       (path (kruskal nodes m-edges))
       (filtered-path (map (lambda (x) (cdr x)) path))
       (maze (subtract edges filtered-path)))

  (print "Edges : " (length edges))
  (print "Maze : "  (length maze))
  ; (print "Vertices : " vertices)

  (for-each (lambda (maze-edge)
              (let* ((base (hash-table-ref backup maze-edge))
                     (v1 (car base))
                     (v2 (cadr base))
                     (vect1 (list-ref vertices v1))
                     (vect2 (list-ref vertices v2)))
                (print vect1 " - " vect2)
                (cons vect1 vect2)
              ))
            maze)

  ;(print "Nodes : " nodes)
  ;(print "Edges : " m-edges)
  ;(print "Path : " filtered-path)
  ;(print "Maze : " maze)
))
