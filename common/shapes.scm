(use numbers)
(use extras)
(use glm)

(use srfi-1)
(use srfi-69)

(define (get-nodes edges)
  (sort 
    (let loop ((current '())
               (rest edges))
      (if (null? rest)
        current

        (loop (append (car rest) current)
              (cdr rest))))
    <)
  )

(define (make-icosahedron-vertices radius)
  (define (make-vert horiz vert)
    (vec3 (* radius (cos horiz) (cos vert))
          (* radius (sin horiz) (cos vert))
          (* radius (sin vert))))

  (let* ((dg (+ 1 (sqrt 5)))
        (PI (acos -1))
        (PI2 (* 2 PI))
        (pie 72)
        (phiaa 26.56505)

        (phia (* PI phiaa 1/180))
        (theb (* PI 36/180))
        (the72 (* PI 72/180))

        (offset 32)
        (vertices (list (vec3 0 0 radius))))
        
    (let loop ((horiz 0)
               (vert phia)
               (result vertices))

          (cond ((= (length result) 11)
                 (reverse (cons (vec3 0 0 (- radius)) result)))
                ((and (>= horiz PI2) (= vert phia))

                  (loop (+ theb the72)
                        (- phia)
                        (cons (make-vert theb (- phia)) result)))
                (else
                  (loop (+ horiz the72)
                        vert
                        (cons (make-vert horiz vert) result)))))))


(define (make-geometry radius)
  (let* ((vertices (make-icosahedron-vertices radius))
         (faces '((0 1 2)   (0 2 3)   (0 3 4)  (0 4 5)
                  (0 5 1)   (11 6 7)  (11 7 8) (11 8 9)
                  (11 9 10) (11 10 6) (1 2 6)  (2 3 7)
                  (3 4 8)   (4 5 9)   (5 1 10) (6 7 2)
                  (7 8 3)   (8 9 4)   (9 10 5) (10 6 1)))
         (map-point (lambda (point)
                      (list-ref vertices point)))
         (map-face (lambda (face)
                     (map map-point face))))
    (map map-face faces)))



(define (tessellate faces deep)
  (define buffer (make-hash-table))

  (define (maybe-add-point! v)
    (if (hash-table-exists? buffer v)
      #f
      (hash-table-set! buffer v (+ 0 (hash-table-size buffer)))))

  (define (split face)
    (let* ((v1 (car face))
           (v2 (cadr face))
           (v3 (caddr face))
           (v12 (normalize/vec3 (v+ v1 v2)))
           (v23 (normalize/vec3 (v+ v2 v3)))
           (v31 (normalize/vec3 (v+ v3 v1))))

     ; Add potentially new points to ou hashtable
     (maybe-add-point! v12)
     (maybe-add-point! v23)
     (maybe-add-point! v31)

     (list (list v1 v12 v31)
       (list v2 v23 v12)
       (list v3 v31 v23)
       (list v12 v23 v31))))

  (define (split-faces faces)
    (fold (lambda (face result)
            (append-reverse (split face) result))
          (list)
          faces))

  (for-each
    (lambda (face)
      ; Normalize our first faces
      (set-car! face (normalize/vec3 (car face)))
      (set-car! (cdr face) (normalize/vec3 (cadr face)))
      (set-car! (cddr face) (normalize/vec3 (caddr face)))

      ; Add them to our hashtable
      (maybe-add-point! (car face))
      (maybe-add-point! (cadr face))
      (maybe-add-point! (caddr face)))
    faces)

  (define newfaces 
    (let loop ((level deep)
               (faces faces))
      (if (= 0 level)
        faces
        (loop (- level 1) (split-faces faces)))))


  (list 
    (map (lambda (x) (car x))
         (sort (hash-table->alist buffer)
           (lambda (x y)
             (< (cdr x) (cdr y)))))
    (map (lambda (face)
           (list (hash-table-ref buffer (car face))
                 (hash-table-ref buffer (cadr face))
                 (hash-table-ref buffer (caddr face))))
         newfaces)))


(define (MakeTetraSphere recursion)
  (define v1 (vec3  1  0  (/ -1 (sqrt 2))))
  (define v2 (vec3 -1  0  (/ -1 (sqrt 2))))
  (define v3 (vec3  0  1  (/  1 (sqrt 2))))
  (define v4 (vec3  0 -1  (/  1 (sqrt 2))))

  (define faces (list (list v1 v2 v3)
                      (list v1 v3 v4)
                      (list v1 v4 v2)
                      (list v3 v2 v4)))
  (tessellate faces recursion))
