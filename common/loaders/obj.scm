(require-extension srfi-4 extras srfi-13
                   list-utils)

(define (load-obj port)

  (define (to-numbers lst)
    (map string->number lst))

  ; Check for 1, 1/2, 1/2/3, 1//2
  ; Only use the vertex index because 
  ; I have no idea how to draw elements with different
  ; normal/texture indexes...
  (define (parse-vertex lst)
    (to-numbers
      (map (lambda (str)
        (car (string-split str "/" #t))) lst)))
      
  ; Simply convert string to numbers
  (define parse-normal to-numbers)

  ; Reduce by one since the indices start by 1 instead of 
  ; 0
  (define (parse-element lst)
    (map (lambda (x) (- x 1))
        (to-numbers
          (map (lambda (str)
            (car (string-split str "/" #t))) lst))))

  ; Parse texcoord, if w is missing we add 0 in case it matters
  ; while I believe I should just trim it to u, v instead of adding w
  ; which is meaningless in OpenGL as far as I know
  (define (parse-texcoord lst)
    (let ((uvs (to-numbers lst)))
      (if (= (length uvs) 2)
        (append uvs (list 0))
        uvs)))

  ; A recursive let to iteratively replace our parameters
  (let parse ((line (read-line port))
              (name "")
              (vertices (list))
              (texcoords (list))
              (normals (list))
              (elements (list)))

    (if (eof-object? line)

      ; Return a list of everything once done
      (list name
            (reverse vertices)
            (reverse texcoords)
            (reverse normals)
            (reverse elements))

      (let* ((tokens (string-tokenize line))
             (type (car tokens))
             (lst (cdr tokens)))

        ; Recursively loop returning the result of the recursion
        (parse (read-line port)
               (if (equal? type "o") (cadr tokens) name)
               (if (equal? type "v") (append-reverse (parse-vertex lst) vertices) vertices)
               (if (equal? type "vt") (append-reverse (parse-texcoord lst) texcoords) texcoords)
               (if (equal? type "vn") (append-reverse (parse-normal lst) normals) normals)
               (if (equal? type "f") (append-reverse (parse-element lst) elements) elements))))))
