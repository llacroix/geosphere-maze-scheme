(require-extension soil)

(define (keyboardFunc char x y)
  (if (equal? char #\esc)
    (begin
      (glut:LeaveGameMode)
      (exit)))

  (if (equal? char #\a)
    (set! angle_y (- angle_y 2.5)))

  (if (equal? char #\d)
    (set! angle_y (+ angle_y 2.5)))

  (if (equal? char #\s)
    (set! angle_x (- angle_x 2.5)))

  (if (equal? char #\w)
    (set! angle_x (+ angle_x 2.5)))

  (if (equal? char #\t)
    (begin 
      (set! tessellation (+ tessellation 1))
      (generate tessellation)))

  (if (equal? char #\g)
    (if (> tessellation 0)
        (begin
          (set! tessellation (- tessellation 1))
          (generate tessellation))))

  (if (equal? char #\p)
     (save-screenshot "whoa.bmp" save-type/bmp 0 0 screen_width screen_height))
  (print char " " x " " y))
