(require-extension soil)

(define (keyboardFunc char x y)
  (if (equal? char #\esc)
    (begin
      (glut:LeaveGameMode)
      (exit)))
  (if (equal? char #\p)
     (save-screenshot "whoa.bmp" save-type/bmp 0 0 screen_width screen_height))
  (print char " " x " " y))
