(require-extension gl glu glut)
(require-extension srfi-19-core)
(include "common/maze")


(define folder (if (equal? (command-line-arguments) '())
                 "geo"
                 (car (command-line-arguments))))

(require (string-append folder "/" "setup"))
(require "inputs")
(require (string-append folder "/" "renderer"))

(let ((engine (makeEngine)))
  (glut:DisplayFunc renderFunc)
  (glut:KeyboardFunc keyboardFunc)
  (glut:ReshapeFunc reshapeFunc)
  (glut:IdleFunc idleFunc))

(glut:MainLoop)
