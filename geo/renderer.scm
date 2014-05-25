(require-extension posix-extras)
(require-extension gl glu glut glm)
(require-extension srfi-4)
(require-extension srfi-19-core)
(require-extension lolevel)

(require "common/opengl-monkey")

(define (RenderObject program vertices indices type size)
  (gl:UseProgram program)

  (gl:EnableVertexAttribArray attribute_vcoord)
  (gl:EnableVertexAttribArray attribute_vnormal)

  (gl:BindBuffer gl:ARRAY_BUFFER vertices)
  (gl:VertexAttribPointer 
     attribute_vcoord
     3
     gl:FLOAT
     gl:FALSE
     0
     0)

  (gl:VertexAttribPointer 
     attribute_vnormal
     3
     gl:FLOAT
     gl:FALSE
     0
     0)

  (gl:BindBuffer gl:ELEMENT_ARRAY_BUFFER indices)

  (let ((size (gl:GetBufferParameteriv gl:ELEMENT_ARRAY_BUFFER gl:BUFFER_SIZE)))
    (gl:DrawElements type (/ size 2) gl:UNSIGNED_SHORT #f))

  (gl:DisableVertexAttribArray attribute_vcoord)
  ; (gl:Disable gl:DEPTH_TEST)
  (gl:DisableVertexAttribArray attribute_vnormal)
  )



(define (renderFunc)
  (gl:ClearColor 1 1 1 1)
  (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:DEPTH_BUFFER_BIT))

  (RenderObject pink-program vbo_all ibo_cube_elements gl:TRIANGLES 3)
  (RenderObject dark-program vbo_all ibo_maze_elements gl:LINES 2)

  (glut:SwapBuffers))
