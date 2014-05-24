(import foreign)
(require-extension extras)
(require-extension lolevel)
(require-extension srfi-4)
(require-extension srfi-19-core)

(require-extension gl glu glut glm soil tween)

(define screen_width 800)
(define screen_height 600)

(require "common/opengl")
(require "common/vector")
(require "common/loaders/obj")


(define (makeEngine)
  (glut:InitDisplayMode (+ glut:RGBA glut:ALPHA glut:DOUBLE glut:DEPTH))
  (glut:InitWindowSize screen_width screen_height)
  (glut:CreateWindow "My Rotating Cube")

  (InitResources))

(define offset 0)
(define all_coords (f32vector 0))
(define cube_elements (u16vector 0))

(define vbo_all -1)
(define ibo_cube_elements -1)

(define attribute_vcoord -1)
(define attribute_vnormal -1)
(define uniform_mvp -1)
(define uniform_inv_transp -1)

(define (InitResources)
  (gl:Enable gl:BLEND)
  (gl:Enable gl:DEPTH_TEST)
  (gl:BlendFunc gl:SRC_ALPHA gl:ONE_MINUS_SRC_ALPHA)
  (gl:Disable gl:CULL_FACE)

  (define obj (call-with-input-file "geo/suzanne.obj" load-obj))

  (print obj)

  (define vertices_a (list->f32vector (list-ref obj 1)))
  (define cube_elements (list->u16vector (list-ref obj 4)))

  (set! offset (* (length (cadr obj)) 4))

  ; Load vbos
  (set! vbo_all (CreateVBO32 gl:ARRAY_BUFFER gl:STATIC_DRAW vertices_a))
  (set! ibo_cube_elements (CreateVBO16 gl:ELEMENT_ARRAY_BUFFER gl:STATIC_DRAW cube_elements))

  ; Load shaders
  (define vs (CreateShader gl:VERTEX_SHADER (LoadScript "geo/cube.v.glsl")))
  (define fs (CreateShader gl:FRAGMENT_SHADER (LoadScript "geo/cube.f.glsl")))

  (set! program (CreateProgram (list vs fs)))

  (set! attribute_vcoord (gl:GetAttribLocation program "v_coord"))
  (set! attribute_vnormal (gl:GetAttribLocation program "v_normal"))
  (set! uniform_mvp (gl:GetUniformLocation program "mvp"))
  (set! uniform_inv_transp (gl:GetUniformLocation program "m_3x3_inv_transp"))

  (define endl "\n")

  (print "Init result\n"
    "Opengl error => (" (gl:GetError) ")" endl
    "Vertex shader: " vs endl
    "Fragment shader: " fs endl
    "Program: " program endl endl

    "VBOS: " endl
    "vbo_all" vbo_all endl
    "ibo_cube_elements: " ibo_cube_elements  endl

    "Attributes: " endl endl
    "attribute_coord3d: " attribute_vcoord endl
    "attribute_texcoord: " attribute_vnormal endl

    "Uniforms: " endl endl
    "uniform_mytexture: " uniform_inv_transp endl
    "uniform_mvp: " uniform_mvp endl
  )

  #t)

(define (flow min max)
  (lambda (x)
    (if (> x max)
      min
      (if (< x min)
        max
        x))))

(define t 0)
(define g 0)

(define (idleFunc)
  ; ??? like in text file
  (define hop (flow 0 1))

  (define angle (* (glut:Get glut:ELAPSED_TIME) (/ 1 1000) 45))

  (set! t (hop (+ t 0.02)))

  (define axis_y (vec3 0 1 0))

  (set! anim (rotate (mat4 1.0) angle axis_y))

  (define view 
    (look-at (vec3 0 2 10)
             (vec3 0 0 -4)
             (vec3 0 1 0)))

  (define projection
    (perspective 45.0
                 (* 1.0 (/ 800 600))
                 0.1
                 20.0))

  (define model
    (translate (mat4 1)
               (vec3 (tween cubic-ease 'out 0 4 t)
                     (tween bounce-ease 'out 4 1 t)
                     0)))

  (set! vp (m* (m* (m* projection
                       view)
                   anim)
               model))

  (gl:UseProgram program)
  (gl:UniformMatrix4fv uniform_mvp 1 gl:FALSE (mat-data vp))
  (gl:UniformMatrix3fv uniform_inv_transp 1 gl:FALSE (mat-data (transpose (inverse (mat3 1)))))

  (glut:PostRedisplay))

(define (reshapeFunc width height)
  ; Setting some globals 
  (set! screen_width width)
  (set! screen_height height)

  ; Defining gl viewport
  (gl:Viewport 0 0 width height)

  (print "Reshape func called"))
