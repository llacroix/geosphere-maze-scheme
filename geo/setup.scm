(import foreign)
(require-extension extras)
(require-extension lolevel)
(require-extension srfi-4)
(require-extension srfi-19-core)

(require-extension gl glu glut glm soil tween)

(define screen_width 800)
(define screen_height 600)

(define angle_y 0)
(define angle_x 0)

(require "common/opengl")
(require "common/vector")
(require "common/loaders/obj")


(define (makeEngine)
  (glut:InitDisplayMode (+ glut:RGBA glut:ALPHA glut:DOUBLE glut:DEPTH))
  (glut:InitWindowSize screen_width screen_height)
  (glut:CreateWindow "My Rotating Cube")

  (InitResources))

(define all_coords (f32vector 0))
(define cube_elements (u16vector 0))

(define vbo_all -1)
(define ibo_cube_elements -1)
(define ibo_maze_elements -1)

(define attribute_vcoord -1)
(define attribute_vnormal -1)
(define uniform_mvp -1)
(define uniform_inv_transp -1)
(define geosphere 0)

(define (register_sphere tessellation)
  (let* ((geo (tessellate (make-geometry 1) tessellation))
         (verts (list->f32vector (append (concatenate (map f32vector->list (car geo))) (list 0 0 0))))
         (faces (list->u16vector (concatenate (cadr geo))))
         )

      (set! geosphere geo)
      ; Load vbos
      (set! vbo_all (CreateVBO32 gl:ARRAY_BUFFER gl:STATIC_DRAW verts))
      (set! ibo_cube_elements (CreateVBO16 gl:ELEMENT_ARRAY_BUFFER gl:STATIC_DRAW faces))
  ))

(define (generate_maze obj)
  (let* ((geo obj)
         (vertices (car geo))
         (forest (getEdges (cadr geo)))
         (backup (car forest))
         (edges (cadr forest))
         (size (length edges))
         (m-edges (map (lambda (x) (cons (random size) x)) edges))
         (nodes (get-nodes m-edges))
         (path (kruskal nodes m-edges))
         (filtered-path (map (lambda (x) (cdr x)) path))
         (maze (subtract edges filtered-path))
         (base (map (lambda (maze-edge) (append (hash-table-ref backup maze-edge) (list (length vertices)))) maze))
         (faces (list->u16vector (concatenate base))))

    (print "Edges : " (length edges))
    (print "Maze : "  (length maze))

    (set! ibo_maze_elements (CreateVBO16 gl:ELEMENT_ARRAY_BUFFER gl:STATIC_DRAW faces))
    ))

(define tessellation 0)

(define (generate tessellation)
  (print "Regen")
  (register_sphere tessellation)
  (generate_maze geosphere))


(define (InitResources)
  (gl:Enable gl:BLEND)
  (gl:Enable gl:DEPTH_TEST)
  (gl:BlendFunc gl:SRC_ALPHA gl:ONE_MINUS_SRC_ALPHA)
  (gl:Disable gl:CULL_FACE)
  (gl:LineWidth 2)

  (generate tessellation)

  ; Load shaders
  (define vs (CreateShader gl:VERTEX_SHADER (LoadScript "shaders/sphere.v.glsl")))
  (define ds (CreateShader gl:VERTEX_SHADER (LoadScript "shaders/walls.v.glsl")))
  (define fs (CreateShader gl:FRAGMENT_SHADER (LoadScript "shaders/shape.f.glsl")))

  (set! pink-program (CreateProgram (list vs fs)))
  (set! dark-program (CreateProgram (list ds fs)))

  (set! attribute_vcoord (gl:GetAttribLocation pink-program "v_coord"))
  (set! attribute_vnormal (gl:GetAttribLocation pink-program "v_normal"))
  (set! uniform_mvp (gl:GetUniformLocation pink-program "mvp"))
  (set! uniform_inv_transp (gl:GetUniformLocation pink-program "m_3x3_inv_transp"))

  (set! attribute_vcoord (gl:GetAttribLocation dark-program "v_coord"))
  (set! attribute_vnormal (gl:GetAttribLocation dark-program "v_normal"))
  (set! uniform_mvp (gl:GetUniformLocation dark-program "mvp"))
  (set! uniform_inv_transp (gl:GetUniformLocation dark-program "m_3x3_inv_transp"))

  (define endl "\n")

  (print "Init result\n"
    "Opengl error => (" (gl:GetError) ")" endl
    "Vertex shader: " vs endl
    "Fragment shader: " fs endl
    "Program: " pink-program endl endl

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

  ;(define angle (* (glut:Get glut:ELAPSED_TIME) (/ 1 1000) 45))


  (set! t (hop (+ t 0.002)))

  (define axis_y (vec3 0 1 0))

  (set! anim (mat4 1))

  (set! axis_x (m* anim (vec3 1 0 0)))
  (set! anim (rotate anim angle_x axis_x))
  (set! anim (rotate anim angle_y axis_y))

  (define view 
    (look-at (vec3 0 0 -3)
             (vec3 0 0 0)
             (vec3 0 1 0)))

  (define projection
    (perspective 45.0
                 (* 1.0 (/ 800 600))
                 0.1
                 20.0))

  ; (define model
  ;   (translate (mat4 1)
  ;              (vec3 (tween cubic-ease 'out 0 5 t)
  ;                    (tween bounce-ease 'out 4 1 t)
  ;                    0)))
  (define model (translate (mat4 1) (vec3 0 0 0)))

  (set! vp (m* (m* (m* projection
                       view)
                   anim)
               model))

  (gl:UseProgram pink-program)
  (gl:UniformMatrix4fv uniform_mvp 1 gl:FALSE (mat-data vp))
  (gl:UniformMatrix3fv uniform_inv_transp 1 gl:FALSE (mat-data (transpose (inverse (mat3 1)))))

  (gl:UseProgram dark-program)
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
