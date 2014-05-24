(define (f32vector-size vec)
  (* ((if (f32vector? vec) f32vector-length u32vector-length) vec) 4))

(define (u32vector-size vec)
  (* (u32vector-length vec) 4))
