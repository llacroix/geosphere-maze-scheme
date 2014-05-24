; Save old function
(define gl:GetBufferParameterivOld gl:GetBufferParameteriv)

; Rebind Function to return an int
(define (gl:GetBufferParameteriv target value)
  (let ((size (s32vector 0)))
    (gl:GetBufferParameterivOld target value size)
    (s32vector-ref size 0)))


(define gl:VertexAttribPointerOld gl:VertexAttribPointer)
(define (gl:VertexAttribPointer index size type normalized stride offset)
  (gl:VertexAttribPointerOld index
                             size
                             type
                             normalized
                             stride
                             (cond ((integer? offset) (address->pointer offset))
                                   ((and (boolean? offset) (not offset)) offset)
                                   (else (make-locative offset)))))
