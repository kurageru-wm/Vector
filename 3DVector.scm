(define-module 3DVector
  (export-all)
  )
(select-module 3DVector)


(define-class <3dvector> ( )
  ((x :init-keyword :x
      :init-value 0
      :accessor x-of)
   (y :init-keyword :y
      :init-value 0
      :accessor y-of)
   (z :init-keyword :z
      :init-value 0
      :accessor z-of)
   ))

(define-method magnitude ((v <3dvector>))
  (sqrt (+ (* (x-of v) (x-of v))
           (* (y-of v) (y-of v))
           (* (z-of v) (z-of v)))))

(define-method normalize ((v <3dvector>))
  (let* ((m1 (magnitude v))
         (m2 (if (<= m1 0.0001) 1 m1))
         (x (/ (x-of v) m2))
         (y (/ (y-of v) m2))
         (z (/ (z-of v) m2)))
    (make <3dvector>
      :x (if (< (abs x) 0.0001) 0.0 x)
      :y (if (< (abs y) 0.0001) 0.0 y)
      :z (if (< (abs z) 0.0001) 0.0 z)
      )))

(define-method reverse ((v <3dvector>))
  (make <3dvector>
    :x (- (x-of v))
    :y (- (y-of v))
    :z (- (z-of v))))

(define (voperator o1 o2 init u v args)
  (define (res proc)
    (o1 (proc u) (proc v) (fold (lambda (x c) (o2 (proc x) c)) init args)))  
  (make <3dvector>
    :x (res x-of)
    :y (res y-of)
    :z (res z-of)))

(define-method + ((u <3dvector>) (v <3dvector>) . args)
  (voperator + + 0 u v args))

(define-method - ((u <3dvector>) (v <3dvector>) . args)
  (voperator - + 0 u v args))

(define-method * ((v <3dvector>) s)
  (make <3dvector>
    :x (* (x-of v) s)
    :y (* (y-of v) s)
    :z (* (z-of v) s)))

(define-method / ((v <3dvector>) s)
  (make <3dvector>
    :x (/. (x-of v) s)
    :y (/. (y-of v) s)
    :z (/. (z-of v) s)))

(define-method ^ ((u <3dvector>) (v <3dvector>))
  (make <3dvector>
    :x (- (* (y-of u) (z-of v)) (* (z-of u) (y-of v)))
    :y (+ (* (- (x-of u)) (z-of v)) (* (z-of u) (x-of v)))
    :z (- (* (x-of u) (y-of v)) (* (y-of u) (x-of v)))))

(define-method * ((u <3dvector>) (v <3dvector>))
  (+ (* (x-of u) (x-of v))
     (* (y-of u) (y-of v))
     (* (z-of u) (z-of v))))

;; (define v (make <3dvector> :x 10 :y 4))
;; (magnitude v)
;; (normalize v)
;; (reverse v)
;; (y-of (+ v v v v (make <3dvector> :y 3)))
;; (y-of (- v (make <3dvector> :x 1 :y 10) (make <3dvector> :x 2)))
;; (y-of (* v 3))
;; (y-of (/ v 3))
;; (magnitude (^ v v))
;; (* (make <3dvector> :x 1 :y 1) (make <3dvector> :x 10))



(provide "3DVector")

