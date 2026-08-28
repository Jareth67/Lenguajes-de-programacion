#lang racket

;; ==========================================
;; Sección 1: Representación
;; ==========================================

(struct num-exp (n) #:transparent)
(struct id-exp (id) #:transparent)
(struct add-exp (e1 e2) #:transparent)
(struct mul-exp (e1 e2) #:transparent)
(struct with-exp (id e1 e2) #:transparent)

;; ==========================================
;; Seccion 2:función calc
;; ==========================================

;; Operaciones sobre las asociaciones
(define (asocs-vacias) '())

(define (extender asocs nombre valor)
  (cons (cons nombre valor) asocs))

(define (buscar nombre asocs)
  (cdr (assoc nombre asocs)))

;; Implementación de calc
(define (calc expr asocs)
  (match expr
    [(num-exp n) n]
    [(id-exp x) (buscar x asocs)]
    [(add-exp e1 e2) (+ (calc e1 asocs) (calc e2 asocs))]
    [(mul-exp e1 e2) (* (calc e1 asocs) (calc e2 asocs))]
    [(with-exp id e1 e2) 
     (calc e2 (extender asocs id (calc e1 asocs)))]))

;; --- Casos de verificación ---
;; Estos casos devuelven 11, 49, 15 y 30

; (a) add(3, mul(2, 4)) -> 11
(calc (add-exp (num-exp 3) (mul-exp (num-exp 2) (num-exp 4))) 
      (asocs-vacias))

; (b) with y = add(3, 4) in mul(y, y) -> 49
(calc (with-exp 'y (add-exp (num-exp 3) (num-exp 4)) 
                   (mul-exp (id-exp 'y) (id-exp 'y))) 
      (asocs-vacias))

; (c) with x = add(2, 3) in with y = mul(x, 2) in add(x, y) -> 15
(calc (with-exp 'x (add-exp (num-exp 2) (num-exp 3))
                   (with-exp 'y (mul-exp (id-exp 'x) (num-exp 2))
                                (add-exp (id-exp 'x) (id-exp 'y))))
      (asocs-vacias))

; (d) with a = 5 in mul(with b = add(a, 1) in b, a) -> 30
(calc (with-exp 'a (num-exp 5)
                   (mul-exp (with-exp 'b (add-exp (id-exp 'a) (num-exp 1))
                                      (id-exp 'b))
                            (id-exp 'a)))
      (asocs-vacias))


;; ==========================================
;; Seccion 3:trazado
;; ==========================================

#|
Trazado de: with x = add(2, 3) in with y = mul(x, 2) in add(x, y)

Abreviaturas:
N2 = (num-exp 2)            N3 = (num-exp 3)
A1 = (add-exp N2 N3)
M1 = (mul-exp (id-exp 'x) N2)
A2 = (add-exp (id-exp 'x) (id-exp 'y))
W1 = (with-exp 'y M1 A2)

(calc (with-exp 'x A1 W1) sigma0)
= (calc W1 extend(sigma0, 'x, (calc A1 sigma0)))                             [with]
= (calc W1 extend(sigma0, 'x, (calc N2 sigma0) + (calc N3 sigma0)))          [add]
= (calc W1 extend(sigma0, 'x, 2 + (calc N3 sigma0)))                         [num]
= (calc W1 extend(sigma0, 'x, 2 + 3))                                        [num]
= (calc W1 sigma1)                                con sigma1 = extend(sigma0, 'x, 5)
= (calc A2 extend(sigma1, 'y, (calc M1 sigma1)))                             [with]
= (calc A2 extend(sigma1, 'y, (calc (id-exp 'x) sigma1) * (calc N2 sigma1))) [mul]
= (calc A2 extend(sigma1, 'y, lookup('x, sigma1) * (calc N2 sigma1)))        [id]
= (calc A2 extend(sigma1, 'y, 5 * (calc N2 sigma1)))
= (calc A2 extend(sigma1, 'y, 5 * 2))                                        [num]
= (calc A2 sigma2)                                con sigma2 = extend(sigma1, 'y, 10)
= (calc (id-exp 'x) sigma2) + (calc (id-exp 'y) sigma2)                      [add]
= lookup('x, sigma2) + lookup('y, sigma2)                                    [id]
= 5 + 10
= 15
|#integrador