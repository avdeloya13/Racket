;; Author: Ana Valeria Deloya Andrade

#lang nanopass              ;Decimos que el lenguaje con el que vamos a trabajar es nanopass
(provide (all-defined-out)) ;Indicamos que todas las funciones que se definan aqui son publicas,
                            ;así que podran ser utilizadas en otros modulos siempre y cuando se
                            ;importe solucion.rkt

;; My-map, con recursion
  (define (my-map f l)
    (if (empty? l) l
    ;El else, si no es vacia
      (cons (f (first l)) 
          (my-map f (rest l)))))

;; My-filter, con recursion
  (define (my-filter p l)
    (cond ((empty? l) l)
      ((p (first l)) (cons (first l) (my-filter p (rest l)))) (else (my-filter p (rest l)))))

;; My foldr, con recursion
   (define (my-foldr f i l)
     (if (empty? l) i
       (f (first l) (my-foldr f i (rest l)))))     
  
;; My foldl, pasa 1/3 de pruebas  
  ;;; (define (my-foldl f i l)
  ;;;   (if (empty? l) i (my-foldl f (f (first l) i) (rest l))))  

;; ************************ Palindromo ************************

;; Auxiliar para palindromo. Para pasar un numero a lista.
  (define (numList n)
    (string->list (number->string n)))

;; Los siguientes dos auxiliares (usados para implementar palindromo de esta practica) 
;; fueron tomados de internet. 
;; Sirven para borrar acentos, utilizando la funcion que hice en la primer parte de la practica "my-map".
  
  ; maps accented chars to unaccented chars
  (define translate '#hash((#\á . #\a) (#\é . #\e) (#\í . #\i) (#\ó . #\o) (#\ú . #\u)))

  (define (remove-accents str)

      (apply string ; convert char list back into string
         ; for each char: replace it with non-accented
         ; version, if not present leave it unmodified
         (my-map (λ (c) (hash-ref translate c (const c))) ; para ya no tener acentos
              (string->list str)))) ; convert string to char list

;; Auxiliar para palindromo. Para el caso en que tenemos una cadena.
  (define (strList1 s)
    ; pasa a minusculas y elimina acentos
      (string-downcase (remove-accents s)))

;; Auxiliar para palindromo. Para el caso en que tenemos una cadena.
  (define (strList2 s)
    ; elimina espacios
  (remove* '(#\space)
    ; pasa la cadena a lista
    (string->list (strList1 s))))

  (define (palindromo p) 
    (if 
      ; para el caso en que p sea una cadena, se debe antes
      ; quitar espacios, acentos y pasar a minuscula la cadena.
      (string? p) (equal? (my-reverse  (strList2 p)) (strList2 p))
      
      ; para el caso en que p sea un numero,
      ; debemos convertirlo a lista
      (if (number? p) (equal? (my-reverse (numList p)) (numList p))

        ; para el caso en que p sea una lista
        (equal? (my-reverse p) p))))
  
; ************************ Collatz ************************
  (define (lista-collatz x) 
    (if (= x 1) '(1)
      (cond 
        ((odd? x) (cons x (lista-collatz (+ 1 (* x 3)))))
        (else (cons x (lista-collatz (/ x 2)))))))

  ; my-length, auxiliar para pasos-collatz
  (define (my-length l)
    (if (empty? l) 0 (+ 1 (my-length (rest l)))))

  (define (pasos-collatz x)
    (my-length (rest (lista-collatz x))))

; *********************************************************

;; Usando my-foldr previamente implementado
  (define (my-append lst1 lst2) (my-foldr cons lst2 lst1))

;; Usando my-append que fue previamente implementado con el uso de my-foldr
  (define (my-reverse lst)
    (cond 
      ((empty? lst) empty)
        (else 
          (my-append (my-reverse (cdr lst)) (list (first lst))))))
