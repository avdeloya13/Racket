;; Author: Ana Valeria Deloya Andrade

#lang nanopass              ;Lenguaje con el que trabajaremos: nanopass
(provide (all-defined-out)) 


;;;;; FUNCIONES DE ORDEN SUPERIOR ;;;;;


; MY-MAP

;   Haciendo uso de recursion
    (define (my-map f l)
        (if (empty? l) l
        ;este es el else, si no es vacia
        (cons (f (first l)) 
            (my-map f (rest l)))))


; MY-FILTER

;   Haciendo uso de recursion
    (define (my-filter p l)
        (cond ((empty? l) l)
            ((p (first l)) (cons (first l) (my-filter p (rest l)))) 
                (else (my-filter p (rest l)))))


; MY-FOLDR

;   Haciendo uso de recursion
    (define (my-foldr f i l)
        (if (empty? l) i
            (f (first l) (my-foldr f i (rest l)))))        
  

; MY-FOLDL

    (define (my-foldl f i l)
        (if (empty? l) i
            (my-foldl f (f i (first l)) (rest l)))) 
            

; MY-REVERSE

    (define (my-reverse lst)
    ;my-foldr recibe una funcion (lambda), un valor inicial (lista vacia) y una lista (la lista que vamos a invertir)
        (my-foldr 
            ;Para la parte de lambda, siguiendo un poco la idea del intento anterior, ahora usando my-append
            (lambda (x y) (my-append y (list x))) 
                '() lst))


; MY-LENGHT

    (define (my-length lst) 
        ;vamos sumando 1 por cada elemento de la lista
        (my-foldr (lambda (x y) (+ y 1)) 
            ;partimos el conteo desde 0, y lst es la lista cuya longitud sacamos
            0 lst))
            
; Referencias para las partes de lambdas en my-reverse y my-lenght: los ejemplos de la documentacion 
; para ver como se usan las lambdas y adaptarlo a lo que necesitaba para mi implementacion
; https://docs.racket-lang.org/reference/lambda.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._lambda%29%29


; MY-APPEND

;   Concatena listas en una.
;   Usando funcion my-foldr previamente implementada
    (define (my-append lst1 lst2) (my-foldr cons lst2 lst1))


; MY-CONCATENATE
;   Concatena una lista de listas (sus elementos) en una sola lista.
;   (my-concatenate '((1 2 3 4) (5 6) (7 8)))  -> '(1 2 3 4 5 6 7 8)

;   Intento usando my-foldr y my-append (que concatena)
;   Implementado siguiendo la idea de las implementaciones anteriores en las que se ha usado my-foldr
    (define (my-concatenate lst-de-lst) 
        (my-foldr my-append '() lst-de-lst))


;;;; PALINDROMO ;;;;
;; Recibe numeros, cadenas y listas, y devuelve si es un palindromo o no


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


; Ahora si, palindromo.
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
  
  
;;;; COLLATZ ;;;; 


;; Recibe un entero positivo y devuelve una lista de 
;; transformaciones hasta que se convierte en 1

  (define (lista-collatz x) 
    (if (= x 1) '(1)
      (cond 
        ((odd? x) (cons x (lista-collatz (+ 1 (* x 3)))))
        (else (cons x (lista-collatz (/ x 2)))))))


;; Recibe un entero positivo y devuelve el numero de 
;; transformaciones hasta que se convierte en 1

  ; my-length, auxiliar para pasos-collatz
  (define (my-length l)
    (if (empty? l) 0 (+ 1 (my-length (rest l)))))

  (define (pasos-collatz x)
    (my-length (rest (lista-collatz x))))

