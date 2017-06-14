#lang racket
;(library (SchemeLibrary)
;(export createBoardRL createBoardRC checkBoard play getTamanioRL getTamanioRC getDificultadRL getDificultadRC getBoardRL getBoardRC)
;(import (rnrs) (ironscheme random))
;(define null '())

;-------------------------------------------

;TDA Matriz--> Tablero

;--------------------------------------------------------------------------

;Costructor

;Función que crea un tablero a partir de la información entregada por el usuario.
;Entrada: un tamaño, una dificultad
;Salida: Una lista de tipo cons con toda la información del tablero

(define tableroRC (lambda (tamanio dificultad)
                  (let ((boardRC (createBoardRC tamanio tamanio dificultad)))
                  (if (eqv? boardRC '())
                      #f
                      (append (list tamanio) (list dificultad) boardRC)))))

(define tableroRL (lambda (tamanio dificultad)
                    (let ((boardRL (createBoardRL tamanio tamanio dificultad)))
                      (if (eqv? boardRL '())
                          #f
                          (append (list tamanio) (list dificultad) boardRL)))))

;--------------------------------------------------------------------------

;Funciones de pertenencia

;Función que comprueba si el valor del tamaño es un cuadrado perfecto
;Entrada: El tamaño del tablero
;Salida: un valor 0 en caso de que efectivamente sea un cuadrado perfecto o 1 en caso de que no lo sea.
(define (esPerfecto? tamanio)
  (if (= (perfecto tamanio) tamanio)
      #t
      #f
      )
  )
;--------------------------------------------------------------------------

;Modificadores

;Funcion que "modifica" una posicion de una lista por otro valor
;Entrada: La posicion del numero que se quiere cambiar, el numero a ubicar en la posicion y la lista.
;Salida: Una nueva lista con los valores modificados.
(define (setElem L pos elem)
    (if (and (list? L) (number? (+ pos 1)))
        (if (>= pos 0)
            (if (null? L)
                null
                (if (= pos 1)
                    (cons elem (cdr L))
                    (cons (car L) (setElem (cdr L) (- pos 1) elem))
                    )
                )
            L
            )
        L
        )
  )

;Funcion que modifica una posicion de una matriz
;Entrada:Un tablero una posicion x e y y el valor a ingresar en esa posicion
(define (modificarElemento tablero posX posY value)
  (setElem (getElem tablero posX) posY value))

;Función que modifica un elemento de una lista sólo si éste es un 0
;Entrada: Un tablero, las posiciones y un valor
(define (modificarElementoMejorado tablero posX posY value)
  (if (eq? (getElem (getElem tablero posX) posY) 0)
      (setElem tablero posX (modificarElemento tablero posX posY value))
      (modificarElementoMejorado tablero (rand (length tablero)) (rand (length tablero)) (rand (length tablero))
                                 )
      )
  )

;Función que ubica una pista en un tablero solo en una posicion que contenga un 0 y además si es un tablero válido
;Entrada: Un tablero
;Salida: El mismo tablero, con una pista
(define (ubicarPista tablero)
  (let ((posX (rand (length tablero))))
    (let ((tableroAux (modificarElementoMejorado tablero posX (rand (length tablero)) (rand (length tablero)))))
      (if (checkBoard tableroAux)
          tableroAux
          (ubicarPista tablero)
          )
      )
    )
  )

;Funcion que modifica un tablero, obteniendo listas que contienen las columnas
;Entrada: una lista (tablero)
;Salida, la matriz traspuesta
(define (traspuesta lista)
  (cond
    ((null? (car lista)) '())
    (else (cons (map car lista) (traspuesta (map cdr lista))))
    )
  )

;Funcion que concatena dos listas
;Entrada: Dos listas
;Salida: Una lista con ambas listas concatenadas
(define concatena (lambda (lista1 lista2)
                            (if (and (null? lista1) (null? lista2))
                                '()
                                (if (not (null? lista1))
                                    (cons (car lista1) (concatena (cdr lista1) lista2))
                                    (if (not (null? lista2))
                                        (cons (car lista2) (concatena lista1 (cdr lista2)))
                                        '()
                                        )
                                    )
                                )
                            )
  )

;Función que crea una única lista a partir de un conjunto de listas (elimina paréntesis)
;Entrada: Una lista
;Salida: La lista sin paréntesis extras
(define apisonadora (lambda (lista)
                      (if (null? lista)
                          '()
                          (if (list? (car lista))
                              (concatena (apisonadora (car lista)) (apisonadora (cdr lista)))
                              (concatena (list (car lista)) (apisonadora (cdr lista))
                                                 )
                              )
                          )
                      )
  )

;--------------------------------------------------------------------------

;SELECTORES

(define (getElem L pos)
  (if (and (list? L) (number? (+ pos 1)))   ;esto se hara en cada llamado recursivo
      (if (null? L)
          null   ;recorrido diferente => problema con def de funciones
          (if (= pos 1)
              (car L)
              (getElem (cdr L) (- pos 1))
              )
          )
      null
      )
  )

(define (obtenerElemento L pos)
  (if (and (list? L) (number? pos))   ;esto se hara en cada llamado recursivo
      (if (null? L)
          null   ;recorrido diferente => problema con def de funciones
          (if (= pos 0)
              (car L)
              (obtenerElemento (cdr L) (- pos 1))
          )
       )
      null
   )
)

(define (obtenerValorMatriz L fila columna)
  (obtenerElemento (obtenerElemento L fila) columna))

(define (obtenerValor lista fila columna)
  (getElem (getElem lista fila) columna))

(define (getTamanioRL boardRL)
  (if (list? boardRL)
      (car boardRL)
      #f
      )
  )

(define (getDificultadRL boardRL)
  (if (list? boardRL)
      (car (cdr boardRL))
      #f
      )
  )

(define (getBoardRL boardRL)
  (if (list? boardRL)
      (cdr(cdr boardRL))
      #f))

(define (getTamanioRC boardRC)
  (if (list? boardRC)
      (car boardRC)
      #f
      )
  )

(define (getDificultadRC boardRC)
  (if (list? boardRC)
      (car (cdr boardRC))
      #f
      )
  )

(define (getBoardRC boardRC)
  (if (list? boardRC)
      (cdr(cdr boardRC))
      #f))


;Dado un elemento y una lista, retorna la lista sin ese elemento
(define extraerElemento (lambda (elemento lista)
                          (if (null? lista)
                              '()
                              (if (eqv? elemento (car lista))
                                  (extraerElemento elemento (cdr lista))
                                  (cons (car lista) (extraerElemento elemento (cdr lista)))
                                  )
                              )
                          )
  )

;Función que obtiene el mayor elementod e una lista
(define (elementoMayor lista)
    (if (eq? (length lista) 1)
      (car lista)
      (if (> (car lista) (car(cdr lista)))
          (elementoMayor (cons (car lista) (cdr (cdr lista))))
          (elementoMayor (cdr lista))
          )
      )
  )

;Función que obtiene el primer cuadrante de un tablero
;Entrada: Un tablero, 1, 1, el tamaño del cuadrante, una lista vacía 1 1.
(define (PrimerCuadrante tablero fila columna tamanioCuadrante lista contadorFila contadorColumna)
  (let ((cuadranteAux (cons (obtenerValor tablero fila columna) lista)))
  (if (and (eqv? contadorFila tamanioCuadrante) (eqv? contadorColumna tamanioCuadrante))
      (reverse cuadranteAux)
      (if (eqv? fila tamanioCuadrante)
         (PrimerCuadrante tablero (- fila (- tamanioCuadrante 1)) (+ columna 1) tamanioCuadrante cuadranteAux (- contadorFila (- tamanioCuadrante 1)) (+ contadorColumna 1))
         (PrimerCuadrante tablero (+ fila 1) columna tamanioCuadrante cuadranteAux (+ contadorFila  1) contadorColumna))
         )))

;Función que obtiene los cuadrantes superiores de un tablero
;Entrada: Un tablero, el tamaño del cuadrante, un contador, una lista de lista de ceros, 1 y 1
(define (obtenerPrimerosCuadrantes tablero tamanioCuadrante contador lista fila columna)
  (let ((listaConCuadrantes (setElem lista contador (PrimerCuadrante tablero fila columna tamanioCuadrante '() 1 1))))
    (if (eqv? contador tamanioCuadrante)
        listaConCuadrantes
        (obtenerPrimerosCuadrantes tablero tamanioCuadrante (+ contador 1) listaConCuadrantes fila (+ columna tamanioCuadrante))
        ))
  )


;Función que modifica un tablero, obtieniendo listas que contienen los cuadrantes
;Entrada: Un tablero
;Salida: Listas con los cuadrantes.
(define (obtenerCuadrantes tablero contador tamanioCuadrantes vacia listadeceros)
  (let ((cuadrantes (concatena (obtenerPrimerosCuadrantes tablero tamanioCuadrantes 1 listadeceros 1 1) vacia)))
    (if (eqv? contador  tamanioCuadrantes)
        cuadrantes
        (obtenerCuadrantes (aplicaCdr tablero 1 tamanioCuadrantes) (+ contador 1) tamanioCuadrantes cuadrantes listadeceros))))

;--------------------------------------------------------------------------

;Funciones que operan sobre los datos ingresados por el usuario

;Funcion que crea un tavlero válido mediante recursión lineal
;Entrada: Las dimensiones (N y M) y la dificultad.
;Salida: Un tablero válido
(define (createBoardRL N M dificultad)
    (if (and (eqv? N M) (esPerfecto? N) (positive? N) (< dificultad 5) (> dificultad 0))
        (createBoardPrevio (crearMatrizRL N M '()) (obtenerPistas N dificultad))
        '()
        )
    )

;Función que crea un tablero válido con pistas ubicadas
;Entrada: Un tablero y la cantidad de pistas
(define (createBoardPrevio tablero pistas)
  (let ((tableroAux (ubicarPista tablero)))
    (if (eqv? pistas -1)
        tablero
        (createBoardPrevio tableroAux (- pistas 1)))
    )
  )

;Función que crea un tablero lleno de ceros mediante recursión lineal
;Entrada Numero de columnas, de filas y una lista vacia
;Salida: Un tablero lleno de ceros
(define (crearMatrizRL columnas filas lista)
  (if (eq? filas 0)
      lista
     (append (crearMatrizRL columnas (- filas 1) '()) (list (crearLista columnas lista)))
     )
  )

;Función que crea un tablero mediante recursión de cola
;Entrada Numero de filas, de columnas y su dificultad
;NOTA: El numero de filas y columnas deben ser iguales, la dificultad debe ser entre 1 y 4 incluyendo ambos numeros
;Salida: Un tablero válido o una lista vacía en caso de no cumplir las condiciones
(define (createBoardRC N M dificultad)
  (if (and (eqv? N M) (esPerfecto? N) (positive? N) (< dificultad 5) (> dificultad 0))
       (createBoardPrevio (obtenerTablero N) (obtenerPistas N dificultad))
      '())
  )

(define (obtenerTablero tamanio)
  (crearMatriz tamanio))

;Función que crea una matriz de tamanio*tamanio llena de ceros
;Entrada: Un tamaño
;Salida: Una matriz llena de ceros
(define (crearMatriz tamanio)
  (let exterior ((i tamanio) (resultado '()))
    (if (= i 0)
        resultado
        (exterior (- i 1) 
                (cons 
                 (let diagonal ((j tamanio) (fila '()))
                   (if (= j 0)
                       fila
                       (diagonal (- j 1) (cons (if (= i j) 0 0) fila))))
                 resultado)
                )
        )
    )
  )

;Función que comprueba si un tablero es válido
;Entrada: Un tablero
;NOTA: El tablero debe ser creado de forma correcta N y M deben ser iguales junto con una dificultad entre 1 y 4
;NOTA: Si el tablero contiene algún número fuera del rango, se considera inválido
;Salida: #t si es válido, #f en caso contrario
(define (checkBoard tablero)
  (if (eqv? tablero '())
      #f
      (if (> (elementoMayor (apisonadora tablero)) (length tablero))
      #f
      (if (list? tablero)
          (if (eqv? tablero '())
          #f
          (if (eqv? (validezFilas tablero) #f)
              #f
              (if (eqv? (validezColumnas tablero) #f)
                  #f
                  (if (eqv? (validezCuadrantes tablero) #f)
                      #f
                      #t
                      )
                  )
              )
          )
          #f
          )
      )
      )
  )

;Funcion Comprobar validez de filas en un tablero
;Entrada: Una lista
;Salida: Si todas las filas del tablero son validas, retorna #t, de lo contrario #f
(define (validezFilas lista)
    (if (eqv? (length (checkFilas lista)) 1)
        (car lista)
        (if (eqv? #f (car (checkFilas lista)))
            #f
            (validezFilas (cdr (checkFilas lista)))
          )
      )
  )

;Funcion Comprobar validez de columnas en un tablero
;Entrada: Una lista
;Salida: Si todas las columnas del tablero son validas, retorna #t, de lo contrario #f
(define (validezColumnas lista)
  (let ((largoLista (length (checkColumnas lista))))
    (let ((largoDefinitivo (length (extraerElemento #f (checkColumnas lista) ))))
      (if (eqv? largoLista largoDefinitivo)
          #t
          #f
          )
      )
    )
  )

;Funcion Comprobar validez de cuadrantes en un tablero
;Entrada: Una lista
;Salida: Si todos los cuadrantes del tablero son validos, retorna #t, de lo contrario #f
(define (validezCuadrantes lista)
  (let ((largoLista (length (checkCuadrantes lista))))
    (let ((largoDefinitivo (length (extraerElemento #f (checkCuadrantes lista) ))))
      (if (eqv? largoLista largoDefinitivo)
          #t
          #f
          )
      )
    )
  )

;Funcion que comprueba si todos los elementos de las filas no se repiten
;Entrada: una lista (tablero)
;Salida: Una lista de booleanos donde #f es la fila que no cumple con validez y #t la que si
(define (checkFilas tablero)
  (if (list? (car tablero))
      (car (list (map comparaFilas tablero)))
      tablero
      )
  )

;Funcion que comprueba si todos los elementos de la columna no se repiten
;Entrada: una lista (tablero)
;Salida: Lista de booleanos con #t si son correctas todas las columnas #f si no
(define (checkColumnas tablero)
  (let ((x (traspuesta tablero)))
    (if (list? (car x))
        (car (list (map comparaFilas x)))
        x
        )
    )
  )

;Funcion que comprueba si todos los elementos del cuadrante no se repiten
;Entrada: una lista (tablero)
;Salida: Lista de booleanos con #t si son correctos todos los cuadrantes y #f si no
(define (checkCuadrantes tablero)
  (let ((x (obtenerCuadrantes tablero 1 (sqrt (length tablero)) '() (crearMatrizRL (length tablero) (sqrt (length tablero)) '()))))
    (if (list? (car x))
        (car (list (map comparaFilas x)))
        x
        )
    )
  )


;Funcion que revisa si TODOS los elementos de una fila son correctos
;Entrada: Una fila
;Salida: #t o #f (si es válida o inválida)
(define (comparaFilas fila)
  (if (eqv? (length fila) 1)
      #t
      (if (and (eqv? (validarPrimerValorFila fila) #t) (eqv? (validarPrimerValorFila (reverse fila)) #t))
          (comparaFilas (cdr fila))
          #f
          )
      )
  )


;Funcion que revisa si el primer valor de una fila es correcto
;Entrada: Una fila
;Salida: Un #t si es valido, un #f en caso contrario
(define (validarPrimerValorFila fila)
    (if (eq? (length fila) 1)
        #t
        (if (eqv? (car fila) 0)
            (validarPrimerValorFila (cdr fila))
            (if (string? (car fila))
                (if (or (eqv? (string->number (car fila)) (car (cdr fila)))(< (string->number (car fila)) 1))
                    #f
                    (validarPrimerValorFila (cons (car fila) (cdr (cdr fila)))))
                (if (or (eqv? (car fila) (car (cdr fila)))(< (car fila) 1))
                    #f
                    (validarPrimerValorFila (cons (car fila) (cdr (cdr fila))))
                    )
                )
            )
        )
  )


;Funcion Play
;Funcion que permite realizar una jugada sobre el tablero
;Entrada: un tablero, la posicion x la posicion y, y que numero se quiere ubicar
;NOTA: Este valor no puede estar fuera del rango 1<valor<tamaño
;Salida: La lista modificada en la posicion ingresada con el numero ingresado
(define (play tablero posX posY value)
  (let ((numero (number->string value)))
    (let ((tamanio (length tablero)))
      (if (string? (getElem (getElem tablero posX) posY))
          (setElem tablero posX (modificarElemento tablero posX posY numero))
          (if (> value tamanio)
              '()
              (if (< value 1)
                  '()
                  (if (positive? (getElem (getElem tablero posX) posY))
                      '()
                      (setElem tablero posX (modificarElemento tablero posX posY numero))
                      )
                  )
              )
          )
      )
    )
  )

(define (solvePrevio tablero contador)
  (if (eqv? contador 0)
      tablero
      (solvePrevio (ubicarPista tablero) (- contador 1))))
;----------------------------------------------------------------------

;Funciones ajenas al TDA pero que son necesarias para el funcionamiento

;Funcion que eleva al cuadrado la raiz de un numero
(define (perfecto tamanio)
  (* (sqrt tamanio) (sqrt tamanio)))

;Funcion que obtiene un numero random entre 1 y la cantidad de filas del tablero
;Entrada: El tamaño del tablero
;Salida: Un numero random entre 1 y tamaño
(define (rand tamanio)
    (+ 1 (random tamanio)))

(define (crearLista contador lista)
  (if (eq? contador 0)
      lista
      (cons 0 (crearLista (- contador 1) lista))))


;Se obtiene la cantidad de pistas que aparecerán en el tablero
;Entrada: Dificultad de juego
;Salida: Cantidad de pistas
(define (obtenerPistas tamanio dificultad)
  (if (= dificultad 1)
      (inexact->exact (round (* 0.4 (* tamanio tamanio))))
      (if (= dificultad 2)
          (inexact->exact (round (* 0.35 (* tamanio tamanio))))
          (if (= dificultad 3)
              (inexact->exact (round (* 0.3 (* tamanio tamanio))))
              (if (= dificultad 4)
                  (inexact->exact (round (* 0.25 (* tamanio tamanio))))
                  #f
                  )
              )
          )
      )
  )

;Función que saca los primeros terminos de una lista de listas
;Entrada: Un tablero, un contador (que parte en 1) y el tamaño del cuadrante del tablero
;Salida: Un tablero sin los primeros términos, se usa para el checkCuadrantes
(define (aplicaCdr tablero contador tamanioCuadrante)
        (if (eqv? ( - contador 1) tamanioCuadrante)
            tablero
            (aplicaCdr (cdr tablero) (+ contador 1) tamanioCuadrante)))

;)