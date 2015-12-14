#lang racket

;TDA CELDA
;REPRESENTACION

;CONSTRUCTOR

(define(crearCelda value mod)
  (if(and (integer? value)(integer? mod)(or(= mod 1)(= mod 0)))
     (cons value (cons mod null))
     null
     )
  )

;FUNCIONES PERTENENCIA
(define (celda? C)
  (and(list? C)(= (length C) 2)(integer? (car C)) (integer? (cadr C)) (or(= (cadr C) 1)(= (cadr C) 0)))
  )

;FUNCIONES SELECTORAS

(define(selectV C)
  (if(celda? C)
     (car C)
     null
     )
  )

(define(selectM C)
  (if(celda? C)
     (cadr C)
     null
     )
  )


;FUNCIONES MODIFICADORAS

(define(modV C v)
  (if(and(celda? C)(integer? v))
     (crearCelda v (cadr C))
     C
     )
  )

(define(modM C m)
  (if(and(celda? C)(integer? m))
     (crearCelda (car C) m)
     C
     )
  )


;FUNCIONES TDA

(define(numIniciales fil col dif)
  (if(= dif 1)
     (- (* fil col 0.5) 1)
     (if(= dif 2)
        (- (* fil col 0.4) 1)
        (if(= dif 3)
           (- (* fil col 0.3) 1)
           0
           )
        )
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TDA FILA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;CONSTRUCTOR LINEAL
(define(crearFilaLin largoF fila cont)
  (if(= cont largoF)
     fila
     (cons (crearCelda 0 1) (crearFilaLin largoF fila (+ cont 1)))
     )
  )

;CONSTRUCTOR DE COLA

(define(crearFilaCol largoF fila)
  (if(= largoF 0)
     fila
     (crearFilaCol (- largoF 1) (cons (crearCelda 0 1) fila))
     )
  )
     
;SELECTOR  

(define(getFila matriz fila cont)
  (if(= cont fila)
     (car matriz)
     (getFila (cdr matriz) fila (+ cont 1))
     )
  )
 
;MODIFICADOR
     
(define (setFila fila pos elem bool)
    (if (and (list? fila) (number? pos))
        (if (>= pos 0)
            (if (null? fila)
                null
                (if (= pos 0)
                    (cons (crearCelda elem bool) (cdr fila))
                    (cons (car fila) (setFila (cdr fila) (- pos 1) elem bool))
                 )
             )
            fila
        )
        fila
    )
  )

                

;FUNCIONES DEL TDA

(define (buscarEnFila fila num)
  (if(null? fila)
     #t
     (if(= (caar fila) num)
        #f
        (buscarEnFila (cdr fila) num)
        )
     )
  )

(define (buscarEnColumna matriz idColumna num)
  (if(null? matriz)
     #t
     (if(= (car(list-ref (car matriz) idColumna)) num)
        #f
        (buscarEnColumna (cdr matriz) idColumna num)
        )
     )
  )

;(cuadFil(- fil (modulo fil (sqrt M))))
;        (cuadCol(- fil (modulo fil (sqrt M))))

(define(buscarEnCuadrante matriz fil col cuadFil cuadCol num cont)
  (if(= cont (length matriz))
     #f
     (if(= (selectV(getFila(getFila matriz (+ cuadFil (modulo cont (sqrt (length matriz)))) 0) (+ cuadCol (quotient cont (sqrt (length matriz)))) 0)) num)
        #t
        (buscarEnCuadrante matriz fil col cuadFil cuadCol num (+ cont 1))
        )
     )
  )
  
(define(cuad matriz M fil col initFil initCol cont num)
  (if(= cont M)
     #t
     (if(= (selectV(getFila(getFila matriz (+ initFil (modulo cont (sqrt M))) 0) (+ initCol (quotient cont (sqrt M))) 0)) num)
        #f
        (cuad matriz M fil col initFil initCol (+ cont 1) num)
        )
     )
  )

(define (valido? matriz idFila idColumna value)
  (and(buscarEnFila (getFila matriz idFila 0) value)(buscarEnColumna matriz idColumna value))
  )
  
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TDA MATRIZ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CONSTRUCTOR MATRIZ LINEAL

(define(crearMatrizLin largoC columna cont)
  (if(= cont largoC)
     columna
     (cons (crearFilaLin largoC columna 0)(crearMatrizLin largoC columna (+ cont 1)))
     )
  )


;CONSTRUCTOR MATRIZ DE COLA
(define(crearMatrizCol largoC columna cont)
  (if(= largoC cont)
     columna
     (crearMatrizCol largoC (cons (crearFilaCol largoC null) columna) (+ cont 1))
     )
  )

;SELECTOR

;ya que tanto el tda matriz, como el tda fila son listas, ambos usan el mismo selector

;MODIFICADOR

(define(modMatriz  matriz fil col value bool)
  (if(and (list? matriz)(number? fil))
     (if(null? matriz)
        null
        (if(= fil 0)
           (cons (setFila (getFila matriz fil 0) col value bool)(cdr matriz))
           (cons (car matriz) (modMatriz (cdr matriz)(- fil 1) col value bool))
           )
        )
     matriz
     )
  )

;FUNCIONES TDA 

(define(modificable matriz nFila nColumna value)
  (and((buscarEnFila (getFila matriz nFila 0) value) (buscarEnColumna matriz nColumna value)))
  )
  

;REQUERIMIENTOS FUNCIONALES

(define (createBoardRL N M difficulty)
  (cbrl N M difficulty (crearMatrizLin N null 0)(numIniciales N M difficulty) 0)
  )

(define(cbrl N M difficulty matriz inic cont)
  (let*((nFila(random  N))
       (nColumna(random N))
        (value(+ (random N) 1))
        )
    (if(< inic cont)
       matriz
       (if(and(buscarEnFila (getFila matriz nFila 0) value) (buscarEnColumna matriz nColumna value))
          (cbrl N M difficulty (modMatriz matriz nFila nColumna value 0) inic (+ cont 1))
          (cbrl N M difficulty matriz inic cont)
          )
       )
    )
  )

(define(createBoardRC N M difficulty)
  (cbrc N M difficulty (crearMatrizCol N null 0) (numIniciales N M difficulty) 0)
  )

(define(cbrc N M difficulty matriz inic cont)
  (let*((nFila(random  N))
       (nColumna(random N))
        (value(+ (random N) 1))
        )
  (if(< inic cont)
     matriz
     (if(and(buscarEnFila (getFila matriz nFila 0) value) (buscarEnColumna matriz nColumna value) (buscarEnCuadrante matriz nFila nColumna (- nFila (modulo nFila (sqrt M))) (- nColumna (modulo nColumna (sqrt (length matriz)))) 0 value))
        (cbrl N M difficulty (modMatriz matriz nFila nColumna value 0) inic (+ cont 1))  
        (cbrl N M difficulty matriz inic cont)
          )
       )
    )
  )

;CHECKBOARD

(define(checkBoard board)
  (checkAux board 0 0)
  )

(define(checkAux board fil col)
  (if(= (length board) fil)
     #t
     (if(= (length board) col)
        (checkAux board (+ fil 1) 0)
        (if(valido? board fil col (car(list-ref (list-ref fil) col)))
           (checkAux board fil (+ col 1))
           #f
           )
        )
     )
  )
        
;PLAY

(define(play board positionX positionY value)
  (if(= (cadr(list-ref (list-ref positionY) positionX)) 0)
     board
     (modMatriz board positionY positionX value 1)
     )
  )
     
  
  
  


