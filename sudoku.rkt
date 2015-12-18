#lang racket

;AUTOR: RENÉ GUZMÁN MARTÍNEZ
;código en scheme para curso de paradigmas de programacion, usach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TDA CELDA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;REPRESENTACION: el tda celda es un par, donde el primer elemento indica el valor de la casilla, mientras que el segundo elemento indica si el valor de
;dicha casilla fue agregada por el sistema (valor 0, es decir, no puede ser modificada) o si esta fue agregada por el jugador(valor 1, es modificable)
;ej '(2 0) -> la casilla tiene el valor 2 y puede ser modificada por el usuario


;CONSTRUCTOR

;esta funcion recibe como parametros dos enteros, value indica el valor de la casilla y mod indica si la celda es modificable
;y devuelve un par constituido por esos numeros

(define(crearCelda value mod)
  (if(and (integer? value)(integer? mod)(or(= mod 1)(= mod 0))) 
     (cons value (cons mod null))
     null
     )
  )


;FUNCIONES PERTENENCIA

;recibe como parametro un par y verifica si esta cumple con las condiciones necesarias para que esta sea considerada como una celda valida
;retorna un booleano

(define (celda? C)
  (and(list? C)(= (length C) 2)(integer? (car C)) (integer? (cadr C)) (or(= (cadr C) 1)(= (cadr C) 0)))
  )


;FUNCIONES SELECTORAS

;recibe un celda como parametro y devuelve el valor de la celda 

(define(selectV C)
  (if(celda? C)
     (car C)
     null
     )
  )

;recibe un celda como parametro y devuelve el valor  modificable de la celda
(define(selectM C)
  (if(celda? C)
     (cadr C)
     null
     )
  )


;FUNCIONES MODIFICADORAS

;recibe una celda C y un numero v como parametro de entrada y retorna la celda modificada

(define(modV C v)
  (if(and(celda? C)(integer? v))
     (crearCelda v (cadr C))
     C
     )
  )

;recibe una celda C y un numero m como parametro de entrada y retorna la celda modificada

(define(modM C m)
  (if(and(celda? C)(integer? m))
     (crearCelda (car C) m)
     C
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TDA FILA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;REPRESENTACION: este tda es una lista de celdas y como su nombre lo dice representa una fila 
 

;CONSTRUCTOR LINEAL

;esta funcion recibe como parametro el largo de la fila que se quiere crear (largoF), una fila (fila) y un contador (cont)
;retorna una fila "con valores nulos" (sus valores son pares '(0 1))  de largo largoF
;usa recursion lineal porque es un requerimiento pedido

(define(crearFilaLin largoF cont)
  (if(and(< 0 largoF)(integer? largoF))
     (if(= cont largoF)
        null
        (cons (crearCelda 0 1) (crearFilaLin largoF (+ cont 1))) ;llamada recursiva con estado pendiente
        )
     null
     )
  )

  
;CONSTRUCTOR DE COLA

;esta funcion recibe como parametro el largo de la fila que se quiere crear (largoF), una fila (fila) y un contador (cont)
;retorna una fila "nula" (sus valores son pares '(0 1))  de largo largoF
;usa recursion de cola porque es un requerimiento pedido
  
(define(crearFilaCol largoF fila)
  (if(= largoF 0)
     fila
     (crearFilaCol (- largoF 1) (cons (crearCelda 0 1) fila)) ; llamada recuriva de cola sin llamados pendientes
     )
  )
 
  
;SELECTOR  
;este es el selector del tda fila, recibe como entrada la fila, la posicion del elemento buscado y un contador
;retorna el elemento de la posicion pos de la fila
;usa recursion de cola porque así es más facil recorrer la fila

(define(getFila fila pos cont)
  (if(and(list? fila)(integer? cont)(integer? pos))
     (if(= cont pos)
        (car fila)
        (getFila (cdr fila) pos (+ cont 1)) ;rec de cola
        )
     null
     )
  )
            
     
;MODIFICADOR
;este modificador retorna una nueva lista donde reemplaza el elemento de la posicion con los valores de elem y mod,
; se utiliza recursion lineal ya que necesito ir creando nuevamente la lista mientras llego a la posicion donde quiero
;cambiar el elemento
     
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

;esta funcion busca un elemento num en una fila y retorna un booleano
;se utiliza recursion recursion de cola porque es la forma mas simple de recorrer una matriz

(define (buscarEnFila fila num)
  (if(null? fila)
     #t
     (if(= (caar fila) num)
        #f
        (buscarEnFila (cdr fila) num)
        )
     )
  )


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TDA MATRIZ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;REPRESENTACION: este tda esta hecho a base de una lista de filas y a nivel de juego representa el tablero de sudoku en su totalidad
;donde cada casilla tiene dos valores, el primero el mostrado y el segundo indicasi dicha casilla puede recibir modificaciones o no


;CONSTRUCTOR MATRIZ LINEAL

;esta funcion recibe como parametro el largo de columna, una lista llamada columna y un contador
;retorna una lista de filas (matriz) con valores nulos 
;utiliza recursion lineal por requerimientos

(define(crearMatrizLin largoC cont)
  (if(= cont largoC)
     null
     (cons (crearFilaLin largoC 0)(crearMatrizLin largoC (+ cont 1))) ;llamado recursivo con estado pendiente
     )
  )


;CONSTRUCTOR MATRIZ DE COLA


;esta funcion recibe como parametro el largo de columna, una lista llamada columna y un contador
;retorna una lista de filas (matriz) con valores nulos 
;utiliza recursion de cola por requerimientos

(define(crearMatrizCol largoC columna cont)
  (if(= largoC cont)
     columna
     (crearMatrizCol largoC (cons (crearFilaCol largoC null) columna) (+ cont 1)) ;llamado recursivo sin estado pendiente
     )
  )


;SELECTOR

;ya que tanto el tda matriz, como el tda fila son listas y tienen la misma estructura, ambos usan el mismo selector


;MODIFICADOR

;este modificador recibe como parametros una matriz, el numero de la fila y columna que se quiere "reemplazar", y los nuevos valores
;que tendrá dicha celda.
;retorna una nueva matriz con los cambios que se indicaban en sus parametros de entrada
;utiliza recursion lineal ya que se necesita ir reconstruyendo la nueva matriz mienttras se llega  a la posicion donde se quiere hacer
;el cambio.

(define(modMatriz  matriz fil col value bool)
  (if(and (list? matriz)(number? fil))
     (if(null? matriz)
        null
        (if(= fil 0)
           (cons (setFila (getFila matriz fil 0) col value bool)(cdr matriz))   ;llamado recursivo con estado pendiente
           (cons (car matriz) (modMatriz (cdr matriz)(- fil 1) col value bool)) ;llamado recursivo con estado pendiente
           )
        )
     matriz
     )
  )


;FUNCIONES TDA 

;indica la cantidad de numeros que se mostraran al usuario una vez inicie su partida
;sus parametro de entrada son las dimensiones del tabblero y la dificultad seleccionada
;retorna un valor numerico

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


;esta funcion recibe como parametro una matriz, un numero de columna y un valor num, verifica si dicho valor está en la columna
;retorna un booleano

(define (buscarEnColumna matriz idColumna num)
  (if(null? matriz)
     #t
     (if(= (car(list-ref (car matriz) idColumna)) num)
        #f
        (buscarEnColumna (cdr matriz) idColumna num)
        )
     )
  )


;recibe como paramteros una matrz, las posiciones de la celda a evaluar, el valor a introducir,
;las posicion fila-columna del primer elemento del cuadrante y un contador
;retorna un booleano y usa recursion de cola y la razon de uso es porque es mas simple recorrer 
;la matriz de esta forma

(define(buscarEnCuad matriz fil col value inicF inicC cont)
  (if(= cont (length matriz))
     #t
     (if(= (selectV(getFila(getFila matriz(+ inicF (modulo cont (sqrt(length matriz)))) 0)(+ inicC (quotient cont (sqrt (length matriz)))) 0)) value)
        #t
        (buscarEnCuad matriz fil col value inicF inicC (+ cont 1))
        )
     )
  )
        


;evalua si el valor value de la posicion idFila idColumna se encuentra en la matriz de tal forma que rompe alguna
;de las reglas del sudoku
;retorna un booleano

(define (valido? matriz idFila idColumna value)
  (let*((inicF(- idFila (modulo idFila (sqrt (length matriz)))))
        (inicC(- idColumna (modulo idColumna (sqrt (length matriz)))))
        )
    (and(buscarEnCuad matriz idFila idColumna value inicF inicC 0)(buscarEnFila (getFila matriz idFila 0) value)(buscarEnColumna matriz idColumna value))
    )
  )
  


;REQUERIMIENTOS FUNCIONALES

;funcion que recibe como parametros las dimensiones de la matriz y la dificultad que se desea
;retorna el llamado de su funcion auxliar

(define (createBoardRL N M difficulty)
  (cbrl N M (crearMatrizLin N 0)(numIniciales N M difficulty) 0)
  )


;funcion auxiliar que recibe como parametro las dimensiones del tablero, untablero nulo, la cantidad inical de
;elementos a mostrar y un contador, usa recursion lineal ya que de esa forma ees que esta implementado el modificador
; del tda matriz
;retorna un tablero para ser utilizado en una partida

(define(cbrl N M matriz inic cont)
  (let*((nFila(random  N))
       (nColumna(random N))
        (value(+ (random N) 1))
        )
    (if(< inic cont)
       matriz
       (if(valido? matriz nFila nColumna value)
          (cbrl N M (modMatriz matriz nFila nColumna value 0) inic (+ cont 1))
          (cbrl N M matriz inic cont)
          )
       )
    )
  )


;funcion que recibe como parametros las dimensiones de la matriz y la dificultad que se desea
;retorna el llamado de su funcion auxliar

(define(createBoardRC N M difficulty)
  (cbrc N M (crearMatrizCol N null 0) (numIniciales N M difficulty) 0)
  )


;funcion auxiliar que recibe como parametro las dimensiones del tablero, untablero nulo, la cantidad inical de
;elementos a mostrar y un contador, usa recursion lineal ya que de esa forma ees que esta implementado el modificador
; del tda matriz
;retorna un tablero para ser utilizado en una partida

(define(cbrc N M matriz inic cont)
  (let*((nFila(random  N))
       (nColumna(random N))
        (value(+ (random N) 1))
        )
  (if(< inic cont)
     matriz
     (if(valido? matriz nFila nColumna value)
        (cbrl N M (modMatriz matriz nFila nColumna value 0) inic (+ cont 1))  
        (cbrl N M matriz inic cont)
          )
       )
    )
  )

;CHECKBOARD

;recibe un taablero como entrada y lo evalua en su funcion auxiliar

(define(checkBoard board)
  (checkAux board 0 0)
  )


;recibe como parametro un tablero y dos contadores, uno para las filas y otro para columnas
;revisa elemento a elemento si cumplen con las reglas del sudoku y retorna un booleano como respuesta
;usa recursion de cola ya que asi es mas facil recorrer la matriz elemento a elemento

(define(checkAux board fil col)
  (if(= (length board) fil)
     #t
     (if(= (length board) col)
        (checkAux board (+ fil 1) 0)
        (if(valido? board fil col (car(list-ref (list-ref board fil) col)))
           (checkAux board fil (+ col 1))
           #f
           )
        )
     )
  )
        
;PLAY

;recibe como parametro de entrada el tablero a modificar, las coordenadas y valor de la casilla que se quiere cambiar 
;en caso de que sea una celda modificable(su valor mod = 1) se entrega la matriz con el cambio pedido, en caso contrario
;se devuelve la misma matriz de entrada sin ningun cambio

(define(play board positionX positionY value)
  (if(= (cadr(list-ref (list-ref  board positionY) positionX)) 0)
     board
     (modMatriz board positionY positionX value 1)
     )
  )

