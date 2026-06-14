


;; ========================================================
;; ITERACIÓN 2
;; ========================================================


;; EXTENSIÓN 1

 
;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================


(defun transicion (color-actual cambiar-a)
	(cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'rojo-intermitente)) (list color-actual "cambiar-a-rojo-intermitente"))
        ((and (equal color-actual 'en-rojo-intermitente) (equal cambiar-a 'verde)) (list color-actual "cambiar-a-verde"))
        ((and (equal color-actual 'en-verde) (equal cambiar-a 'verde-intermitente)) (list color-actual "cambiar-a-verde-intermitente"))
        ((and (equal color-actual 'en-verde-intermitente) (equal cambiar-a 'amarillo)) (list color-actual "cambiar-a-amarillo"))
        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'amarillo-intermitente)) (list color-actual "cambiar-a-amarillo-intermitente"))
	    	((and (equal color-actual 'en-amarillo-intermitente) (equal cambiar-a 'rojo)) (list color-actual "cambiar-a-rojo"))	    	
	    	(t (list color-actual 'accion-por-defecto))
	)
)


;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA:  Condicional. La decisión del color se realiza mediante evaluaciones lógicas del resultado del resto. 
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (timestamp duracion-rojo duracion-verde duracion-amarillo)
	(if (>= timestamp 0)
		  (cond
		       ((< (mod timestamp (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) duracion-rojo) 'en-rojo)
		       ((< (mod timestamp (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) (+ duracion-rojo 3)) 'en-rojo-intermitente)
		       ((< (mod timestamp (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) (+ duracion-rojo 3 duracion-verde)) 'en-verde)
		       ((< (mod timestamp (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) (+ duracion-rojo 3 duracion-verde 3)) 'en-verde-intermitente)
		       ((< (mod timestamp (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) (+ duracion-rojo 3 duracion-verde 3 duracion-amarillo)) 'en-amarillo)
		       (t 'en-amarillo-intermitente)
	      )
	    'timestamp-invalido
	)
) 
 


;; ========================================================
;; FUNCIÓN: logging
;; NATURALEZA: Impura (imprime un mensaje)
;; ESTRATEGIA: Formateo de salida
;; IMPACTO: No destructiva (no modifica listas ni nada, recibe datos y los muestra)
;; ========================================================



(defun logging (timestamp duracion-rojo duracion-verde duracion-amarillo) ; considerar solo si hay que calcular en que color se encuentra y cual fue el anterior
	(cond 
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-rojo) (format t "Tiempo ~A: la luz ha cambiado de en-amarillo_intermitente a en-rojo" 
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-rojo-intermitente) (format t "Tiempo ~A: la luz ha cambiado de en-rojo a en-rojo-intermitente" 
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-verde) (format t "Tiempo ~A: la luz ha cambiado de en-rojo-intermitente a en-verde"
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-verde-intermitente) (format t "Tiempo ~A: la luz ha cambiado de en-verde a en-verde-intermitente"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-amarillo) (format t "Tiempo ~A: la luz ha cambiado de en-verde-intermitente a en-amarillo"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
        ((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-amarillo-intermitente) (format t "Tiempo ~A: la luz ha cambiado de en-amarillo a en-amarillo-intermitente"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))

		(t 'timestamp-invalido)
  )
)



;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Operación aritmética
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (duracion-rojo duracion-verde duracion-amarillo)
	(if (and (>= duracion-rojo 0) (>= duracion-verde 0) (>= duracion-amarillo 0)) 
		(+ duracion-rojo duracion-verde duracion-amarillo 9) nil
	)
)


;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmética redondeando a la parte entera
;; IMPACTO: No destructiva	
;; ========================================================


(defun ciclos-por-tiempo (minutos duracion-rojo duracion-verde duracion-amarillo)
	(truncate (/ (* minutos 60) (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)))
)


;; ========================================================
;; FUNCIÓN: porcentaje-temporal
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmética con retorno de lista
;; IMPACTO: No destructiva
;; ========================================================

(defun porcentaje-temporal (duracion-rojo duracion-verde duracion-amarillo)
	(list
		(* (/ duracion-rojo (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)
		(*(/ 3 (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)               ;; porcentaje intermitencia roja
		(* (/ duracion-verde (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)
		(*(/ 3 (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)               ;; porcentaje intermitencia verde
		(* (/ duracion-amarillo (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)
		(*(/ 3 (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)               ;; porcentaje intermitencia amarilla

	)
)




;; EXTENSIÓN 2


(defun logging (timestamp duracion-rojo duracion-verde duracion-amarillo) ; considerar solo si hay que calcular en que color se encuentra y cual fue el anterior
	(cond 
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-rojo) (format nil "Tiempo ~A: la luz ha cambiado de en-amarillo_intermitente a en-rojo" 
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-rojo-intermitente) (format nil "Tiempo ~A: la luz ha cambiado de en-rojo a en-rojo-intermitente" 
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-verde) (format nil "Tiempo ~A: la luz ha cambiado de en-rojo-intermitente a en-verde"
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-verde-intermitente) (format nil "Tiempo ~A: la luz ha cambiado de en-verde a en-verde-intermitente"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-amarillo) (format nil "Tiempo ~A: la luz ha cambiado de en-verde-intermitente a en-amarillo"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
        ((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo)'en-amarillo-intermitente) (format nil "Tiempo ~A: la luz ha cambiado de en-amarillo a en-amarillo-intermitente"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))

		(t 'timestamp-invalido)
  )
)


(defun informe (lista)

  (with-open-file (stream
                   "informe-ejecucion-semaforo.txt"
                   :direction :output)

    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%~%")

    (mapcar
      (lambda (x)
        (format stream "~A~%" x))
      lista)

    (format stream "~%--- Fin del Informe ---~%")
  )
)

;; Cuando se invoque a la funcion informe quedaria asi 
;; (informe (list (logging 17000000 90 120 6) (logging 13499595 90 120 6) (logging 20004840 90 120 6) ))
;; Es decir una lista de cadenas de texto con el resultado de logging