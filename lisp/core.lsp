(defun ingresar-datos()
	(let (timestamp minutos duracion-rojo duracion-verde duracion-amarillo)
		(pprint "Ingrese un tiempo en segundos: ")
		(setq timestamp (read))
		(pprint "Ingrese un tiempo en minutos: ")
		(setq minutos (read))
		(pprint "Ingrese la duracion del color rojo: ")
		(setq duracion-rojo (read))
		(pprint "Ingrese la duracion del color verde: ")
		(setq duracion-verde (read))
		(pprint "Ingrese la duracion del color amarillo: ")
		(setq duracion-amarillo (read))

		(if (and (numberp timestamp) (numberp minutos) (numberp duracion-rojo) (numberp duracion-verde) (numberp duracion-amarillo))
			(ejecutarSemaforo timestamp minutos duracion-rojo duracion-verde duracion-amarillo)
			(pprint "Datos inválidos")
			)
	)
)
;; en este caso unicamente se uso la funcion seq para el ingreso y lectura de los datos/variables, como buena practica de la materia!!

(defun ejecutarSemaforo (timestamp minutos duracion-rojo duracion-verde duracion-amarillo)
	(progn 
		    (pprint (transicion (timer timestamp duracion-rojo duracion-verde duracion-amarillo)))
		    (logging timestamp duracion-rojo duracion-verde duracion-amarillo)
		    (format t "El ciclo dura ~A segundos~%" (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo))
		    (pprint (recomendacion-ciclo duracion-rojo duracion-verde duracion-amarillo))
		    (format t "En ~A minutos el ciclo se ejecuta ~A veces~%" minutos (ciclos-por-tiempo minutos duracion-rojo duracion-verde duracion-amarillo))
		    (format t "El porcentaje de rojo verde y amarillo es ~A respectivamente~%" (porcentaje-temporal duracion-rojo duracion-verde duracion-amarillo))
  )
)


;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual) ;; color-actual es timer
	(cond
        ((equal color-actual 'en-rojo) (list color-actual "cambiar-a-verde"))
        ((equal color-actual 'en-verde) (list color-actual "cambiar-a-amarillo"))
		    ((equal color-actual 'en-amarillo) (list color-actual "cambiar-a-rojo"))
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
		       ((< (mod timestamp (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) (+ duracion-rojo duracion-verde)) 'en-verde)
		       (t 'en-amarillo)
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


(defun logging (timestamp duracion-rojo duracion-verde duracion-amarillo)
	(cond
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-rojo) (format t "Tiempo ~A: la luz ha cambiado de en-amarillo a en-rojo" timestamp))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-verde) (format t "Tiempo ~A: la luz ha cambiado de en-rojo a en-verde" timestamp))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-amarillo) (format t "Tiempo ~A: la luz ha cambiado de en-verde a en-amarillo" timestamp))
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
		(+ duracion-rojo duracion-verde duracion-amarillo) nil
	)
)


;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (devuelve la recomendación pero no imprime)
;; ESTRATEGIA: Análisis lógico condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion-rojo duracion-verde duracion-amarillo)
	(cond
	    ((< (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo) 35) "Duración baja según estándares de ingeniería de tráfico")
	    ((> (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo) 150) "Duración elevada según estándares de ingeniería de tráfico")
	    (t  "Duración óptima según estándares de ingeniería de tráfico!!")
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
		(* (/ duracion-verde (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)
		(* (/ duracion-amarillo (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 100)
	)
)






;; FASE 2: Integración de la librería local-time en la función logging.

;; ========================================================
;; FUNCIÓN: logging
;; NATURALEZA: Impura (imprime un mensaje)
;; ESTRATEGIA: Formateo de salida
;; IMPACTO: No destructiva (no modifica listas ni nada, recibe datos y los muestra)
;; ========================================================

(ql:quickload :local-time)

(defun logging (timestamp duracion-rojo duracion-verde duracion-amarillo) ; considerar solo si hay que calcular en que color se encuentra y cual fue el anterior
	(cond 
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-rojo) (format t "Tiempo ~A: la luz ha cambiado de en-amarillo a en-rojo" 
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-verde) (format t "Tiempo ~A: la luz ha cambiado de en-rojo a en-verde"
			(local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		((equal (timer timestamp duracion-rojo duracion-verde duracion-amarillo) 'en-amarillo) (format t "Tiempo ~A: la luz ha cambiado de en-verde a en-amarillo"
		  (local-time:format-timestring nil (local-time:unix-to-timestamp timestamp))))
		(t 'timestamp-invalido)
  )
)






