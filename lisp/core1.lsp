;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)
	(cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde)) (list color-actual "cambiar-a-verde"))
        ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo)) (list color-actual "cambiar-a-amarillo"))
		((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo)) (list color-actual "cambiar-a-rojo"))
		(t (list color-actual 'accion-por-defecto))
	)
)


;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA:  Condicional. La decisión del color se realiza mediante evaluaciones lógicas del resultado del resto. 
;; IMPACTO: No destructiva
;; ========================================================

(defun timer (timestamp)
	(if (>= timestamp 0)
		  (cond
		       ((< (mod timestamp 216) 90) 'en-rojo)
		       ((< (mod timestamp 216) 210) 'en-verde)
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

(defun logging (timestamp color-anterior color-actual) 
	(if (>= timestamp 0)
		(format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" timestamp color-anterior color-actual) nil
	)
)

;; FASE 2: Integración de la librería local-time en la función logging.

(defun logging (timestamp color-anterior color-actual)
	(if (>= timestamp 0)
		(format t "[~A] la luz ha cambiado de ~A a ~A~%"
			(local-time:format-timestring
				nil
				(local-time:unix-to-timestamp timestamp))
			color-anterior
			color-actual)
	)
)

;(defun logging (timestamp) ; considerar solo si hay que calcular en que color se encuentra y cual fue el anterior
;	(cond 
;		((and (<= timestamp 90) (equal (timer timestamp) 'en-rojo)) (format t "Tiempo ~A: la luz ha cambiado
;		 de en-amarillo a en-rojo" timestamp))
;		((and (<= timestamp 210) (equal (timer timestamp) 'en-verde)) (format t "Tiempo ~A: la luz ha cambiado
;		 de en-rojo a en-verde" timestamp))
;		(t (format t "Tiempo ~A: la luz ha cambiado de en-verde a en-amarillo" timestamp)))
;)


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

(defun recomendacion-ciclo (duracion-ciclo)
	(cond
	    ((< duracion-ciclo 35) "Duración baja según estándares de ingeniería de tráfico")
	    ((> duracion-ciclo 150) "Duración elevada según estándares de ingeniería de tráfico")
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
