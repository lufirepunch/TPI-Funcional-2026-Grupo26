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
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
; ESTRATEGIA: La decisión del color se realiza mediante evaluaciones lógicas del resultado del resto implementadas con un cond.

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
;; FUNCIÓN: 
;; NATURALEZA: 
;; ESTRATEGIA:
;; IMPACTO: 
;; ========================================================

(defun logging (timestamp)
	(cond 
		((equal (timer timestamp) 'en-rojo) (format t "Tiempo ~A: la luz ha cambiado
		 de en-verde a en-rojo" timestamp))

    )
)


;; ========================================================
;; FUNCIÓN: 
;; NATURALEZA: 
;; ESTRATEGIA:
;; IMPACTO: 
;; ========================================================


;; ========================================================
;; FUNCIÓN: 
;; NATURALEZA: 
;; ESTRATEGIA:
;; IMPACTO: 
;; ========================================================

