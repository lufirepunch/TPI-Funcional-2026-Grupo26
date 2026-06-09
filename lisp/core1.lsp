;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: 
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)
	(cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'amarillo)) (list 'en-rojo "cambiar-a-amarillo"))
        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'verde)) (list 'en-amarillo "cambiar-a-verde"))
		((and (equal color-actual 'en-verde) (equal cambiar-a 'rojo)) (list 'en-verde "cambiar-a-rojo"))
	)
)

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================
; ESTRATEGIA: La decisión del color se realiza mediante evaluaciones lógicas del resultado del resto implementadas con un cond.

(defun timer (timestamp)
	(if (>= timestamp 0)
		  (cond
		       ((< (mod timestamp 216) 90) 'en-rojo)
		       ((< (mod timestamp 216) 96) 'en-amarillo)
		       (t 'en-verde)
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

