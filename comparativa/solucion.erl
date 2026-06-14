;;            LENGUAJE EN ERLANG
;; ==================================================
;; FUNCION: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional con coincidencia de patrones
;; IMPACTO: No destructiva
;; ==================================================

transicion(en_rojo, verde) ->
    {en_rojo, "cambiar-a-verde"};      ;; las "," separan expresiones

transicion(en_verde, amarillo) ->
    {en_verde, "cambiar-a-amarillo"};  ;; los ";" separan alternativas condicionales

transicion(en_amarillo, rojo) ->
    {en_amarillo, "cambiar-a-rojo"};

transicion(ColorActual, ) ->           ;; "" significa cualquier valor
    {ColorActual, accion_por_defecto}.  ;; se terminan las funciones con un "."

;; en Erlang se definen varios casos de una misma función que se ejecutan
;; cuando los parámetros coinciden con los de la implementación, eso 
;; simplifica la lógica de múltiples comparaciones como en lisp

;; ========================================================
;; FUNCION: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================

timer(Timestamp) when Timestamp < 0 -> timestamp_invalido;  ;; caso en el que la función en el timestamp devuelve inválido

timer(Timestamp) ->
    Posicion = Timestamp rem 216,      ;; calcula la posición actual dentro del ciclo de 216 segundos
                                       ;; en una valiable aux
    if
        Posicion < 90 -> en_rojo;

        Posicion < 210 -> en_verde;

        true -> en_amarillo
    end.