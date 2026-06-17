-module(solucion).
-export([transicion/2, timer/4]).

%%            LENGUAJE EN ERLANG
%% ==================================================
%% FUNCION: transicion
%% NATURALEZA: Pura
%% ESTRATEGIA: Condicional con coincidencia de patrones
%% IMPACTO: No destructiva
%% ==================================================

transicion(en_rojo, verde) ->
    {en_rojo, "cambiar-a-verde"};      %% las "," separan expresiones

transicion(en_verde, amarillo) ->
    {en_verde, "cambiar-a-amarillo"};  %% los ";" separan alternativas condicionales

transicion(en_amarillo, rojo) ->
    {en_amarillo, "cambiar-a-rojo"};

transicion(ColorActual, _) ->           %% "" significa cualquier valor
    {ColorActual, accion_por_defecto}.  %% se terminan las funciones con un "."

%% en Erlang se definen varios casos de una misma función que se ejecutan
%% cuando los parámetros coinciden con los de la implementación, eso 
%% simplifica la lógica de múltiples comparaciones como en lisp

%% ========================================================
%% FUNCION: timer
%% NATURALEZA: Pura
%% ESTRATEGIA: Condicional
%% IMPACTO: No destructiva
%% ========================================================

timer(Timestamp, _, _, _) when Timestamp < 0 -> timestamp_invalido;  %% caso en el que la función en el timestamp devuelve inválido

timer(Timestamp, DuracionRojo, DuracionVerde, DuracionAmarillo) ->
    CicloTotal = DuracionRojo + DuracionVerde + DuracionAmarillo,
    Posicion = Timestamp rem CicloTotal,
    if
        Posicion < DuracionRojo -> en_rojo;
        Posicion < DuracionRojo + DuracionVerde -> en_verde;
        true -> en_amarillo
    end.