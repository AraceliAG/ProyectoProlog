:- use_module(library(pce)).

% Predicado para crear la ventana del tablero
crear_ventana_tablero :-
    new(@main, picture('Tablero 3x3')),
    send(@main, size, size(300, 300)), % Tamaño de la ventana
    send(@main, open).

% Predicado para crear el tablero 3x3
crear_tablero :-
    new(@tablero, device),
    send(@main, display, @tablero, point(0, 0)),
    Deltas = [0, 100, 200],
    crear_celdas(Deltas, Deltas).

% Predicado para crear las celdas del tablero
crear_celdas([], _).
crear_celdas([X | Xs], Ys) :-
    crear_fila(X, Ys),
    crear_celdas(Xs, Ys).

crear_fila(_, []).
crear_fila(X, [Y | Ys]) :-
    new(Celda, box(100, 100)),
    send(Celda, colour, white),
    send(@tablero, display, Celda, point(X, Y)),
    crear_fila(X, Ys).

% Predicado principal para crear el tablero
crear_tablero :-
    crear_ventana_tablero,
    crear_tablero.

% Inicia la interfaz gráfica
:- crear_tablero.