% includes
:- consult('utils.pl').
:- consult('menu.pl').
:- consult('game.pl').
:- consult('assets.pl').

% predicate to run the game
start :- clear_screen, main_menu.

