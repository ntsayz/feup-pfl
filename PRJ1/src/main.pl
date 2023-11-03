% includes
:- consult('utils.pl').
:- consult('menu.pl').
:- consult('game.pl').
:- consult('assets.pl').
:- consult('boards.pl').
:- consult('logic.pl').


% predicate to run the game
start :- clear_screen, main_menu.

