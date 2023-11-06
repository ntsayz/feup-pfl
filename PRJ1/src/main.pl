% includes
:- consult('utils.pl').
:- consult('menu.pl').
:- consult('game.pl').
:- consult('assets.pl').
:- consult('boards.pl').
:- consult('logic.pl').
:- consult('../src/tests.pl/logic_test.pl').
:- consult('ai.pl').

% predicate to run the game
start :- clear_screen, main_menu.

