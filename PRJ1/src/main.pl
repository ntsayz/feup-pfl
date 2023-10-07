% includes
:- consult('utils.pl').
:- consult('menu.pl').
:- consult('game.pl').
:- consult('assets.pl').

% predicate to run the application
start :- main_menu.

