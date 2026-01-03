/*
================================================================================
  SYSTEME EXPERT : DIAGNOSTIC DES MALADIES DES PLANTES MARAICHERES
  Fichier : systeme_expert.pl (Point d'entree principal)
  Module : Ingenierie de la Connaissance - GINF3
  Domaine : Agriculture
  Langage : SWI-Prolog
================================================================================
*/

:- dynamic symptome/2.
:- dynamic environnement/2.
:- dynamic reponse_utilisateur/2.

/*
================================================================================
  CHARGEMENT DES MODULES
================================================================================
*/

:- consult('base_connaissances.pl').
:- consult('moteur_inference.pl').
:- consult('interface.pl').

/*
================================================================================
  POINT D'ENTREE PRINCIPAL
================================================================================
*/

demarrer :-
    write('=============================================='), nl,
    write('   SYSTEME EXPERT - DIAGNOSTIC MALADIES'), nl,
    write('     Plantes Maraicheres (Tomate, Laitue,'), nl,
    write('         Pomme de terre, Concombre)'), nl,
    write('=============================================='), nl, nl,
    aide.

% Message de bienvenue au demarrage
:- initialization(demarrer).

/*
================================================================================
  EXEMPLES D'UTILISATION :
  
  Pour lancer le systeme :
  ?- consulter.
  
  Pour voir les maladies disponibles :
  ?- lister_maladies.
  
  Pour obtenir des informations sur une maladie :
  ?- info_maladie(mildiou_tomate).
  
  Pour l'aide :
  ?- aide.
  
================================================================================
*/