/*
================================================================================
  TESTS UNITAIRES - Règles de Diagnostic Individuelles
  Fichier : tests/test_unitaires.pl
================================================================================
*/

:- use_module(library(plunit)).

% Charger les modules nécessaires (ordre important)
:- consult('../src/base_connaissances.pl').
:- consult('../src/moteur_inference.pl').
:- consult('../src/interface.pl').

/*
================================================================================
  TESTS : MILDIOU TOMATE
================================================================================
*/

:- begin_tests(mildiou_tomate).

test(mildiou_conditions_completes, [true(M == mildiou_tomate)]) :-
    % Toutes les conditions du mildiou sont présentes
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    maladie(M, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(mildiou_conditions_partielles, [fail]) :-
    % Manque aureole_jaune -> diagnostic doit échouer
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    maladie(mildiou_tomate, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(mildiou_environnement_oppose, [fail]) :-
    % Temps sec au lieu d'humidité -> diagnostic doit échouer
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(temps_sec, oui)),
    maladie(mildiou_tomate, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(mildiou_traitement_existe, [true]) :-
    traitement(mildiou_tomate, T),
    is_list(T),
    length(T, Len),
    Len > 0.

test(mildiou_prevention_existe, [true]) :-
    prevention(mildiou_tomate, P),
    is_list(P),
    length(P, Len),
    Len > 0.

:- end_tests(mildiou_tomate).

/*
================================================================================
  TESTS : FUSARIOSE TOMATE
================================================================================
*/

:- begin_tests(fusariose_tomate).

test(fusariose_conditions_completes, [true(M == fusariose_tomate)]) :-
    retractall(symptome(_, _)),
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    assertz(symptome(brunissement_vasculaire, oui)),
    maladie(M, tomate),
    retractall(symptome(_, _)).

test(fusariose_symptome_manquant, [fail]) :-
    retractall(symptome(_, _)),
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    maladie(fusariose_tomate, tomate),
    retractall(symptome(_, _)).

test(fusariose_traitement_existe, [true]) :-
    traitement(fusariose_tomate, T),
    is_list(T).

:- end_tests(fusariose_tomate).

/*
================================================================================
  TESTS : OÏDIUM TOMATE
================================================================================
*/

:- begin_tests(oidium_tomate).

test(oidium_conditions_completes, [true(M == oidium_tomate)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(deformation_feuilles, oui)),
    assertz(environnement(temps_sec, oui)),
    maladie(M, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(oidium_humidite_oppose, [fail]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(deformation_feuilles, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(oidium_tomate, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(oidium_tomate).

/*
================================================================================
  TESTS : ALTERNARIOSE TOMATE
================================================================================
*/

:- begin_tests(alternariose_tomate).

test(alternariose_conditions_completes, [true(M == alternariose_tomate)]) :-
    retractall(symptome(_, _)),
    assertz(symptome(taches_circulaires_brunes, oui)),
    assertz(symptome(cercles_concentriques, oui)),
    assertz(symptome(chute_feuilles, oui)),
    maladie(M, tomate),
    retractall(symptome(_, _)).

test(alternariose_cercles_distinctifs, [true]) :-
    retractall(symptome(_, _)),
    assertz(symptome(taches_circulaires_brunes, oui)),
    assertz(symptome(cercles_concentriques, oui)),
    findall(M, maladie(M, tomate), Maladies),
    member(alternariose_tomate, Maladies),
    retractall(symptome(_, _)).

:- end_tests(alternariose_tomate).

/*
================================================================================
  TESTS : POMME DE TERRE
================================================================================
*/

:- begin_tests(pomme_terre).

test(mildiou_pdt_conditions, [true(M == mildiou_pomme_terre)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(pourriture_tubercules, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(M, pomme_terre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(gale_commune_conditions, [true(M == gale_commune)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    assertz(environnement(sol_alcalin, oui)),
    maladie(M, pomme_terre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(gale_ph_critique, [fail]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    maladie(gale_commune, pomme_terre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(pomme_terre).

/*
================================================================================
  TESTS : LAITUE
================================================================================
*/

:- begin_tests(laitue).

test(sclerotinia_conditions, [true(M == sclerotinia_laitue)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(pourriture_molle, oui)),
    assertz(symptome(mycellium_blanc, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(M, laitue),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(mildiou_laitue_conditions, [true(M == mildiou_laitue)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_jaunes_feuilles, oui)),
    assertz(symptome(duvet_gris, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    maladie(M, laitue),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(laitue).

/*
================================================================================
  TESTS : CONCOMBRE
================================================================================
*/

:- begin_tests(concombre).

test(oidium_concombre_conditions, [true(M == oidium_concombre)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(taches_jaunes, oui)),
    assertz(environnement(temps_sec, oui)),
    maladie(M, concombre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(anthracnose_concombre_conditions, [true(M == anthracnose_concombre)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_circulaires, oui)),
    assertz(symptome(lesions_fruits, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(M, concombre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(concombre).

/*
================================================================================
  COMMANDES PRATIQUES
================================================================================
*/

% Lancer tous les tests unitaires
lancer_tests_unitaires :-
    write('═════════════════════════════════════════'), nl,
    write('  TESTS UNITAIRES - Règles de Diagnostic'), nl,
    write('═════════════════════════════════════════'), nl, nl,
    run_tests.

% Lancer tests d'une catégorie spécifique
test_tomate :- 
    run_tests([mildiou_tomate, fusariose_tomate, oidium_tomate, alternariose_tomate]).

test_pdt :- run_tests(pomme_terre).
test_laitue :- run_tests(laitue).
test_concombre :- run_tests(concombre).

% Initialisation automatique
:- initialization(lancer_tests_unitaires, main).