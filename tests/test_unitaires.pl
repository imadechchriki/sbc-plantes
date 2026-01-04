/*
================================================================================
  TESTS UNITAIRES - Regles Individuelles
  Fichier : tests/test_unitaires.pl
================================================================================
*/

:- use_module(library(plunit)).

% Charger les modules necessaires
:- consult('../src/base_connaissances.pl').
:- consult('../src/moteur_inference.pl').
:- consult('../src/interface.pl').

/*
================================================================================
  TESTS DES REGLES DE DIAGNOSTIC - TOMATE
================================================================================
*/

:- begin_tests(diagnostic_tomate).

test(mildiou_tomate_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    maladie(mildiou_tomate, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(fusariose_tomate_correct, [true]) :-
    retractall(symptome(_, _)),
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    assertz(symptome(brunissement_vasculaire, oui)),
    maladie(fusariose_tomate, tomate),
    retractall(symptome(_, _)).

test(oidium_tomate_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(deformation_feuilles, oui)),
    assertz(environnement(temps_sec, oui)),
    maladie(oidium_tomate, tomate),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(alternariose_tomate_correct, [true]) :-
    retractall(symptome(_, _)),
    assertz(symptome(taches_circulaires_brunes, oui)),
    assertz(symptome(cercles_concentriques, oui)),
    assertz(symptome(chute_feuilles, oui)),
    maladie(alternariose_tomate, tomate),
    retractall(symptome(_, _)).

test(mildiou_symptomes_incomplets, [fail]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    % Manque conditions environnementales
    maladie(mildiou_tomate, tomate).

:- end_tests(diagnostic_tomate).

/*
================================================================================
  TESTS DES REGLES DE DIAGNOSTIC - POMME DE TERRE
================================================================================
*/

:- begin_tests(diagnostic_pomme_terre).

test(mildiou_pomme_terre_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(pourriture_tubercules, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(mildiou_pomme_terre, pomme_terre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(gale_commune_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    assertz(environnement(sol_alcalin, oui)),
    maladie(gale_commune, pomme_terre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(diagnostic_pomme_terre).

/*
================================================================================
  TESTS DES REGLES DE DIAGNOSTIC - LAITUE
================================================================================
*/

:- begin_tests(diagnostic_laitue).

test(sclerotinia_laitue_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(pourriture_molle, oui)),
    assertz(symptome(mycellium_blanc, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(sclerotinia_laitue, laitue),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(mildiou_laitue_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_jaunes_feuilles, oui)),
    assertz(symptome(duvet_gris, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    maladie(mildiou_laitue, laitue),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(diagnostic_laitue).

/*
================================================================================
  TESTS DES REGLES DE DIAGNOSTIC - CONCOMBRE
================================================================================
*/

:- begin_tests(diagnostic_concombre).

test(oidium_concombre_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(taches_jaunes, oui)),
    assertz(environnement(temps_sec, oui)),
    maladie(oidium_concombre, concombre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(anthracnose_concombre_correct, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    assertz(symptome(taches_circulaires, oui)),
    assertz(symptome(lesions_fruits, oui)),
    assertz(environnement(humidite_elevee, oui)),
    maladie(anthracnose_concombre, concombre),
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(diagnostic_concombre).

/*
================================================================================
  TESTS DES TRAITEMENTS
================================================================================
*/

:- begin_tests(traitements).

test(tous_traitements_sont_listes, [true]) :-
    forall(traitement(_, T), is_list(T)).

test(traitements_non_vides, [true]) :-
    forall(traitement(_, T), (length(T, L), L > 0)).

test(mildiou_tomate_traitement_complet, [true]) :-
    traitement(mildiou_tomate, T),
    length(T, L),
    L >= 3,
    member('Appliquer fongicide a base de cuivre (bouillie bordelaise)', T).

test(fusariose_tomate_traitement_destruction, [true]) :-
    traitement(fusariose_tomate, T),
    member('Arracher et detruire plants infectes', T).

test(oidium_traitement_soufre, [true]) :-
    traitement(oidium_tomate, T),
    member('Appliquer soufre mouillable', T).

:- end_tests(traitements).

/*
================================================================================
  TESTS DES PREVENTIONS
================================================================================
*/

:- begin_tests(preventions).

test(preventions_sont_listes, [true]) :-
    forall(prevention(_, P), is_list(P)).

test(preventions_non_vides, [true]) :-
    forall(prevention(_, P), (length(P, L), L > 0)).

test(mildiou_tomate_prevention_varietes, [true]) :-
    prevention(mildiou_tomate, P),
    member('Choisir varietes resistantes (F1 hybrides)', P).

test(gale_commune_prevention_ph, [true]) :-
    prevention(gale_commune, P),
    member('Maintenir pH acide (5.0-5.5)', P).

:- end_tests(preventions).

/*
================================================================================
  TESTS DES DESCRIPTIONS
================================================================================
*/

:- begin_tests(descriptions).

test(tous_symptomes_ont_description, [true]) :-
    findall(S, (maladie(M, _), 
                clause(maladie(M, _), Body),
                extraire_symptomes_body(Body, S)),
            Symptomes),
    list_to_set(Symptomes, SymptomesUniques),
    forall(member(Sym, SymptomesUniques),
           description_symptome(Sym, _)).

test(descriptions_symptomes_non_vides, [true]) :-
    forall(description_symptome(_, Desc),
           (atom_length(Desc, L), L > 5)).

test(descriptions_environnement_non_vides, [true]) :-
    forall(description_environnement(_, Desc),
           (atom_length(Desc, L), L > 5)).

test(symptome_poudre_blanche_existe, [true]) :-
    description_symptome(poudre_blanche, Desc),
    atom_length(Desc, L),
    L > 0.

:- end_tests(descriptions).

% Predicat auxiliaire pour extraire symptomes
extraire_symptomes_body((A, B), S) :- !,
    (extraire_symptomes_body(A, S) ; extraire_symptomes_body(B, S)).
extraire_symptomes_body(symptome(S, oui), S).
extraire_symptomes_body(environnement(_, _), _) :- fail.

/*
================================================================================
  TESTS DU MOTEUR D'INFERENCE
================================================================================
*/

:- begin_tests(moteur_inference).

test(selection_plante_tomate, [true]) :-
    plante_correspondante(1, tomate).

test(selection_plante_pomme_terre, [true]) :-
    plante_correspondante(2, pomme_terre).

test(selection_plante_laitue, [true]) :-
    plante_correspondante(3, laitue).

test(selection_plante_concombre, [true]) :-
    plante_correspondante(4, concombre).

test(verification_symptome_present, [true]) :-
    retractall(symptome(_, _)),
    assertz(symptome(poudre_blanche, oui)),
    verifier_symptome(poudre_blanche),
    retractall(symptome(_, _)).

test(verification_symptome_absent, [fail]) :-
    retractall(symptome(_, _)),
    assertz(symptome(poudre_blanche, non)),
    verifier_symptome(poudre_blanche).

test(verification_environnement_present, [true]) :-
    retractall(environnement(_, _)),
    assertz(environnement(humidite_elevee, oui)),
    verifier_environnement(humidite_elevee),
    retractall(environnement(_, _)).

:- end_tests(moteur_inference).

/*
================================================================================
  TESTS DE L'INTERFACE
================================================================================
*/

:- begin_tests(interface).

test(statistiques_executable, [true]) :-
    catch(statistiques, _, true).

test(lister_maladies_executable, [true]) :-
    catch(lister_maladies, _, true).

test(info_maladie_mildiou, [true]) :-
    catch(info_maladie(mildiou_tomate), _, true).

test(reinitialiser_executable, [true]) :-
    assertz(symptome(test, oui)),
    reinitialiser,
    \+ symptome(test, oui).

:- end_tests(interface).

/*
================================================================================
  LANCEMENT AUTOMATIQUE
================================================================================
*/

lancer_tests_unitaires :-
    write('=============================================='), nl,
    write('  TESTS UNITAIRES - Regles Individuelles'), nl,
    write('=============================================='), nl, nl,
    run_tests.

:- initialization(lancer_tests_unitaires, main).