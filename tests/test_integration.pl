/*
================================================================================
  TESTS D'INTEGRATION - Scenarios Complets
  Fichier : tests/test_integration.pl
================================================================================
*/

:- use_module(library(plunit)).

% Charger les modules necessaires
:- consult('../src/base_connaissances.pl').
:- consult('../src/moteur_inference.pl').
:- consult('../src/interface.pl').

/*
================================================================================
  SCENARIOS COMPLETS DE DIAGNOSTIC
================================================================================
*/

:- begin_tests(scenarios_complets).

test(scenario_mildiou_tomate_complet, [true]) :-
    % Simulation d'une session complete de diagnostic mildiou
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)),
    
    % Simulation reponses utilisateur
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    
    % Verification diagnostic avec verifier_maladie
    verifier_maladie(mildiou_tomate, tomate),
    
    % Verification traitement existe
    traitement(mildiou_tomate, Traitements),
    is_list(Traitements),
    length(Traitements, L),
    L >= 3,
    
    % Cleanup
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)).

test(scenario_aucune_maladie, [true]) :-
    % Aucun symptome ne correspond
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(taches_brunes_feuilles, non)),
    assertz(symptome(poudre_blanche, non)),
    assertz(symptome(flechissement_plante, non)),
    
    findall(M, verifier_maladie(M, tomate), Maladies),
    Maladies == [],
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(scenario_diagnostic_differentiel, [true]) :-
    % Plusieurs maladies possibles (ambiguite)
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    
    findall(M, (verifier_maladie(M, tomate)), Maladies),
    length(Maladies, L),
    L >= 1,
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(scenario_fusariose_complete, [true]) :-
    retractall(symptome(_, _)),
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    assertz(symptome(brunissement_vasculaire, oui)),
    
    verifier_maladie(fusariose_tomate, tomate),
    traitement(fusariose_tomate, T),
    is_list(T),
    
    retractall(symptome(_, _)).

test(scenario_oidium_serre, [true]) :-
    % Conditions typiques en serre
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(deformation_feuilles, oui)),
    assertz(environnement(temps_sec, oui)),
    
    verifier_maladie(oidium_tomate, tomate),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(scenario_gale_sol_alcalin, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    assertz(environnement(sol_alcalin, oui)),
    
    verifier_maladie(gale_commune, pomme_terre),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(scenarios_complets).

/*
================================================================================
  TESTS DE COHERENCE - BASE DE CONNAISSANCES
================================================================================
*/

:- begin_tests(coherence_base).

test(toutes_maladies_ont_traitement, [true]) :-
    findall(M, clause(maladie(M, _), _), Maladies),
    list_to_set(Maladies, MaladiesUniques),
    forall(member(M, MaladiesUniques), traitement(M, _)).

test(toutes_maladies_uniques, [true]) :-
    findall(M, clause(maladie(M, _), _), Maladies),
    list_to_set(Maladies, MaladiesUniques),
    length(Maladies, L1),
    length(MaladiesUniques, L2),
    L1 == L2.

test(plantes_supportees, [true]) :-
    findall(P, plante_correspondante(_, P), Plantes),
    list_to_set(Plantes, PlantesUniques),
    length(PlantesUniques, L),
    L == 4.

test(nombre_maladies_minimum, [true]) :-
    % Compter les clauses de maladie definies
    findall(M-P, clause(maladie(M, P), _), MaladiesAll),
    list_to_set(MaladiesAll, MaladiesUniques),
    length(MaladiesUniques, L),
    L >= 8.  % Au moins 8 maladies (nous en avons 10)

:- end_tests(coherence_base).

/*
================================================================================
  TESTS DE PERFORMANCE
================================================================================
*/

:- begin_tests(performance).

test(diagnostic_rapide, [true(Temps < 0.1)]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    
    get_time(Debut),
    verifier_maladie(_, tomate),
    get_time(Fin),
    Temps is Fin - Debut,
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(recherche_traitement_rapide, [true(Temps < 0.01)]) :-
    get_time(Debut),
    traitement(mildiou_tomate, _),
    get_time(Fin),
    Temps is Fin - Debut.

test(charge_100_diagnostics, [true(Temps < 5.0)]) :-
    get_time(Debut),
    forall(between(1, 100, _), (
        retractall(symptome(_, _)),
        retractall(environnement(_, _)),
        assertz(symptome(taches_brunes_feuilles, oui)),
        assertz(symptome(aureole_jaune, oui)),
        assertz(environnement(humidite_elevee, oui)),
        assertz(environnement(temperature_fraiche, oui)),
        findall(M, verifier_maladie(M, tomate), _)
    )),
    get_time(Fin),
    Temps is Fin - Debut,
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(performance).

/*
================================================================================
  TESTS DE ROBUSTESSE
================================================================================
*/

:- begin_tests(robustesse).

test(conditions_vides, [true(M == [])]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    findall(Mal, verifier_maladie(Mal, tomate), M).

test(conditions_negatives_seulement, [true(M == [])]) :-
    retractall(symptome(_, _)),
    assertz(symptome(taches_brunes_feuilles, non)),
    assertz(symptome(poudre_blanche, non)),
    findall(Mal, verifier_maladie(Mal, tomate), M),
    retractall(symptome(_, _)).

test(plante_inexistante, [fail]) :-
    verifier_maladie(_, plante_xyz_inexistante).

:- end_tests(robustesse).

/*
================================================================================
  CAS REELS DE VALIDATION
================================================================================
*/

:- begin_tests(cas_reels).

test(cas_reel_1_mildiou_typique, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    % Cas observe sur exploitation (5 jan 2026)
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    
    verifier_maladie(mildiou_tomate, tomate),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(cas_reel_2_oidium_serre, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(deformation_feuilles, oui)),
    assertz(environnement(temps_sec, oui)),
    
    verifier_maladie(oidium_tomate, tomate),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(cas_reel_3_fusariose_avancee, [true]) :-
    retractall(symptome(_, _)),
    
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    assertz(symptome(brunissement_vasculaire, oui)),
    
    verifier_maladie(fusariose_tomate, tomate),
    
    retractall(symptome(_, _)).

test(cas_reel_4_gale_ph_eleve, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    assertz(environnement(sol_alcalin, oui)),
    
    verifier_maladie(gale_commune, pomme_terre),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(cas_reel_5_sclerotinia_laitue, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(pourriture_molle, oui)),
    assertz(symptome(mycellium_blanc, oui)),
    assertz(environnement(humidite_elevee, oui)),
    
    verifier_maladie(sclerotinia_laitue, laitue),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(cas_reels).

/*
================================================================================
  TESTS SUPPLEMENTAIRES - VERIFICATION MODULES
================================================================================
*/

:- begin_tests(verification_modules).

test(base_connaissances_chargee, [true]) :-
    % Verifie que la base de connaissances est chargee
    clause(maladie(mildiou_tomate, tomate), _).

test(moteur_inference_charge, [true]) :-
    % Verifie que le moteur d'inference est charge
    current_predicate(verifier_maladie/2).

test(interface_chargee, [true]) :-
    % Verifie que l'interface est chargee
    current_predicate(consulter/0).

test(descriptions_completes, [true]) :-
    % Tous les symptomes ont une description
    findall(S, (clause(maladie(_, _), Body), 
                extraire_symptomes_test(Body, S)), Symptomes),
    list_to_set(Symptomes, SymptomesUniques),
    forall(member(Sym, SymptomesUniques), 
           description_symptome(Sym, _)).

:- end_tests(verification_modules).

% Predicat auxiliaire pour extraire symptomes des tests
extraire_symptomes_test((A, B), S) :- !, 
    (extraire_symptomes_test(A, S) ; extraire_symptomes_test(B, S)).
extraire_symptomes_test(verifier_symptome(S), S).
extraire_symptomes_test(verifier_environnement(_), _) :- fail.

/*
================================================================================
  LANCEMENT AUTOMATIQUE
================================================================================
*/

lancer_tests_integration :-
    write('=============================================='), nl,
    write('  TESTS D INTEGRATION - Scenarios Complets'), nl,
    write('=============================================='), nl, nl,
    run_tests.

:- initialization(lancer_tests_integration, main).