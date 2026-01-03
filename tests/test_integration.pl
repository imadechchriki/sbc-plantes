/*
================================================================================
  TESTS D'INTÉGRATION - Scénarios Complets
  Fichier : tests/test_integration.pl
================================================================================
*/

:- use_module(library(plunit)).

% Charger les modules nécessaires
:- consult('../src/base_connaissances.pl').
:- consult('../src/moteur_inference.pl').
:- consult('../src/interface.pl').

/*
================================================================================
  SCÉNARIOS COMPLETS DE DIAGNOSTIC
================================================================================
*/

:- begin_tests(scenarios_complets).

test(scenario_mildiou_tomate_complet, [true]) :-
    % Simulation d'une session complète de diagnostic mildiou
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)),
    
    % Simulation réponses utilisateur
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    
    % Vérification diagnostic
    findall(M, maladie(M, tomate), Maladies),
    member(mildiou_tomate, Maladies),
    
    % Vérification traitement existe
    traitement(mildiou_tomate, Traitements),
    is_list(Traitements),
    length(Traitements, L),
    L >= 3,
    
    % Cleanup
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)).

test(scenario_aucune_maladie, [true]) :-
    % Aucun symptôme ne correspond
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(taches_brunes_feuilles, non)),
    assertz(symptome(poudre_blanche, non)),
    assertz(symptome(flechissement_plante, non)),
    
    findall(M, maladie(M, tomate), Maladies),
    Maladies == [],
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(scenario_diagnostic_differentiel, [true]) :-
    % Plusieurs maladies possibles (ambiguïté)
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(environnement(humidite_elevee, oui)),
    
    findall(M, (maladie(M, tomate), 
                symptome(taches_brunes_feuilles, oui)),
            Maladies),
    length(Maladies, L),
    L >= 1,
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(scenario_fusariose_complete, [true]) :-
    retractall(symptome(_, _)),
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    assertz(symptome(brunissement_vasculaire, oui)),
    
    maladie(fusariose_tomate, tomate),
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
    
    maladie(oidium_tomate, tomate),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(scenario_gale_sol_alcalin, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    assertz(environnement(sol_alcalin, oui)),
    
    maladie(gale_commune, pomme_terre),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(scenarios_complets).

/*
================================================================================
  TESTS DE COHÉRENCE - BASE DE CONNAISSANCES
================================================================================
*/

:- begin_tests(coherence_base).

test(toutes_maladies_ont_traitement, [true]) :-
    findall(M, (maladie(M, _), \+ traitement(M, _)), SansTraitement),
    SansTraitement == [].

test(toutes_maladies_uniques, [true]) :-
    findall(M, maladie(M, _), Maladies),
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
    findall(M, maladie(M, _), MaladiesAll),
    list_to_set(MaladiesAll, MaladiesUniques),
    length(MaladiesUniques, L),
    L >= 10.

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
    maladie(_, tomate),
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
        findall(M, maladie(M, tomate), _)
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
    findall(Mal, maladie(Mal, tomate), M).

test(conditions_negatives_seulement, [true(M == [])]) :-
    retractall(symptome(_, _)),
    assertz(symptome(taches_brunes_feuilles, non)),
    assertz(symptome(poudre_blanche, non)),
    findall(Mal, maladie(Mal, tomate), M),
    retractall(symptome(_, _)).

test(plante_inexistante, [fail]) :-
    maladie(_, plante_xyz_inexistante).

:- end_tests(robustesse).

/*
================================================================================
  CAS RÉELS DE VALIDATION
================================================================================
*/

:- begin_tests(cas_reels).

test(cas_reel_1_mildiou_typique, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    % Cas observé sur exploitation (5 jan 2026)
    assertz(symptome(taches_brunes_feuilles, oui)),
    assertz(symptome(aureole_jaune, oui)),
    assertz(environnement(humidite_elevee, oui)),
    assertz(environnement(temperature_fraiche, oui)),
    
    maladie(mildiou_tomate, tomate),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(cas_reel_2_oidium_serre, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(poudre_blanche, oui)),
    assertz(symptome(deformation_feuilles, oui)),
    assertz(environnement(temps_sec, oui)),
    
    maladie(oidium_tomate, tomate),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(cas_reel_3_fusariose_avancee, [true]) :-
    retractall(symptome(_, _)),
    
    assertz(symptome(flechissement_plante, oui)),
    assertz(symptome(jaunissement_unilateral, oui)),
    assertz(symptome(brunissement_vasculaire, oui)),
    
    maladie(fusariose_tomate, tomate),
    
    retractall(symptome(_, _)).

test(cas_reel_4_gale_ph_eleve, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(lesions_liegeuses_tubercules, oui)),
    assertz(symptome(croutes_brunes, oui)),
    assertz(environnement(sol_alcalin, oui)),
    
    maladie(gale_commune, pomme_terre),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

test(cas_reel_5_sclerotinia_laitue, [true]) :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    
    assertz(symptome(pourriture_molle, oui)),
    assertz(symptome(mycellium_blanc, oui)),
    assertz(environnement(humidite_elevee, oui)),
    
    maladie(sclerotinia_laitue, laitue),
    
    retractall(symptome(_, _)),
    retractall(environnement(_, _)).

:- end_tests(cas_reels).

/*
================================================================================
  LANCEMENT AUTOMATIQUE
================================================================================
*/

lancer_tests_integration :-
    write('═════════════════════════════════════════'), nl,
    write('  TESTS D\'INTÉGRATION - Scénarios Complets'), nl,
    write('═════════════════════════════════════════'), nl, nl,
    run_tests.

:- initialization(lancer_tests_integration, main).