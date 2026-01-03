/*
================================================================================
  MOTEUR D'INFÉRENCE
  Fichier : moteur_inference.pl
  Contenu : Logique de raisonnement et chaînage avant
================================================================================
*/

/*
================================================================================
  1. INITIALISATION DU SYSTÈME
================================================================================
*/

initialiser :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)),
    write('=============================================='), nl,
    write('   DÉMARRAGE DU DIAGNOSTIC'), nl,
    write('=============================================='), nl, nl.

/*
================================================================================
  2. SÉLECTION DE LA PLANTE
================================================================================
*/

selectionner_plante(Plante) :-
    write('Quelle plante souhaitez-vous diagnostiquer ?'), nl,
    write('1. Tomate'), nl,
    write('2. Pomme de terre'), nl,
    write('3. Laitue'), nl,
    write('4. Concombre'), nl,
    write('Choix (1-4) : '),
    read(Choix),
    plante_correspondante(Choix, Plante),
    format('~nVous avez sélectionné : ~w~n~n', [Plante]).

plante_correspondante(1, tomate).
plante_correspondante(2, pomme_terre).
plante_correspondante(3, laitue).
plante_correspondante(4, concombre).

/*
================================================================================
  3. GESTION DES QUESTIONS
================================================================================
*/

% Poser une question (avec mémorisation)
poser_question(Question, Reponse) :-
    reponse_utilisateur(Question, Rep), !,
    Reponse = Rep.

poser_question(Question, Reponse) :-
    format('~w ? (oui/non) : ', [Question]),
    read(Rep),
    assertz(reponse_utilisateur(Question, Rep)),
    Reponse = Rep.

/*
================================================================================
  4. VÉRIFICATION DES CONDITIONS
================================================================================
*/

% Vérification des symptômes
verifier_symptome(Symptome) :-
    symptome(Symptome, oui), !.

verifier_symptome(Symptome) :-
    \+ symptome(Symptome, _),
    description_symptome(Symptome, Description),
    poser_question(Description, Reponse),
    assertz(symptome(Symptome, Reponse)),
    Reponse = oui.

% Vérification de l'environnement
verifier_environnement(Condition) :-
    environnement(Condition, oui), !.

verifier_environnement(Condition) :-
    \+ environnement(Condition, _),
    description_environnement(Condition, Description),
    poser_question(Description, Reponse),
    assertz(environnement(Condition, Reponse)),
    Reponse = oui.

/*
================================================================================
  5. MOTEUR DE DIAGNOSTIC PRINCIPAL - VERSION SIMPLIFIÉE
================================================================================
*/

diagnostiquer :-
    initialiser,
    selectionner_plante(Plante),
    write('--- ANALYSE DES SYMPTÔMES ---'), nl, nl,
    trouver_maladies(Plante, Maladies),
    afficher_resultats(Maladies).

% Trouver toutes les maladies possibles pour une plante
trouver_maladies(Plante, Maladies) :-
    findall(Maladie, 
            tester_maladie(Maladie, Plante),
            Maladies).

% Tester chaque maladie spécifiquement
tester_maladie(mildiou_tomate, tomate) :-
    verifier_symptome(taches_brunes_feuilles),
    verifier_symptome(aureole_jaune),
    verifier_environnement(humidite_elevee),
    verifier_environnement(temperature_fraiche).

tester_maladie(fusariose_tomate, tomate) :-
    verifier_symptome(flechissement_plante),
    verifier_symptome(jaunissement_unilateral),
    verifier_symptome(brunissement_vasculaire).

tester_maladie(oidium_tomate, tomate) :-
    verifier_symptome(poudre_blanche),
    verifier_symptome(deformation_feuilles),
    verifier_environnement(temps_sec).

tester_maladie(alternariose_tomate, tomate) :-
    verifier_symptome(taches_circulaires_brunes),
    verifier_symptome(cercles_concentriques),
    verifier_symptome(chute_feuilles).

tester_maladie(mildiou_pomme_terre, pomme_terre) :-
    verifier_symptome(taches_brunes_feuilles),
    verifier_symptome(pourriture_tubercules),
    verifier_environnement(humidite_elevee).

tester_maladie(gale_commune, pomme_terre) :-
    verifier_symptome(lesions_liegeuses_tubercules),
    verifier_symptome(croutes_brunes),
    verifier_environnement(sol_alcalin).

tester_maladie(sclerotinia_laitue, laitue) :-
    verifier_symptome(pourriture_molle),
    verifier_symptome(mycellium_blanc),
    verifier_environnement(humidite_elevee).

tester_maladie(mildiou_laitue, laitue) :-
    verifier_symptome(taches_jaunes_feuilles),
    verifier_symptome(duvet_gris),
    verifier_environnement(temperature_fraiche).

tester_maladie(oidium_concombre, concombre) :-
    verifier_symptome(poudre_blanche),
    verifier_symptome(taches_jaunes),
    verifier_environnement(temps_sec).

tester_maladie(anthracnose_concombre, concombre) :-
    verifier_symptome(taches_circulaires),
    verifier_symptome(lesions_fruits),
    verifier_environnement(humidite_elevee).

/*
================================================================================
  6. AFFICHAGE DES RÉSULTATS
================================================================================
*/

% Cas où aucune maladie n'est identifiée
afficher_resultats([]) :-
    nl,
    write('==============================================='), nl,
    write('  DIAGNOSTIC : Aucune maladie identifiée'), nl,
    write('==============================================='), nl, nl,
    write('Recommandations générales :'), nl,
    write('- Vérifier irrigation et drainage'), nl,
    write('- Examiner présence de parasites'), nl,
    write('- Analyser fertilisation'), nl,
    write('- Consulter un agronome si problème persiste'), nl.

% Cas où une ou plusieurs maladies sont identifiées
afficher_resultats([Maladie|Autres]) :-
    nl,
    write('==============================================='), nl,
    format('  DIAGNOSTIC : ~w~n', [Maladie]),
    write('==============================================='), nl, nl,
    
    write('--- TRAITEMENT RECOMMANDÉ ---'), nl,
    traitement(Maladie, Traitements),
    afficher_liste(Traitements),
    
    (prevention(Maladie, Preventions) ->
        (nl, write('--- PRÉVENTION FUTURE ---'), nl,
         afficher_liste(Preventions))
    ; true),
    
    (Autres \= [] ->
        (nl, write('⚠ ATTENTION : Autres maladies possibles :'), nl,
         afficher_maladies(Autres))
    ; true).

% Afficher une liste d'éléments
afficher_liste([]).
afficher_liste([H|T]) :-
    format('  • ~w~n', [H]),
    afficher_liste(T).

% Afficher les noms de maladies
afficher_maladies([]).
afficher_maladies([M|Ms]) :-
    format('  - ~w~n', [M]),
    afficher_maladies(Ms).