/*
================================================================================
  MOTEUR D'INFERENCE
  Fichier : moteur_inference.pl
  Contenu : Logique de raisonnement et chainage avant
================================================================================
*/

/*
================================================================================
  1. INITIALISATION DU SYSTEME
================================================================================
*/

initialiser :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)),
    write('=============================================='), nl,
    write('   DEMARRAGE DU DIAGNOSTIC'), nl,
    write('=============================================='), nl, nl.

/*
================================================================================
  2. SELECTION DE LA PLANTE
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
    format('~nVous avez selectionne : ~w~n~n', [Plante]).

plante_correspondante(1, tomate).
plante_correspondante(2, pomme_terre).
plante_correspondante(3, laitue).
plante_correspondante(4, concombre).

/*
================================================================================
  3. GESTION DES QUESTIONS
================================================================================
*/

% Poser une question (avec memorisation)
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
  4. VERIFICATION DES CONDITIONS
================================================================================
*/

% Verification des symptomes
verifier_symptome(Symptome) :-
    symptome(Symptome, oui), !.

verifier_symptome(Symptome) :-
    \+ symptome(Symptome, _),
    description_symptome(Symptome, Description),
    poser_question(Description, Reponse),
    assertz(symptome(Symptome, Reponse)),
    Reponse = oui.

% Verification de l'environnement
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
  5. MOTEUR DE DIAGNOSTIC PRINCIPAL - UTILISE BASE_CONNAISSANCES.PL
================================================================================
*/

diagnostiquer :-
    initialiser,
    selectionner_plante(Plante),
    write('--- ANALYSE DES SYMPTOMES ---'), nl, nl,
    trouver_maladies(Plante, Maladies),
    afficher_resultats(Maladies).

% Trouver toutes les maladies possibles pour une plante
% UTILISE LES REGLES maladie/2 DE base_connaissances.pl
trouver_maladies(Plante, Maladies) :-
    findall(Maladie, 
            verifier_maladie(Maladie, Plante),
            Maladies).

% Verifie qu'une maladie correspond (toutes conditions vraies)
verifier_maladie(Maladie, Plante) :-
    maladie(Maladie, Plante),           % Regle de base_connaissances.pl
    verifier_toutes_conditions(Maladie, Plante).

% Verifie toutes les conditions d'une regle maladie/2
verifier_toutes_conditions(Maladie, Plante) :-
    clause(maladie(Maladie, Plante), Body),
    verifier_body(Body).

% Parcours recursif du corps de regle (Body)
verifier_body(true) :- !.

verifier_body((A, B)) :- 
    !,
    verifier_body(A), 
    verifier_body(B).

verifier_body(symptome(S, oui)) :- 
    verifier_symptome(S).

verifier_body(environnement(E, oui)) :- 
    verifier_environnement(E).

verifier_body(\+ Cond) :-  % Gestion negation (ex: fusariose)
    \+ verifier_body(Cond).

/*
================================================================================
  6. AFFICHAGE DES RESULTATS
================================================================================
*/

% Cas ou aucune maladie n'est identifiee
afficher_resultats([]) :-
    nl,
    write('==============================================='), nl,
    write('  DIAGNOSTIC : Aucune maladie identifiee'), nl,
    write('==============================================='), nl, nl,
    write('Recommandations generales :'), nl,
    write('- Verifier irrigation et drainage'), nl,
    write('- Examiner presence de parasites'), nl,
    write('- Analyser fertilisation'), nl,
    write('- Consulter un agronome si probleme persiste'), nl.

% Cas ou une ou plusieurs maladies sont identifiees
afficher_resultats([Maladie|Autres]) :-
    nl,
    write('==============================================='), nl,
    format('  DIAGNOSTIC : ~w~n', [Maladie]),
    write('==============================================='), nl, nl,
    
    write('--- TRAITEMENT RECOMMANDE ---'), nl,
    traitement(Maladie, Traitements),
    afficher_liste(Traitements),
    
    (prevention(Maladie, Preventions) ->
        (nl, write('--- PREVENTION FUTURE ---'), nl,
         afficher_liste(Preventions))
    ; true),
    
    (Autres \= [] ->
        (nl, write('ATTENTION : Autres maladies possibles :'), nl,
         afficher_maladies(Autres))
    ; true).

% Afficher une liste d'elements
afficher_liste([]).
afficher_liste([H|T]) :-
    format('  * ~w~n', [H]),
    afficher_liste(T).

% Afficher les noms de maladies
afficher_maladies([]).
afficher_maladies([M|Ms]) :-
    format('  - ~w~n', [M]),
    afficher_maladies(Ms).