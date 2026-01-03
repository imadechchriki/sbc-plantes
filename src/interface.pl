/*
================================================================================
  INTERFACE UTILISATEUR
  Fichier : interface.pl
  Contenu : Commandes et utilitaires pour l'utilisateur
================================================================================
*/

/*
================================================================================
  1. COMMANDE PRINCIPALE DE CONSULTATION
================================================================================
*/

consulter :- 
    diagnostiquer.

/*
================================================================================
  2. LISTER LES MALADIES DISPONIBLES
================================================================================
*/

lister_maladies :-
    write('=============================================='), nl,
    write('  MALADIES DANS LA BASE DE CONNAISSANCES'), nl,
    write('=============================================='), nl, nl,
    findall(M-P, maladie(M, P), Maladies),
    trier_par_plante(Maladies).

% Trier et afficher les maladies par plante
trier_par_plante(Maladies) :-
    afficher_par_plante(tomate, Maladies),
    afficher_par_plante(pomme_terre, Maladies),
    afficher_par_plante(laitue, Maladies),
    afficher_par_plante(concombre, Maladies).

afficher_par_plante(Plante, Maladies) :-
    format('~n--- ~w ---~n', [Plante]),
    findall(M, member(M-Plante, Maladies), MaladiesPlante),
    afficher_maladies_simple(MaladiesPlante).

afficher_maladies_simple([]).
afficher_maladies_simple([M|Ms]) :-
    format('  * ~w~n', [M]),
    afficher_maladies_simple(Ms).

/*
================================================================================
  3. INFORMATIONS SUR UNE MALADIE SPECIFIQUE
================================================================================
*/

info_maladie(Maladie) :-
    (maladie(Maladie, Plante) ->
        (format('~n==============================================~n', []),
         format('  FICHE : ~w~n', [Maladie]),
         format('  Plante concernee : ~w~n', [Plante]),
         format('==============================================~n~n', []),
         
         write('--- TRAITEMENT ---'), nl,
         traitement(Maladie, T),
         afficher_liste_info(T),
         
         (prevention(Maladie, P) ->
             (nl, write('--- PREVENTION ---'), nl,
              afficher_liste_info(P))
         ; true),
         
         nl, write('--- SYMPTOMES ASSOCIES ---'), nl,
         afficher_symptomes_maladie(Maladie),
         nl)
    ; 
        (write('Erreur : Maladie non trouvee dans la base'), nl,
         write('Utilisez lister_maladies. pour voir les maladies disponibles'), nl)).

afficher_liste_info([]).
afficher_liste_info([H|T]) :-
    format('  * ~w~n', [H]),
    afficher_liste_info(T).

% Afficher les symptomes d'une maladie
afficher_symptomes_maladie(Maladie) :-
    clause(maladie(Maladie, _), Body),
    extraire_symptomes(Body, Symptomes),
    afficher_symptomes_detailles(Symptomes).

extraire_symptomes((A, B), Resultat) :- 
    !,
    extraire_symptomes(A, S1),
    extraire_symptomes(B, S2),
    append(S1, S2, Resultat).

extraire_symptomes(symptome(S, oui), [S]) :- !.
extraire_symptomes(environnement(_, _), []) :- !.
extraire_symptomes(_, []).

afficher_symptomes_detailles([]).
afficher_symptomes_detailles([S|Ss]) :-
    (description_symptome(S, Desc) ->
        format('  * ~w~n', [Desc])
    ;
        format('  * ~w~n', [S])),
    afficher_symptomes_detailles(Ss).

/*
================================================================================
  4. STATISTIQUES DU SYSTEME
================================================================================
*/

statistiques :-
    write('=============================================='), nl,
    write('  STATISTIQUES DU SYSTEME'), nl,
    write('=============================================='), nl, nl,
    
    % Compter les maladies
    findall(M, maladie(M, _), Maladies),
    length(Maladies, NbMaladies),
    format('Nombre total de maladies : ~w~n', [NbMaladies]),
    
    % Compter par plante
    findall(M, maladie(M, tomate), MTomate),
    length(MTomate, NbTomate),
    format('  - Tomate : ~w~n', [NbTomate]),
    
    findall(M, maladie(M, pomme_terre), MPomme),
    length(MPomme, NbPomme),
    format('  - Pomme de terre : ~w~n', [NbPomme]),
    
    findall(M, maladie(M, laitue), MLaitue),
    length(MLaitue, NbLaitue),
    format('  - Laitue : ~w~n', [NbLaitue]),
    
    findall(M, maladie(M, concombre), MConcombre),
    length(MConcombre, NbConcombre),
    format('  - Concombre : ~w~n', [NbConcombre]),
    
    nl,
    
    % Compter les symptomes
    findall(S, description_symptome(S, _), Symptomes),
    length(Symptomes, NbSymptomes),
    format('Nombre de symptomes : ~w~n', [NbSymptomes]),
    
    % Compter les conditions environnementales
    findall(E, description_environnement(E, _), Environnements),
    length(Environnements, NbEnvironnements),
    format('Conditions environnementales : ~w~n', [NbEnvironnements]),
    nl.

/*
================================================================================
  5. RECHERCHE DE MALADIES PAR SYMPTOME
================================================================================
*/

rechercher_par_symptome(Symptome) :-
    format('~nRecherche des maladies avec le symptome : ~w~n~n', [Symptome]),
    findall(M-P, 
            (maladie(M, P), 
             clause(maladie(M, P), Body), 
             contient_symptome(Body, Symptome)),
            Resultats),
    (Resultats = [] ->
        write('Aucune maladie trouvee avec ce symptome.'), nl
    ;
        write('Maladies trouvees :'), nl,
        afficher_resultats_recherche(Resultats)).

contient_symptome((A, _), Symptome) :- 
    contient_symptome(A, Symptome), !.
contient_symptome((_, B), Symptome) :- 
    contient_symptome(B, Symptome), !.
contient_symptome(symptome(Symptome, oui), Symptome).

afficher_resultats_recherche([]).
afficher_resultats_recherche([M-P|Rest]) :-
    format('  * ~w (Plante: ~w)~n', [M, P]),
    afficher_resultats_recherche(Rest).

/*
================================================================================
  6. AIDE ET DOCUMENTATION
================================================================================
*/

aide :-
    write('=============================================='), nl,
    write('  COMMANDES DISPONIBLES'), nl,
    write('=============================================='), nl, nl,
    
    write('DIAGNOSTIC :'), nl,
    write('  consulter.                   - Lancer un diagnostic interactif'), nl,
    nl,
    
    write('CONSULTATION :'), nl,
    write('  lister_maladies.             - Voir toutes les maladies'), nl,
    write('  info_maladie(Nom).           - Infos detaillees sur une maladie'), nl,
    write('  rechercher_par_symptome(S).  - Chercher maladies par symptome'), nl,
    nl,
    
    write('STATISTIQUES :'), nl,
    write('  statistiques.                - Voir les stats du systeme'), nl,
    nl,
    
    write('SYSTEME :'), nl,
    write('  aide.                        - Afficher cette aide'), nl,
    write('  halt.                        - Quitter le systeme'), nl,
    nl,
    
    write('EXEMPLES :'), nl,
    write('  ?- consulter.'), nl,
    write('  ?- info_maladie(mildiou_tomate).'), nl,
    write('  ?- rechercher_par_symptome(poudre_blanche).'), nl,
    nl.

/*
================================================================================
  7. REINITIALISATION
================================================================================
*/

reinitialiser :-
    retractall(symptome(_, _)),
    retractall(environnement(_, _)),
    retractall(reponse_utilisateur(_, _)),
    write('Systeme reinitialise'), nl.