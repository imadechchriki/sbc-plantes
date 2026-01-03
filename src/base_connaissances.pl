/*
================================================================================
  BASE DE CONNAISSANCES
  Fichier : base_connaissances.pl
  Contenu : Regles de diagnostic, traitements et preventions
================================================================================
*/

/*
================================================================================
  0. DECLARATIONS DYNAMIQUES
================================================================================
*/

:- dynamic symptome/2.
:- dynamic environnement/2.
:- dynamic reponse_utilisateur/2.

/*
================================================================================
  1. REGLES DE PRODUCTION - DIAGNOSTIC DES MALADIES
================================================================================
*/

/* 1.1 Regles de diagnostic pour la Tomate */

maladie(mildiou_tomate, tomate) :-
    symptome(taches_brunes_feuilles, oui),
    symptome(aureole_jaune, oui),
    environnement(humidite_elevee, oui),
    environnement(temperature_fraiche, oui).

maladie(fusariose_tomate, tomate) :-
    symptome(flechissement_plante, oui),
    symptome(jaunissement_unilateral, oui),
    symptome(brunissement_vasculaire, oui).

maladie(oidium_tomate, tomate) :-
    symptome(poudre_blanche, oui),
    symptome(deformation_feuilles, oui),
    environnement(temps_sec, oui).

maladie(alternariose_tomate, tomate) :-
    symptome(taches_circulaires_brunes, oui),
    symptome(cercles_concentriques, oui),
    symptome(chute_feuilles, oui).

/* 1.2 Regles de diagnostic pour la Pomme de terre */

maladie(mildiou_pomme_terre, pomme_terre) :-
    symptome(taches_brunes_feuilles, oui),
    symptome(pourriture_tubercules, oui),
    environnement(humidite_elevee, oui).

maladie(gale_commune, pomme_terre) :-
    symptome(lesions_liegeuses_tubercules, oui),
    symptome(croutes_brunes, oui),
    environnement(sol_alcalin, oui).

/* 1.3 Regles de diagnostic pour la Laitue */

maladie(sclerotinia_laitue, laitue) :-
    symptome(pourriture_molle, oui),
    symptome(mycellium_blanc, oui),
    environnement(humidite_elevee, oui).

maladie(mildiou_laitue, laitue) :-
    symptome(taches_jaunes_feuilles, oui),
    symptome(duvet_gris, oui),
    environnement(temperature_fraiche, oui).

/* 1.4 Regles de diagnostic pour le Concombre */

maladie(oidium_concombre, concombre) :-
    symptome(poudre_blanche, oui),
    symptome(taches_jaunes, oui),
    environnement(temps_sec, oui).

maladie(anthracnose_concombre, concombre) :-
    symptome(taches_circulaires, oui),
    symptome(lesions_fruits, oui),
    environnement(humidite_elevee, oui).

/*
================================================================================
  2. BASE DE CONNAISSANCES - TRAITEMENTS
================================================================================
*/

traitement(mildiou_tomate, 
    ['Appliquer fongicide a base de cuivre (bouillie bordelaise)',
     'Espacer les plants pour ameliorer aeration',
     'Eviter arrosage par aspersion',
     'Eliminer feuilles infectees',
     'Rotation des cultures sur 3-4 ans']).

traitement(fusariose_tomate,
    ['Arracher et detruire plants infectes',
     'Utiliser varietes resistantes',
     'Desinfecter le sol (solarisation)',
     'Rotation culturale obligatoire',
     'Eviter exces azote']).

traitement(oidium_tomate,
    ['Appliquer soufre mouillable',
     'Pulveriser bicarbonate de soude (5g/L)',
     'Ameliorer circulation air',
     'Traitement preventif au debut saison']).

traitement(alternariose_tomate,
    ['Fongicide a base de mancozebe',
     'Eliminer debris vegetaux',
     'Rotation des cultures',
     'Eviter blessures sur plants']).

traitement(mildiou_pomme_terre,
    ['Bouillie bordelaise en preventif',
     'Eliminer plants infectes',
     'Butter les plants',
     'Recolter par temps sec']).

traitement(gale_commune,
    ['Acidifier le sol (pH 5.0-5.5)',
     'Rotation avec cereales',
     'Eviter chaulage avant plantation',
     'Irrigation reguliere formation tubercules']).

traitement(sclerotinia_laitue,
    ['Eliminer plants malades',
     'Ameliorer drainage',
     'Reduire densite plantation',
     'Rotation avec graminees']).

traitement(mildiou_laitue,
    ['Fongicide systemique (metalaxyl)',
     'Aeration des serres',
     'Eviter arrosage feuillage',
     'Varietes resistantes']).

traitement(oidium_concombre,
    ['Soufre en poudrage',
     'Lait dilue 10% en pulverisation',
     'Ventilation serres',
     'Varietes resistantes']).

traitement(anthracnose_concombre,
    ['Fongicide cuivrique',
     'Eliminer fruits touches',
     'Rotation 3 ans',
     'Semences saines certifiees']).

/*
================================================================================
  3. BASE DE CONNAISSANCES - PREVENTION
================================================================================
*/

prevention(mildiou_tomate,
    ['Choisir varietes resistantes (F1 hybrides)',
     'Plantation espacee (80cm entre rangs)',
     'Paillage pour eviter eclaboussures',
     'Surveillance meteo (alerte mildiou)',
     'Traitement preventif si conditions favorables']).

prevention(fusariose_tomate,
    ['Utiliser plants greffes sur porte-greffe resistant',
     'Sterilisation du sol avant plantation',
     'pH optimal 6.5-7.0',
     'Eviter stress hydrique']).

prevention(oidium_tomate,
    ['Pulverisations preventives au soufre',
     'Espacer les plants',
     'Eviter exces d azote',
     'Ventilation adequate en serre']).

prevention(alternariose_tomate,
    ['Rotation des cultures',
     'Eliminer debris vegetaux en fin de saison',
     'Semences saines certifiees',
     'Eviter blessures lors des travaux']).

prevention(mildiou_pomme_terre,
    ['Utiliser plants certifies',
     'Butter haut pour proteger tubercules',
     'Surveillance meteo',
     'Detruire repousses et tas de dechets']).

prevention(gale_commune,
    ['Maintenir pH acide (5.0-5.5)',
     'Rotation longue (4-5 ans)',
     'Eviter fumier frais',
     'Irrigation pendant tuberisation']).

prevention(sclerotinia_laitue,
    ['Rotation avec non-hotes',
     'Drainage efficace',
     'Densite plantation reduite',
     'Elimination residus cultures precedentes']).

prevention(mildiou_laitue,
    ['Varietes resistantes',
     'Espacer les plants',
     'Ventilation en culture abritee',
     'Eviter arrosage par aspersion']).

prevention(oidium_concombre,
    ['Varietes resistantes ou tolerantes',
     'Eviter stress hydrique',
     'Fertilisation equilibree',
     'Ventilation en serre']).

prevention(anthracnose_concombre,
    ['Semences traitees ou certifiees',
     'Rotation triennale minimum',
     'Elimination residus de culture',
     'Eviter travail sur feuillage humide']).

/*
================================================================================
  4. DESCRIPTIONS DES SYMPTOMES
================================================================================
*/

description_symptome(taches_brunes_feuilles, 
    'Presence de taches brunes sur les feuilles').
description_symptome(aureole_jaune, 
    'Taches entourees d une aureole jaune').
description_symptome(flechissement_plante, 
    'Fletrissement de la plante').
description_symptome(jaunissement_unilateral, 
    'Jaunissement d un seul cote de la plante').
description_symptome(brunissement_vasculaire, 
    'Brunissement des vaisseaux (coupe de tige)').
description_symptome(poudre_blanche, 
    'Poudre blanche sur les feuilles').
description_symptome(deformation_feuilles, 
    'Deformation et recroquevillement des feuilles').
description_symptome(taches_circulaires_brunes, 
    'Taches circulaires brunes sur feuilles').
description_symptome(cercles_concentriques, 
    'Cercles concentriques visibles dans les taches').
description_symptome(chute_feuilles, 
    'Chute prematuree des feuilles').
description_symptome(pourriture_tubercules, 
    'Pourriture des tubercules/fruits').
description_symptome(lesions_liegeuses_tubercules, 
    'Lesions liegeuses sur tubercules').
description_symptome(croutes_brunes, 
    'Croutes brunes rugueuses').
description_symptome(pourriture_molle, 
    'Pourriture molle et aqueuse').
description_symptome(mycellium_blanc, 
    'Presence de mycelium blanc cotonneux').
description_symptome(taches_jaunes_feuilles, 
    'Taches jaunes sur feuilles').
description_symptome(duvet_gris, 
    'Duvet gris au revers des feuilles').
description_symptome(taches_jaunes, 
    'Taches jaunes sur feuillage').
description_symptome(taches_circulaires, 
    'Taches circulaires avec centre clair').
description_symptome(lesions_fruits, 
    'Lesions deprimees sur fruits').

/*
================================================================================
  5. DESCRIPTIONS DES CONDITIONS ENVIRONNEMENTALES
================================================================================
*/

description_environnement(humidite_elevee, 
    'Humidite elevee (>80%) ou pluies frequentes').
description_environnement(temperature_fraiche, 
    'Temperatures fraiches (10-20 C)').
description_environnement(temps_sec, 
    'Temps sec et chaud').
description_environnement(sol_alcalin, 
    'Sol alcalin (pH > 7)').