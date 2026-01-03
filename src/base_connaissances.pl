/*
================================================================================
  BASE DE CONNAISSANCES
  Fichier : base_connaissances.pl
  Contenu : Règles de diagnostic, traitements et préventions
================================================================================
*/

/*
================================================================================
  0. DÉCLARATIONS DYNAMIQUES
================================================================================
*/

:- dynamic symptome/2.
:- dynamic environnement/2.
:- dynamic reponse_utilisateur/2.

/*
================================================================================
  1. RÈGLES DE PRODUCTION - DIAGNOSTIC DES MALADIES
================================================================================
*/

/* 1.1 Règles de diagnostic pour la Tomate */

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

/* 1.2 Règles de diagnostic pour la Pomme de terre */

maladie(mildiou_pomme_terre, pomme_terre) :-
    symptome(taches_brunes_feuilles, oui),
    symptome(pourriture_tubercules, oui),
    environnement(humidite_elevee, oui).

maladie(gale_commune, pomme_terre) :-
    symptome(lesions_liegeuses_tubercules, oui),
    symptome(croutes_brunes, oui),
    environnement(sol_alcalin, oui).

/* 1.3 Règles de diagnostic pour la Laitue */

maladie(sclerotinia_laitue, laitue) :-
    symptome(pourriture_molle, oui),
    symptome(mycellium_blanc, oui),
    environnement(humidite_elevee, oui).

maladie(mildiou_laitue, laitue) :-
    symptome(taches_jaunes_feuilles, oui),
    symptome(duvet_gris, oui),
    environnement(temperature_fraiche, oui).

/* 1.4 Règles de diagnostic pour le Concombre */

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
    ['Appliquer fongicide à base de cuivre (bouillie bordelaise)',
     'Espacer les plants pour améliorer aération',
     'Éviter arrosage par aspersion',
     'Éliminer feuilles infectées',
     'Rotation des cultures sur 3-4 ans']).

traitement(fusariose_tomate,
    ['Arracher et détruire plants infectés',
     'Utiliser variétés résistantes',
     'Désinfecter le sol (solarisation)',
     'Rotation culturale obligatoire',
     'Éviter excès azote']).

traitement(oidium_tomate,
    ['Appliquer soufre mouillable',
     'Pulvériser bicarbonate de soude (5g/L)',
     'Améliorer circulation air',
     'Traitement préventif au début saison']).

traitement(alternariose_tomate,
    ['Fongicide à base de mancozèbe',
     'Éliminer débris végétaux',
     'Rotation des cultures',
     'Éviter blessures sur plants']).

traitement(mildiou_pomme_terre,
    ['Bouillie bordelaise en préventif',
     'Éliminer plants infectés',
     'Butter les plants',
     'Récolter par temps sec']).

traitement(gale_commune,
    ['Acidifier le sol (pH 5.0-5.5)',
     'Rotation avec céréales',
     'Éviter chaulage avant plantation',
     'Irrigation régulière formation tubercules']).

traitement(sclerotinia_laitue,
    ['Éliminer plants malades',
     'Améliorer drainage',
     'Réduire densité plantation',
     'Rotation avec graminées']).

traitement(mildiou_laitue,
    ['Fongicide systémique (métalaxyl)',
     'Aération des serres',
     'Éviter arrosage feuillage',
     'Variétés résistantes']).

traitement(oidium_concombre,
    ['Soufre en poudrage',
     'Lait dilué 10% en pulvérisation',
     'Ventilation serres',
     'Variétés résistantes']).

traitement(anthracnose_concombre,
    ['Fongicide cuivrique',
     'Éliminer fruits touchés',
     'Rotation 3 ans',
     'Semences saines certifiées']).

/*
================================================================================
  3. BASE DE CONNAISSANCES - PRÉVENTION
================================================================================
*/

prevention(mildiou_tomate,
    ['Choisir variétés résistantes (F1 hybrides)',
     'Plantation espacée (80cm entre rangs)',
     'Paillage pour éviter éclaboussures',
     'Surveillance météo (alerte mildiou)',
     'Traitement préventif si conditions favorables']).

prevention(fusariose_tomate,
    ['Utiliser plants greffés sur porte-greffe résistant',
     'Stérilisation du sol avant plantation',
     'pH optimal 6.5-7.0',
     'Éviter stress hydrique']).

prevention(oidium_tomate,
    ['Pulvérisations préventives au soufre',
     'Espacer les plants',
     'Éviter excès d\'azote',
     'Ventilation adéquate en serre']).

prevention(alternariose_tomate,
    ['Rotation des cultures',
     'Éliminer débris végétaux en fin de saison',
     'Semences saines certifiées',
     'Éviter blessures lors des travaux']).

prevention(mildiou_pomme_terre,
    ['Utiliser plants certifiés',
     'Butter haut pour protéger tubercules',
     'Surveillance météo',
     'Détruire repousses et tas de déchets']).

prevention(gale_commune,
    ['Maintenir pH acide (5.0-5.5)',
     'Rotation longue (4-5 ans)',
     'Éviter fumier frais',
     'Irrigation pendant tubérisation']).

prevention(sclerotinia_laitue,
    ['Rotation avec non-hôtes',
     'Drainage efficace',
     'Densité plantation réduite',
     'Élimination résidus cultures précédentes']).

prevention(mildiou_laitue,
    ['Variétés résistantes',
     'Espacer les plants',
     'Ventilation en culture abritée',
     'Éviter arrosage par aspersion']).

prevention(oidium_concombre,
    ['Variétés résistantes ou tolérantes',
     'Éviter stress hydrique',
     'Fertilisation équilibrée',
     'Ventilation en serre']).

prevention(anthracnose_concombre,
    ['Semences traitées ou certifiées',
     'Rotation triennale minimum',
     'Élimination résidus de culture',
     'Éviter travail sur feuillage humide']).

/*
================================================================================
  4. DESCRIPTIONS DES SYMPTÔMES
================================================================================
*/

description_symptome(taches_brunes_feuilles, 
    'Présence de taches brunes sur les feuilles').
description_symptome(aureole_jaune, 
    'Taches entourées d\'une auréole jaune').
description_symptome(flechissement_plante, 
    'Flétrissement de la plante').
description_symptome(jaunissement_unilateral, 
    'Jaunissement d\'un seul côté de la plante').
description_symptome(brunissement_vasculaire, 
    'Brunissement des vaisseaux (coupe de tige)').
description_symptome(poudre_blanche, 
    'Poudre blanche sur les feuilles').
description_symptome(deformation_feuilles, 
    'Déformation et recroquevillement des feuilles').
description_symptome(taches_circulaires_brunes, 
    'Taches circulaires brunes sur feuilles').
description_symptome(cercles_concentriques, 
    'Cercles concentriques visibles dans les taches').
description_symptome(chute_feuilles, 
    'Chute prématurée des feuilles').
description_symptome(pourriture_tubercules, 
    'Pourriture des tubercules/fruits').
description_symptome(lesions_liegeuses_tubercules, 
    'Lésions liégeuses sur tubercules').
description_symptome(croutes_brunes, 
    'Croûtes brunes rugueuses').
description_symptome(pourriture_molle, 
    'Pourriture molle et aqueuse').
description_symptome(mycellium_blanc, 
    'Présence de mycélium blanc cotonneux').
description_symptome(taches_jaunes_feuilles, 
    'Taches jaunes sur feuilles').
description_symptome(duvet_gris, 
    'Duvet gris au revers des feuilles').
description_symptome(taches_jaunes, 
    'Taches jaunes sur feuillage').
description_symptome(taches_circulaires, 
    'Taches circulaires avec centre clair').
description_symptome(lesions_fruits, 
    'Lésions déprimées sur fruits').

/*
================================================================================
  5. DESCRIPTIONS DES CONDITIONS ENVIRONNEMENTALES
================================================================================
*/

description_environnement(humidite_elevee, 
    'Humidité élevée (>80%) ou pluies fréquentes').
description_environnement(temperature_fraiche, 
    'Températures fraîches (10-20°C)').
description_environnement(temps_sec, 
    'Temps sec et chaud').
description_environnement(sol_alcalin, 
    'Sol alcalin (pH > 7)').