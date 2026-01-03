# 🚀 Guide Complet d'Installation et Tests - VS Code

## 📋 Table des Matières
1. [Installation SWI-Prolog](#1-installation-swi-prolog)
2. [Configuration VS Code](#2-configuration-vs-code)
3. [Structure du Projet](#3-structure-du-projet)
4. [Lancement du Programme](#4-lancement-du-programme)
5. [Lancement des Tests](#5-lancement-des-tests)
6. [Dépannage](#6-dépannage)

---

## 1. Installation SWI-Prolog

### Windows 🪟

**Étape 1 : Télécharger**
```
https://www.swi-prolog.org/download/stable
→ Télécharger : swipl-9.2.7-1.x64.exe (ou version plus récente)
```

**Étape 2 : Installer**
- Double-cliquer sur le fichier `.exe`
- ✅ **IMPORTANT** : Cocher "Add to PATH" pendant l'installation
- Cliquer sur "Install"

**Étape 3 : Vérifier**
Ouvrir PowerShell ou CMD :
```bash
swipl --version
```

✅ Vous devriez voir : `SWI-Prolog version 9.2.7`

### Linux 🐧

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install swi-prolog

# Fedora
sudo dnf install pl

# Arch
sudo pacman -S swi-prolog

# Vérifier
swipl --version
```

### macOS 🍎

```bash
# Avec Homebrew
brew install swi-prolog

# Vérifier
swipl --version
```

---

## 2. Configuration VS Code

### Installer l'Extension Prolog

**Méthode 1 : Interface graphique**
1. Ouvrir VS Code
2. Cliquer sur l'icône Extensions (Ctrl+Shift+X)
3. Rechercher "Prolog"
4. Installer **"VSC-Prolog"** par Arthur Wang

**Méthode 2 : Ligne de commande**
```bash
code --install-extension arthurwang.vsc-prolog
```

### Configuration Optionnelle

Créer `.vscode/settings.json` dans votre projet :

```json
{
    "prolog.executablePath": "swipl",
    "files.associations": {
        "*.pl": "prolog"
    },
    "terminal.integrated.shell.windows": "powershell.exe"
}
```

---

## 3. Structure du Projet

### Créer l'Arborescence

**Ouvrir le Terminal dans VS Code** : `Ctrl + ù` (ou `Ctrl + `` `)

```bash
# Créer le dossier principal
mkdir sbc-plantes
cd sbc-plantes

# Créer les sous-dossiers
mkdir src tests docs exemples

# Vérifier
ls
# Résultat attendu : docs  exemples  src  tests
```

### Structure Finale

```
sbc-plantes/
│
├── src/
│   ├── systeme_expert.pl         ← Fichier principal
│   ├── base_connaissances.pl     ← Règles et faits
│   ├── moteur_inference.pl       ← Logique
│   └── interface.pl              ← Interface utilisateur
│
├── tests/
│   ├── test_unitaires.pl         ← Tests règles individuelles
│   └── test_integration.pl       ← Tests scénarios complets
│
├── docs/
│   └── rapport.md                ← Rapport du projet
│
└── README.md                     ← Instructions
```

---

## 4. Lancement du Programme

### Méthode 1 : Terminal VS Code (RECOMMANDÉ) ⭐

**Étape 1 : Ouvrir le Terminal**
- Dans VS Code : `Ctrl + ù`

**Étape 2 : Naviguer vers src/**
```bash
cd src
```

**Étape 3 : Lancer Prolog**
```bash
swipl systeme_expert.pl
```

**Étape 4 : Utiliser le système**
```prolog
?- consulter.
```

**📸 Ce que vous devriez voir :**
```
═══════════════════════════════════════════════════
   SYSTÈME EXPERT - DIAGNOSTIC MALADIES PLANTES
═══════════════════════════════════════════════════

Commandes disponibles :
  consulter.         - Lancer diagnostic
  lister_maladies.   - Voir base
  aide.              - Afficher aide

?- consulter.

Quelle plante souhaitez-vous diagnostiquer ?
1. Tomate
2. Pomme de terre
3. Laitue
4. Concombre
Choix (1-4) : 
```

**Étape 5 : Tester un diagnostic complet**

Saisir les réponses suivantes :
```
Choix : 1.

Présence de taches brunes sur les feuilles ? (oui/non) : oui.
Taches entourées d'une auréole jaune ? (oui/non) : oui.
Humidité élevée (>80%) ou pluies fréquentes ? (oui/non) : oui.
Températures fraîches (10-20°C) ? (oui/non) : oui.
```

**✅ Résultat attendu :**
```
===============================================
  DIAGNOSTIC : mildiou_tomate
===============================================

--- TRAITEMENT RECOMMANDÉ ---
  • Appliquer fongicide à base de cuivre (bouillie bordelaise)
  • Espacer les plants pour améliorer aération
  • Éviter arrosage par aspersion
  • Éliminer feuilles infectées
  • Rotation des cultures sur 3-4 ans

--- PRÉVENTION FUTURE ---
  • Choisir variétés résistantes (F1 hybrides)
  ...
```

**Étape 6 : Quitter**
```prolog
?- halt.
```

### Méthode 2 : Extension VS Code

**Étape 1 :** Ouvrir `src/systeme_expert.pl` dans VS Code

**Étape 2 :** Clic droit dans le fichier → **"Load File in Prolog"**

**Étape 3 :** Terminal Prolog s'ouvre automatiquement

**Étape 4 :** Taper `consulter.`

### Méthode 3 : Script de Lancement Automatique

**Pour Windows** - Créer `lancer.bat` :
```batch
@echo off
echo ===============================================
echo   LANCEMENT SYSTÈME EXPERT
echo ===============================================
cd src
swipl -s systeme_expert.pl -g consulter
pause
```

Double-cliquer sur `lancer.bat` pour lancer !

**Pour Linux/Mac** - Créer `lancer.sh` :
```bash
#!/bin/bash
echo "==============================================="
echo "  LANCEMENT SYSTÈME EXPERT"
echo "==============================================="
cd src
swipl -s systeme_expert.pl -g consulter
```

Rendre exécutable et lancer :
```bash
chmod +x lancer.sh
./lancer.sh
```

---

## 5. Lancement des Tests

### A. Tests Unitaires

**Méthode Simple :**

```bash
# Dans le terminal VS Code
cd tests
swipl test_unitaires.pl
```

**✅ Résultat attendu :**
```
═════════════════════════════════════════
  TESTS UNITAIRES - Règles de Diagnostic
═════════════════════════════════════════

% PL-Unit: mildiou_tomate ..... done
% All 5 tests passed

% PL-Unit: fusariose_tomate ... done
% All 3 tests passed

% PL-Unit: oidium_tomate ... done
% All 3 tests passed

% PL-Unit: alternariose_tomate .. done
% All 2 tests passed

% PL-Unit: pomme_terre ... done
% All 3 tests passed

% PL-Unit: laitue .. done
% All 2 tests passed

% PL-Unit: concombre .. done
% All 2 tests passed

═════════════════════════════════════════
✓ TOUS LES TESTS UNITAIRES PASSÉS
  Total : 20 tests
═════════════════════════════════════════
```

**Méthode Interactive :**
```bash
cd tests
swipl

?- [test_unitaires].
?- run_tests.
```

**Tester une catégorie spécifique :**
```prolog
?- test_tomate.      % Tests maladies tomate uniquement
?- test_pdt.         % Tests pomme de terre
?- test_laitue.      % Tests laitue
?- test_concombre.   % Tests concombre
```

### B. Tests d'Intégration

```bash
cd tests
swipl test_integration.pl
```

**✅ Résultat attendu :**
```
═════════════════════════════════════════
  TESTS D'INTÉGRATION - Scénarios Complets
═════════════════════════════════════════

% PL-Unit: scenarios_complets ...... done
% All 6 tests passed

% PL-Unit: coherence_base .... done
% All 4 tests passed

% PL-Unit: performance ... done
% All 3 tests passed

% PL-Unit: robustesse ... done
% All 3 tests passed

% PL-Unit: cas_reels ..... done
% All 5 tests passed

═════════════════════════════════════════
✓ TOUS LES TESTS D'INTÉGRATION PASSÉS
  Total : 21 tests
═════════════════════════════════════════
```

### C. Lancer TOUS les Tests

**Créer un script** `tests/lancer_tous_tests.pl` :

```prolog
:- [test_unitaires].
:- [test_integration].

tous_les_tests :-
    write('═══════════════════════════════════════════'), nl,
    write('  SUITE COMPLÈTE DE TESTS'), nl,
    write('═══════════════════════════════════════════'), nl, nl,
    
    write('→ Tests Unitaires...'), nl,
    run_tests,
    nl,
    
    write('→ Tests d\'Intégration...'), nl,
    [test_integration],
    run_tests,
    nl,
    
    write('═══════════════════════════════════════════'), nl,
    write('  ✓ TOUS LES TESTS TERMINÉS'), nl,
    write('═══════════════════════════════════════════'), nl.

:- initialization(tous_les_tests, main).
```

**Lancer :**
```bash
cd tests
swipl lancer_tous_tests.pl
```

---

## 6. Dépannage

### Problème 1 : "swipl: command not found"

**❌ Symptôme :**
```bash
$ swipl
bash: swipl: command not found
```

**✅ Solution Windows :**
1. Réinstaller SWI-Prolog et **cocher "Add to PATH"**
2. OU ajouter manuellement :
   - Ouvrir "Variables d'environnement"
   - Ajouter `C:\Program Files\swipl\bin` au PATH
3. Redémarrer VS Code

**✅ Solution Linux/Mac :**
```bash
# Vérifier où est installé swipl
which swipl

# Si non trouvé, réinstaller
sudo apt-get install swi-prolog  # Linux
brew install swi-prolog          # Mac
```

### Problème 2 : "File not found: systeme_expert.pl"

**❌ Symptôme :**
```prolog
?- [systeme_expert].
ERROR: source_sink `systeme_expert.pl' does not exist
```

**✅ Solution :**
```bash
# Vérifier le répertoire actuel
pwd

# Vous devez être dans sbc-plantes/src/
# Si vous n'y êtes pas :
cd src

# Vérifier que le fichier existe
ls *.pl
# Doit afficher : systeme_expert.pl ...

# Relancer
swipl systeme_expert.pl
```

### Problème 3 : "Undefined procedure"

**❌ Symptôme :**
```prolog
?- consulter.
ERROR: Undefined procedure: consulter/0
```

**✅ Solution :**

**Option A :** Le fichier n'est pas chargé correctement
```prolog
?- [systeme_expert].
true.

?- consulter.
% Devrait fonctionner maintenant
```

**Option B :** Vérifier que `systeme_expert.pl` contient bien :
```prolog
:- ['base_connaissances.pl'].
:- ['moteur_inference.pl'].
:- ['interface.pl'].
```

### Problème 4 : Erreurs de Syntaxe

**❌ Symptôme :**
```
ERROR: Syntax error: Operator expected
```

**✅ Solution :**
1. Regarder le **numéro de ligne** indiqué dans l'erreur
2. Vérifier :
   - Tous les points `.` à la fin des règles
   - Correspondance des parenthèses
   - Guillemets corrects `'...'`

**Exemple d'erreur courante :**
```prolog
% ❌ INCORRECT (manque le point)
maladie(mildiou_tomate, tomate) :-
    symptome(taches_brunes, oui)

% ✅ CORRECT
maladie(mildiou_tomate, tomate) :-
    symptome(taches_brunes, oui).
```

### Problème 5 : Tests Échouent

**❌ Symptôme :**
```
% PL-Unit: mildiou_tomate . FAILED
```

**✅ Solution :**

1. **Lancer le test en mode verbose :**
```prolog
?- run_tests(mildiou_tomate, [verbose(true)]).
```

2. **Tester la règle manuellement :**
```prolog
?- assertz(symptome(taches_brunes_feuilles, oui)),
   assertz(symptome(aureole_jaune, oui)),
   assertz(environnement(humidite_elevee, oui)),
   assertz(environnement(temperature_fraiche, oui)),
   maladie(M, tomate).

M = mildiou_tomate.  % ← Devrait afficher ceci
```

3. **Vérifier que le chemin de chargement est correct dans le test :**
```prolog
% Dans test_unitaires.pl, vérifier :
:- ['../src/systeme_expert.pl'].  % Bon chemin relatif
```

### Problème 6 : Caractères Bizarres (é, à, etc.)

**❌ Symptôme :**
```
Mildiou de la tomate → Mildiou de la tomateâ¬
```

**✅ Solution :**
```prolog
% Au début de votre fichier Prolog
:- set_prolog_flag(encoding, utf8).
```

---

## 📊 Checklist Complète de Vérification

Avant de rendre le projet, vérifier :

### Installation
- [ ] `swipl --version` fonctionne
- [ ] VS Code installé avec extension Prolog
- [ ] Structure de dossiers créée correctement

### Programme Principal
- [ ] `cd src && swipl systeme_expert.pl` se lance
- [ ] `?- consulter.` démarre le diagnostic
- [ ] Diagnostic mildiou fonctionne (4 symptômes → résultat)
- [ ] `?- lister_maladies.` affiche 10 maladies
- [ ] `?- halt.` quitte proprement

### Tests
- [ ] `cd tests && swipl test_unitaires.pl` → 20+ tests passent
- [ ] `swipl test_integration.pl` → 20+ tests passent
- [ ] Aucun test en échec (FAILED)
- [ ] Temps d'exécution < 5 secondes

### Documentation
- [ ] README.md créé avec instructions
- [ ] Rapport complet (10 sections)
- [ ] Code commenté et indenté

---

## 🎬 Démo Rapide (2 minutes)

**Script de démonstration pour le prof :**

```bash
# 1. Montrer l'installation
swipl --version
# → "SWI-Prolog version 9.2.7"

# 2. Lancer le système
cd sbc-plantes/src
swipl systeme_expert.pl

# 3. Faire un diagnostic
?- consulter.
# Répondre : 1 (tomate), oui, oui, oui, oui
# → Diagnostic mildiou s'affiche

# 4. Montrer les tests
?- halt.
cd ../tests
swipl test_unitaires.pl
# → Tous les tests passent en vert

# 5. Conclure
"Le système est fonctionnel et validé avec 40+ tests automatisés"
```

---

## 💡 Commandes Utiles

### Pendant le Développement

```prolog
% Recharger le fichier sans quitter Prolog
?- make.

% Tracer l'exécution (debug)
?- trace.
?- consulter.
% Voir chaque étape de l'exécution

% Arrêter le trace
?- notrace.

% Lister tous les prédicats définis
?- listing(maladie).

% Vérifier si une règle existe
?- clause(maladie(mildiou_tomate, tomate), Body).
```

### Raccourcis VS Code

- **Ctrl + ù** : Ouvrir/Fermer terminal
- **Ctrl + Shift + P** : Palette de commandes
- **Ctrl + /** : Commenter/Décommenter
- **Ctrl + Space** : Auto-complétion
- **F1** : Aide

---

## 📞 Besoin d'Aide ?

Si vous êtes bloqué à une étape spécifique, notez :
1. **Le message d'erreur exact**
2. **La commande que vous avez tapée**
3. **Le contenu de `pwd`** (répertoire actuel)

---

**Bon courage ! 🚀 Avec ce guide, vous avez tout pour réussir !**