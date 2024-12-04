# Étape 1 : Conception de la structure de base

    Membre 1
        structure pour créer les fonctions
        appels de fonctions via des listes avec opérateur en première position.

    Membre 2
        Gérer les expressions S-expressions : parsing, évaluation.
        Inclure les concepts d'environnement pour les variables et fonctions.

    Membre 3
        Supporter les expressions conditionnelles et leurs évaluations.

    Membre 4
        opérateurs mathématiques (+, -, *, div, mod) et les prédicats (eq?, <).

    Membre 5
        test unitaires (ex : call.scm, lambda.scm, if.scm).
        Valider les comportements de base.

# Étape 2 : Documentation et formalisation

    Membre 2
        Expliquer les concepts de base avec des exemples simples.

    Membre 1
        Formaliser la syntaxe des lambdas, fonctions nommées, conditionnelles.

    Membre 5
        Expliquer la structure interne pour aider à l'extension du projet.

# Étape 3 : Extension syntaxique et grammaire

    Membre 3
        Définir une syntaxe plus lisible (inspirée d'autres langages comme Python, Ruby).

    Membre 4
        Transformer le code source en AST utilisable par l’interpréteur existant.

    Membre 1
        Implémenter un système pour supporter des opérateurs infixes (+, *, etc.).

# Étape 4 : Sécurité et robustesse

    Ajouter des vérifications de type basiques :
        Membre 2
        Implémenter une vérification avant exécution pour éviter les erreurs évidentes (ex : division par zéro).

    Gérer les erreurs et exceptions :
        Membre 5
        Définir une gestion cohérente des erreurs avec des messages clairs.

    Renforcer la sécurité :
        Membre 4
        Identifier et bloquer les cas dangereux (ex : appels infinis).

# Étape 5 : Compilation et exécution sur une VM

    Concevoir une machine virtuelle (VM) :
        Membre 1
        Définir un ensemble d'instructions pour exécuter les programmes.

    Créer un compilateur :
        Membre 3
        Transformer le code source en instructions pour la VM.

    Implémenter une sortie binaire :
        Membre 2
        Générer du bytecode lisible par la VM.

    Ajouter des optimisations :
        Membre 4
        Optimiser la compilation (ex : Tail Call Optimization).

    Documentation du processus de compilation :
        Membre 5
        Décrire chaque étape de la compilation et de l'exécution.

# Étape 6 : Ajout de fonctionnalités avancées

    Types de données supplémentaires :
        Membre 2
        Implémenter des listes, tuples, strings, ou autres types avancés.

    Support des entrées/sorties :
        Membre 3
        Ajouter des fonctions pour lire/écrire depuis les fichiers ou le terminal.

    Interopérabilité (FFI) :
        Membre 4
        Permettre d'appeler des fonctions externes (ex : bibliothèques C).

    Ajouter des fonctionnalités impératives :
        Membre 1
        Introduire des boucles et des variables mutables si nécessaire.

    Documentation des extensions :
        Membre 5
        Ajouter des exemples pour chaque nouvelle fonctionnalité.

# Étape finale : Test, intégration et finalisation

    Responsable global : Toute l'équipe
    Intégrer toutes les parties dans un livrable cohérent.
    Tester l’ensemble du système avec des programmes complexes (factorielles, tris, etc.).
    Préparer une défense orale claire avec des démonstrations.
___________________________________________________________________________

tests unitaire

mega parsec (bootstrap pandoc)

parse int and symbols (anything not numeric) (bootstrap glados)
and lists

handle boolean (#t and #f)

define <SYMBOL> <EXPRESSION>

how to make functions :
    - anonymous/lambdas func
    - named func (example : my_strlen)
    - take parameters
    - recursive

conditional exp (if, then, else)

builtin functions (operators, predicates)

FAUT FAIRE DE LA DOC
___________________________________________________________________________

Moi : lisp -> Sexp
parser une liste -> je récupère les atomes
                    je vois une parenthèse je fais une liste

Ed : Sexp -> AST
