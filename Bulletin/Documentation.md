# Documentation brief bulletin

## Jour 1 - Lecture du fichier input

La lecture du fichier a été réalisée à l'aide d'une variable en `PIC X(1000)` afin de s'assurer de lire l'intégralité de toutes les lignes.

L'instruction `RECORD CONTAINS` couplée au `RECORDING MODE IS V` dans la file description `FD` précise que chaque enregistrement dans le fichier est de taille variable (allant de 2 à 1000 caractères).

Les enregistrements sont stockés dans un tableau bidimensionnel (deux dimensions : les étudiants et les cours imbriqués dans le tableau étudiant) dans lequel on trouve les informations suivantes : nom, prénom, âge et moyenne pour chaque élève, ainsi que les noms de matières, les notes, les coefficients, les moyennes par matière. 

Les variables `WS-NBRE-ETUDIANT` et `WS-NBRE-COURS` ont été crées pour délimiter le nombre d'étudiants et de matières selon le fichier en input.

## Jour 2 - Calcul des moyennes par élève et par matière

Création d'une variable permettant de stocker le calcul de note * coeff pour chaque matière

Création d'une variable permettant de stocker le calcul de somme pondérée pour chaque matière

Création d'une variable permettant de stocker le total des coeff

Création d'une variable permettant de stocker la moyenne pour chaque élève

moyenne = (somme pondérée des notes)/total des coeff

Création d'une variable permettant de stocker la moyenne pour chaque matière

Moyenne pour chaque matiere = moyenne de chaque élève / nombre élève

Création d'une variable permettant de stocker le calcul de moyenne matiere * coeff

Création d'une variable permettant de stocker la moyenne de la classe

Moyenne classe = (moyenne matiere * coeff)/ total coeff

