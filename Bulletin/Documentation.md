# Documentation brief bulletin

## Jour 1 - Lecture du fichier input

### Description de la DATA DIVISION

Tout d'abord l'alias du fichier d'entrée à lire est créé. Celui-ci est organisé de manière séquentielle et le `FILE STATUS` définit la variable de contrôle du statut du fichier.

La lecture du fichier a été réalisée à l'aide d'une variable en `PIC X(1000)` afin de s'assurer de lire l'intégralité de toutes les lignes. 
J'ai préféré cette approche pour alléger ma `FILE SECTION`. En effet j'avais commencé par faire plusieurs variables dedans, correspondant aux différents éléments du fichier. Cependant j'ai trouvé que la deuxième approcher apportait plus de souplesse au niveau de la structure de mon fichier, par exemple si je devais lire différents types de lignes et contrôler le découpage de celles-ci.

L'instruction `RECORD CONTAINS` couplée au `RECORDING MODE IS V` dans la file description `FD` précise que chaque enregistrement dans le fichier est de taille variable (allant de 2 à 1000 caractères). J'ai pu découvrir cette instruction grâce aux fichiers fournis dans le brief.

Le booléen `F-STATUT-ENTREE` est la variable définie par le `FILE STATUS` servant à préciser la fin de lecture du fichier. Il prend la valeur `00` pendant la lecture et lorsque la fin du fichier est atteinte, il prend la valeur `10`.
Pareil ici, j'ai découvert cette utilisation du booléen un peu "automatisée" via le brief. En effet j'ai eu du mal à cerner son utilisation au début car je ne connaissais pas le `FILE STATUS` et je pensais devoir gérer les valeurs du booléen moi-même.


Les enregistrements sont stockés dans un tableau bidimensionnel. En effet pour chaque étudiant sont associées les notes dans chaque cours, il y a donc deux dimensions : les étudiants et les cours qui sont imbriqués. Dans ce tableau, on trouve les informations suivantes : nom, prénom, âge et moyenne pour chaque élève, ainsi que les noms de matières, les notes, les coefficients, les moyennes par matière. 
Le nombre d'occurences défini dans le tableau est variable car dépendant du nombre d'étudiants et donc du fichier d'entrée utilisé. C'est à l'aide du `OCCURS ... DEPENDING ON` qu'il est possible de spécifier cela. J'ai découvert ce dernier verbe dans le brief et j'ai essayé de l'utiliser aussi pour le tableau des cours imbriqués dans celui des étudiants, cependant j'ai découvert que ce n'était pas possible. J'ai donc décidé de fixer les occurences pour les cours à 999 pour avoir de la marge, sachant que je peux contrôler mes boucles grâce à des variables de décompte du nombre d'étudiants et du nombre de cours dans le fichier en entrée.

Les variables `WS-NBRE-ETUDIANT` et `WS-NBRE-COURS` ont été créées pour compter et délimiter le nombre d'étudiants et de matières selon le fichier en input.

D'autres variables ont été ajoutées pour le tri manuel des matières et les en-têtes pour l'affichage (afin de pouvoir les réutiliser à chaque affichage).

Plusieurs index ont été créés afin de parcourir le tableau multidimensionnel (`WS-IDX-ETUD` et `WS-IDX-COURS`) et des index supplémentaires ont été ajoutés pour le tri manuel des matières.


### Description de la PROCEDURE DIVISION

Tout d'abord j'ai décidé d'écrire des paragraphes dans ma PROCEDURE DIVISION car je trouve que cela rend mon code plus lisble. En effet les noms de mes paragraphes indiquent bien ce qu'il va se passer et le fait de les voir tous alignés dans la PROCEDURE permet de mieux voir le déroulement du programme.

Le premier paragraphe concerne la lecture du fichier en entrée. On réalise une boucle pour parcourir les enregistrements du fichier à l'aide du booléen qui va indiquer la fin de boucle. Tant qu'on se trouve dans la boucle on va traiter le fichier d'entrée afin de stocker les informations qu'il contient dans le tableau créé en WORKING-STORAGE.
Dans le paragraphe de traitement, j'ai décidé de faire une branche conditionnelle avec un EVALUATE TRUE car je voulais traiter deux cas bien précis: si la clé pour chaque enregistrement du fichier est 01 ou si c'est 02. Lorsque c'est 01 je remplis mes variables du tableau `WS-ETUDIANT` et lorsque c'est 02 je remplis mes variables du tableau `WS-COURS`. On notera que pour cela, j'utilise ma variable en PIC X(1000) avec des index précis pour indiquer les éléments de la chaîne de caractères que je veux ajouter dans les différentes variables. Lors du traitement, je gère aussi mes index et mes variables de décompte pour chaque cas et à chaque enregistrement.

Ensuite vient le paragraphe de tri. Dans celui-ci j'utilise la fonction `SORT` pour trier les étudiants par ordre alphabétique en spécifiant cela avec `ON ASCENDING KEY` (par ordre croissant donc) selon le nom des étudiants.
Cependant je n'arrivais pas à faire de même pour les matières, j'ai donc décidé de les trier manuellement. Pour cela, je me suis créé un index qui serait toujours incrémenté à +1 par rapport à mon index `WS-IDX-COURS`. Cela me permet de comparer les noms de matières à deux positions successives et de le répéter à l'aide d'une boucle. Ensuite, je me sers d'une variable temporaire pour pouvoir échanger les valeurs des variables si nécessaire après comparaison.

Enfin j'ai décidé de me faire un paragraphe d'affichage afin de contrôler si mes données avaient bien été stockées dans mon tableau en WORKING-STORAGE, en parcourant mes valeurs pour chaque matière pour tous les étudiants. J'utilise mes variables d'en-têtes pour rendre l'affichage moins compliqué.


## Jour 2 - Calcul des moyennes par élève et par matière

### Description de la DATA DIVISION

Afin de pouvoir calculer les moyennes des étudiants, des variables permettant de stocker les calculs intermédiaires ont été créées : 

- `WS-NOTE-POND` pour stocker le calcul de note * coefficient pour chaque matière
  
- `WS-SOMME` pour stocker le calcul de somme pondérée des notes par étudiant pour toutes les matières et la somme des notes par matière pour tous les étudiants
  
- `WS-TOT-COEF` pour stocker le total des coefficients
  
- `WS-MOYENNE` pour stocker la moyenne pour chaque élève
  
- `WS-MOY-MAT` pour stocker la moyenne pour chaque matière
  
- `WS-MOY-MAT-POND` pour stocker le calcul de moyenne matière * coefficient
  
- `WS-MOYENNE-CLASSE` pour stocker la moyenne de la classe


### Description de la PROCEDURE DIVISION

Les calculs ont été effectués comme suit :

Moyenne = (Somme pondérée des notes)/Total des coefficients

Moyenne pour chaque matière = Moyenne de chaque élève / Nombre élèves

Moyenne classe = (Moyenne matière * coefficients)/ Total coefficients

Chacun de ces résultats a été arrondi à l'aide du verbe `ROUNDED`.

Au début j'avais du mal à concevoir comment accéder à la moyenne par matière cependant j'ai décidé d'inverser mes boucles par rapport à celles effectuées pour pouvoir déterminer la moyenne par étudiants. Ainsi je peux parcourir les notes de tous les étudiants pour chaque matière et ainsi déterminer la moyenne par matière.

