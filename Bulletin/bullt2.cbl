      *Brief: Lecture d'un fichier input contenant les noms, prénoms, notes par matière d'étudiants.
      *A partir de ce fichier, calculer la moyenne par étudiant et la 
      *la moyenne générale par matière en tenant compte des différents coefficients par matière
      *Générer un fichier avec les noms, notes, moyenne et appréciations


      ****************************************************************** 
      *                   IDENTIFICATION DIVISION                      *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bullt2.
       AUTHOR. ThomasD.

      ****************************************************************** 
      *                    ENVIRONMENT DIVISION                        *
      ****************************************************************** 
       
       ENVIRONMENT DIVISION.      
        
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *Définition de la décimale des nombres en tant que virgule et non
      *en tant que point 
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------- 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *Création de l'alias pour le fichier d'entrée 
      *Les enregistrements des fichiers sont définis de manière séquentielle pour le traitement 
      *Les enregistrements suivent une organisation séquentielle (ligne par ligne)
      *Création d'une variable de contrôle du statut du fichier 


           SELECT FICHIER-ENTREE ASSIGN TO 'input.dat'
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS F-STATUT-ENTREE.     
                   
       
           SELECT FICHIER-SORTIE
           ASSIGN TO 'output.dat'
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS F-STATUT-SORTIE.     



      ****************************************************************** 
      *                        DATA DIVISION                           *
      ****************************************************************** 
       DATA DIVISION.

       FILE SECTION.

      *Description du fichier: les enregistrements sont de taille variable
      *Ils peuvent contenir 2 à 1000 caractères. 
       FD  FICHIER-ENTREE
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.
       
      *    01  F-ENTREE-2       PIC 9(02).
      *    01  F-ENTREE-10      PIC X(10).
      *    01  F-ENTREE-100     PIC X(100).
           01  F-ENTREE-1000    PIC X(1000).
              
       
       FD  FICHIER-SORTIE
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.   

           01 F-SORTIE        PIC X(250).
      *    
         

      *----------------------------------------------------------------- 
       WORKING-STORAGE SECTION.

      *Définition de la variable de contrôle de statut du fichier 
      *Utile pour définir la fin de boucle de lecture du fichier 
       01  F-STATUT-ENTREE      PIC X(02) VALUE SPACE.
           88 F-STATUT-ENTREE-OK    VALUE '00'.        
           88 F-STATUT-ENTREE-FF    VALUE '10'.

       01  F-STATUT-SORTIE     PIC X(02) VALUE SPACE.
           88 F-STATUT-SORTIE-OK    VALUE '00'.        
           88 F-STATUT-SORTIE-FF    VALUE '10'.


      *Création du tableau bidimensionnel de stockage des informations contenues dans le fichier
      *Celui-ci sera de taille variable selon le nombre d'étudiants et le nombre de cours 
       01  WS-DONNEE-ETUDIANT.
           05 WS-NBRE-ETUDIANT    PIC 9(03)        VALUE 0.
           05 WS-NBRE-COURS       PIC 9(03)        VALUE 0.
           05 WS-NBRE-NOTES       PIC 9(03)        VALUE 0.

           05 WS-ETUDIANT      OCCURS 1 TO 999 TIMES 
                               DEPENDING ON WS-NBRE-ETUDIANT.
               
               10 WS-NOM          PIC X(07).
               10 WS-PRENOM       PIC X(06).      
               10 WS-AGE          PIC 9(02).

      *Création d'une variable permettant de stocker la moyenne pour chaque élève 
               10 WS-MOYENNE      PIC 99V99.
               10 WS-MOYENNE-ED   PIC 99,99.
               10 WS-COURS     OCCURS 999 TIMES. 
                               

                 15 WS-MATIERE    PIC X(21).
                 15 WS-COEF       PIC 9V9.
                 15 WS-COEF-ED    PIC 9,9.
                 15 WS-NOTE       PIC 99V99.   
                 15 WS-NOTE-ED    PIC 99,99.     

      *Création d'une variable permettant de stocker le calcul de note * coeff pour chaque matière
                 15 WS-NOTE-POND  PIC 99V999.

      *Création d'une variable permettant de stocker la moyenne pour chaque matière             
                 15 WS-MOY-MAT    PIC 99V99.
                 15 WS-MOY-MAT-ED PIC 99,99.
       
      *Création d'une variable permettant de stocker le calcul de moyenne * coeff pour chaque matière
                 15 WS-MOY-MAT-POND   PIC 99V999. 

      *Création d'une variable permettant de stocker
      *les calculs de somme pondérée des notes par étudiant pour toutes les matières
      *et de somme des notes par matière pour tous les étudiants  
       01 WS-SOMME                PIC 999V999.

      *Création d'une variable permettant de stocker le total des coeff 
       01 WS-TOT-COEF             PIC 9V9. 
       
      *Création d'une variable pour stocker la moyenne de classe  
       01 WS-MOYENNE-CLASSE       PIC 99V99.
       01 WS-MOYENNE-CLASSE-ED    PIC 99,99.

      *Création de variables de stockage temporaire pour le tri des matières 
       01 WS-MATIERE-TEMPO        PIC X(21).
       01 WS-NOTE-TEMPO           PIC 99V99.
       01 WS-COEF-TEMPO           PIC 9V9.
       
      *Création de variables d'en-tête pour l'affichage et l'écriture
       01 WS-ENTETE-NOM           PIC X(07)   VALUE "NOM".
       01 WS-ENTETE-PRENOM        PIC X(08)   VALUE "PRENOM".
       01 WS-ENTETE-AGE           PIC X(03)   VALUE "AGE".    
       01 WS-ENTETE-MAT           PIC X(21)   VALUE "MATIERE".     
       01 WS-ENTETE-COEF          PIC X(05)   VALUE "COEF". 
       01 WS-ENTETE-NOTE          PIC X(04)   VALUE "NOTE". 
       01 WS-ENTETE-MOYENNE       PIC X(07)   VALUE "MOYENNE". 
       01 WS-ENTETE-MOY-CLS       PIC X(16)   VALUE "MOYENNE CLASSE". 
       01 WS-ETOILE               PIC X(31)   VALUE ALL "*".
       01 WS-TIRET                PIC X(31)   VALUE ALL "-".
   
      *Création de variables d'en-tête pour l'écriture 
       01 WS-ENT-ECRI-NOM           PIC X(18)   VALUE "NOM".
       01 WS-ENT-ECRI-PRENOM        PIC X(18)   VALUE "PRENOM".
       01 WS-ENT-ECRI-AGE           PIC X(23)   VALUE "AGE".    
      *01 WS-ENT-ECRI-MAT           PIC X(21)   VALUE "MATIERE".     
       01 WS-ENT-ECRI-COEF          PIC X(04)   VALUE "COEF". 
       01 WS-ENT-ECRI-NOTE          PIC X(04)   VALUE "NOTE". 
       01 WS-ENT-ECRI-MOYENNE       PIC X(30)   VALUE "MOYENNE". 
      *01 WS-ENT-ECRI-MOY-CLS       PIC X(16)   VALUE "MOYENNE CLASSE".
       01 WS-ENT-ECRI-CLASSE        PIC X(06)   VALUE "CLASSE".
       01 WS-BULL                 PIC X(108)  VALUE "BULLETIN DE NOTES".
       01 WS-BAS-PAGE             PIC X(108)  VALUE "FIN DE RAPPORT".   

       01 WS-ENTETE-COURS     OCCURS 6 TIMES.
           05 WS-ENT-COURS-NUM      PIC X(06).

       01 WS-LIBELLE             PIC X(10).  
       01 WS-LIBELLE-NUM         PIC 9(01).

      *Création de variables filler pour espacer les données de l'output

       01 WS-FILLER-NOM          PIC X(11). 
       01 WS-FILLER-PRENOM       PIC X(12).
       01 WS-FILLER-AGE          PIC X(22).
       01 WS-FILLER-COURS        PIC X(24).
       01 WS-FILLER-NOTE         PIC X(25).
       01 WS-FILLER-CLASSE       PIC X(54).
      
      *Création d'index pour parcourir le tableau selon les dimensions
       77 WS-IDX-ETUD            PIC 9(03)        VALUE 0.      
       77 WS-IDX-COURS           PIC 9(03)        VALUE 0.
       
       77 WS-IDX-COURS2          PIC 9(03)        VALUE 0.
       77 WS-IDX-INCREMENT       PIC 9(03)        VALUE 0.


      *01 WS-DUMMY           PIC X.
      ****************************************************************** 
      *                      PROCEDURE DIVISION                        *
      ****************************************************************** 
       PROCEDURE DIVISION.

       PERFORM 0100-LECTURE-DEBUT
          THRU 0100-LECTURE-FIN.
 
       PERFORM 0200-MOYENNE-ETU-DEBUT
          THRU 0200-MOYENNE-ETU-FIN.
 
       PERFORM 0300-MOYENNE-MAT-DEBUT
          THRU 0300-MOYENNE-MAT-FIN.  

       PERFORM 0400-ECRITURE-DEBUT
          THRU 0400-ECRITURE-FIN.

       STOP RUN. 


      ****************************************************************** 
      *                          PARAGRAPHES                           *
      ****************************************************************** 

       0100-LECTURE-DEBUT.

       DISPLAY "Ouverture du fichier :".
       OPEN INPUT FICHIER-ENTREE.

       

       PERFORM UNTIL F-STATUT-ENTREE-FF

        READ FICHIER-ENTREE
           AT END 
             
             DISPLAY "Fin de fichier atteinte."
             DISPLAY "Statut : " F-STATUT-ENTREE 
             DISPLAY "Lecture du fichier :"

           NOT AT END 
             PERFORM 0110-TRAITEMENT-LECT-DEBUT
               THRU  0110-TRAITEMENT-LECT-FIN 
               
   
        END-READ 
       END-PERFORM.

       DISPLAY "Fin de lecture du fichier.".

       CLOSE FICHIER-ENTREE.
       DISPLAY "Fermeture du fichier.".

       
       PERFORM 0120-TRI-DEBUT
          THRU 0120-TRI-FIN.

       PERFORM 0130-AFFICHE-DEBUT
          THRU 0130-AFFICHE-FIN.

       0100-LECTURE-FIN.
       EXIT.

      *-----------------------------------------------------------------
       0110-TRAITEMENT-LECT-DEBUT.
       EVALUATE TRUE 
           WHEN F-ENTREE-1000(1:2) = 01 
            MOVE 0 TO WS-NBRE-COURS
            MOVE 0 TO WS-IDX-COURS
       
            ADD 1 TO WS-IDX-ETUD
            ADD 1 TO WS-NBRE-ETUDIANT

            MOVE F-ENTREE-1000(3:7) TO WS-NOM(WS-IDX-ETUD)
            MOVE F-ENTREE-1000(10:6) TO WS-PRENOM(WS-IDX-ETUD)
            MOVE F-ENTREE-1000(16:2)TO WS-AGE(WS-IDX-ETUD)
       
            
            
           WHEN F-ENTREE-1000(1:2) = 02 
              
            ADD 1 TO WS-IDX-COURS      
            ADD 1 TO WS-NBRE-COURS

            MOVE F-ENTREE-1000(3:21)
            TO   WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS)
       
            MOVE F-ENTREE-1000(24:3)
            TO WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)

           
            MOVE F-ENTREE-1000(27:5)
            TO WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
                 
                
       END-EVALUATE.


       0110-TRAITEMENT-LECT-FIN.
       EXIT.

      *----------------------------------------------------------------- 
       0120-TRI-DEBUT.

       SORT WS-ETUDIANT ON ASCENDING KEY WS-NOM.

       
       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
               UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT
           
           PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS - 1

               MOVE WS-IDX-COURS TO WS-IDX-INCREMENT
               ADD 1 TO WS-IDX-INCREMENT
               
               PERFORM VARYING WS-IDX-COURS2 FROM WS-IDX-INCREMENT BY 1 
               UNTIL WS-IDX-COURS2 > WS-NBRE-COURS


                   IF WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS) > 
                      WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS2) 

                       PERFORM 0125-ECHANGE-TRI-DEBUT
                          THRU 0125-ECHANGE-TRI-FIN

                   END-IF 
               END-PERFORM 
           END-PERFORM 

       END-PERFORM.


       0120-TRI-FIN.
       EXIT.

      *-----------------------------------------------------------------
       0125-ECHANGE-TRI-DEBUT.

       MOVE WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS)
       TO   WS-MATIERE-TEMPO.

       MOVE WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
       TO   WS-NOTE-TEMPO.

       MOVE WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)
       TO   WS-COEF-TEMPO.

       MOVE WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS2)
       TO   WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS).

       MOVE WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS2)
       TO   WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS).

       MOVE WS-COEF(WS-IDX-ETUD,WS-IDX-COURS2)
       TO   WS-COEF(WS-IDX-ETUD,WS-IDX-COURS).

       MOVE WS-MATIERE-TEMPO
       TO   WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS2).

       MOVE WS-NOTE-TEMPO
       TO   WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS2).

       MOVE WS-COEF-TEMPO
       TO   WS-COEF(WS-IDX-ETUD,WS-IDX-COURS2).


       0125-ECHANGE-TRI-FIN.
       EXIT.
      *-----------------------------------------------------------------
       0130-AFFICHE-DEBUT.

       DISPLAY "Affichage du fichier stocké :".

       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
               UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT

           DISPLAY WS-ETOILE

           DISPLAY WS-ENTETE-NOM WITH NO ADVANCING
                   WS-ENTETE-PRENOM WITH NO ADVANCING
                   WS-ENTETE-AGE

           
           DISPLAY WS-NOM(WS-IDX-ETUD)  
           SPACES WITH NO ADVANCING 
                       WS-PRENOM(WS-IDX-ETUD)
           SPACES WITH NO ADVANCING 
                       WS-AGE(WS-IDX-ETUD)
                       
           DISPLAY WS-ETOILE

           PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS 

               DISPLAY WS-TIRET
               DISPLAY WS-ENTETE-MAT WITH NO ADVANCING
                       WS-ENTETE-COEF WITH NO ADVANCING
                       WS-ENTETE-NOTE
       
               
               DISPLAY WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS)
               SPACES WITH NO ADVANCING 
                       WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)
               SPACES WITH NO ADVANCING 
                       WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)

               DISPLAY WS-TIRET
           END-PERFORM 
       END-PERFORM.

       0130-AFFICHE-FIN.
       EXIT.

      *-----------------------------------------------------------------
       0200-MOYENNE-ETU-DEBUT.
      
       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
               UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT
        
            
           
        MOVE 0 TO WS-SOMME 
        MOVE 0 TO WS-TOT-COEF
      
        PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
            UNTIL WS-IDX-COURS > WS-NBRE-COURS 

           
           COMPUTE WS-NOTE-POND(WS-IDX-ETUD,WS-IDX-COURS) =
                   WS-COEF(WS-IDX-ETUD,WS-IDX-COURS) * 
                   WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
           
           

           ADD WS-NOTE-POND(WS-IDX-ETUD,WS-IDX-COURS)
           TO  WS-SOMME

           
           ADD WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)
           TO  WS-TOT-COEF 
           

        END-PERFORM 
     
       
        COMPUTE WS-MOYENNE(WS-IDX-ETUD) ROUNDED =
                WS-SOMME / WS-TOT-COEF

               
       END-PERFORM.
      
       PERFORM 0210-AFFICHE-MOY-ETUD-DEBUT
          THRU 0210-AFFICHE-MOY-ETUD-FIN.


       0200-MOYENNE-ETU-FIN.
       EXIT.

      *-----------------------------------------------------------------

       0210-AFFICHE-MOY-ETUD-DEBUT.
       DISPLAY "Affichage des moyennes par étudiant :".

       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
               UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT

           DISPLAY WS-ETOILE

           DISPLAY WS-ENTETE-NOM WITH NO ADVANCING
                   WS-ENTETE-PRENOM WITH NO ADVANCING
                   WS-ENTETE-AGE

           
           DISPLAY WS-NOM(WS-IDX-ETUD)  
           SPACES WITH NO ADVANCING 
                       WS-PRENOM(WS-IDX-ETUD)
           SPACES WITH NO ADVANCING 
                       WS-AGE(WS-IDX-ETUD)
                       
           DISPLAY WS-ETOILE

           DISPLAY WS-ENTETE-MOYENNE
           DISPLAY WS-MOYENNE(WS-IDX-ETUD)
            
           DISPLAY WS-TIRET
      
       END-PERFORM.
       
       0210-AFFICHE-MOY-ETUD-FIN.
       EXIT.

      *-----------------------------------------------------------------
       0300-MOYENNE-MAT-DEBUT.
      
       
       PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS
           
           MOVE 0 TO WS-SOMME
           PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
                   UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT

      *    DISPLAY WS-NOTE(WS-IDX-ETUD, WS-IDX-COURS)
      *    ACCEPT WS-DUMMY
               ADD WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
               TO  WS-SOMME
      *        DISPLAY "somme/matiere" WS-SOM-NOTE-MAT
           END-PERFORM     
           COMPUTE WS-MOY-MAT(WS-IDX-ETUD,WS-IDX-COURS) ROUNDED = 
                   WS-SOMME / WS-NBRE-ETUDIANT
      *    DISPLAY "moy/mat" WS-MOY-MAT(WS-IDX-ETUD,WS-IDX-COURS)
            
       END-PERFORM.


       PERFORM 0310-MOYENNE-CLASSE-DEBUT
          THRU 0310-MOYENNE-CLASSE-FIN.

       PERFORM 0320-AFFICHE-MOY-MAT-DEBUT
          THRU 0320-AFFICHE-MOY-MAT-FIN.

       0300-MOYENNE-MAT-FIN.
       EXIT.
       

      *-----------------------------------------------------------------
       
       0310-MOYENNE-CLASSE-DEBUT.

       MOVE 0 TO WS-SOMME.
       MOVE 0 TO WS-TOT-COEF.
       PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS
           

           COMPUTE WS-MOY-MAT-POND(WS-IDX-ETUD,WS-IDX-COURS) =
                   WS-COEF(1,WS-IDX-COURS) * 
                   WS-MOY-MAT(WS-IDX-ETUD,WS-IDX-COURS)
           
           

           ADD WS-MOY-MAT-POND(WS-IDX-ETUD,WS-IDX-COURS)
           TO  WS-SOMME
      *    DISPLAY WS-SOMME
           
           ADD WS-COEF(1,WS-IDX-COURS)
           TO  WS-TOT-COEF 
      *    DISPLAY WS-TOT-COEF 


       END-PERFORM.

       COMPUTE WS-MOYENNE-CLASSE ROUNDED = WS-SOMME / WS-TOT-COEF.
                


       0310-MOYENNE-CLASSE-FIN.
       EXIT.

      *-----------------------------------------------------------------
       0320-AFFICHE-MOY-MAT-DEBUT.
       
       DISPLAY "Affichage des moyennes par matière : ".
       DISPLAY WS-ETOILE.
       
       DISPLAY WS-ENTETE-MAT
       SPACES WITH NO ADVANCING. 
       DISPLAY WS-ENTETE-MOYENNE.

       DISPLAY WS-ETOILE.

       PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS


           DISPLAY WS-MATIERE(1,WS-IDX-COURS)
           SPACES WITH NO ADVANCING
           DISPLAY WS-MOY-MAT(WS-IDX-ETUD,WS-IDX-COURS)
            
           DISPLAY WS-TIRET

       END-PERFORM.

       DISPLAY WS-ENTETE-MOY-CLS WS-MOYENNE-CLASSE.

       0320-AFFICHE-MOY-MAT-FIN.
       EXIT. 


      *----------------------------------------------------------------- 
       0400-ECRITURE-DEBUT.
       DISPLAY "Ouverture du fichier : ".
       OPEN OUTPUT FICHIER-SORTIE.
       DISPLAY "Ecriture du fichier : ".
       
      *Ecriture de l'en-tête du fichier de sortie 
       PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
           UNTIL WS-IDX-COURS > WS-NBRE-COURS
           
           MOVE WS-IDX-COURS TO WS-LIBELLE-NUM

           STRING 
           "COURS" DELIMITED BY SIZE 
           WS-LIBELLE-NUM DELIMITED BY SIZE
           INTO WS-LIBELLE
           END-STRING 

           MOVE WS-LIBELLE TO WS-ENT-COURS-NUM(WS-IDX-COURS)
       END-PERFORM. 



       MOVE ALL "*" TO F-SORTIE.
       WRITE F-SORTIE.
       
       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE SPACES TO F-SORTIE.
       MOVE WS-BULL TO F-SORTIE(108:17).
       WRITE F-SORTIE.

       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE ALL "*" TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       STRING 
           WS-ENT-ECRI-NOM DELIMITED BY SIZE 
           WS-ENT-ECRI-PRENOM DELIMITED BY SIZE 
           WS-ENT-ECRI-AGE DELIMITED BY SIZE 

           WS-ENT-COURS-NUM(1) DELIMITED BY SIZE
           WS-FILLER-COURS DELIMITED BY SIZE

           WS-ENT-COURS-NUM(2) DELIMITED BY SIZE
           WS-FILLER-COURS DELIMITED BY SIZE

           WS-ENT-COURS-NUM(3) DELIMITED BY SIZE
           WS-FILLER-COURS DELIMITED BY SIZE

           WS-ENT-COURS-NUM(4) DELIMITED BY SIZE
           WS-FILLER-COURS DELIMITED BY SIZE

           WS-ENT-COURS-NUM(5) DELIMITED BY SIZE
           WS-FILLER-COURS DELIMITED BY SIZE

           WS-ENT-COURS-NUM(6) DELIMITED BY SIZE
           WS-FILLER-COURS DELIMITED BY SIZE

           WS-ENT-ECRI-MOYENNE DELIMITED BY SIZE
       INTO F-SORTIE
       END-STRING. 

     
       WRITE F-SORTIE.

       MOVE ALL "_" TO F-SORTIE.
       WRITE F-SORTIE.

      *Alimentation des variables d'édition pour l'écriture des variables numériques
       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
               UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT 

           

           PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
                   UNTIL WS-IDX-COURS > WS-NBRE-COURS

               MOVE WS-NOTE(WS-IDX-ETUD WS-IDX-COURS)
               TO   WS-NOTE-ED(WS-IDX-ETUD WS-IDX-COURS)
       
               MOVE WS-COEF(WS-IDX-ETUD WS-IDX-COURS)
               TO   WS-COEF-ED(WS-IDX-ETUD WS-IDX-COURS)

           END-PERFORM 

           MOVE WS-MOYENNE(WS-IDX-ETUD) 
           TO   WS-MOYENNE-ED(WS-IDX-ETUD)
       END-PERFORM.


       PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS 


               MOVE WS-MOY-MAT(WS-IDX-ETUD WS-IDX-COURS)
               TO   WS-MOY-MAT-ED(WS-IDX-ETUD WS-IDX-COURS)
               
       END-PERFORM.


       MOVE WS-MOYENNE-CLASSE TO WS-MOYENNE-CLASSE-ED.

      *Ecriture des données du fichier : nom, prénom, âge, notes, moyenne par étudiant 
       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
           UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT 
           
           MOVE ALL SPACES TO F-SORTIE
           WRITE F-SORTIE
           
           STRING 
            WS-NOM(WS-IDX-ETUD) DELIMITED BY SIZE 
            WS-FILLER-NOM DELIMITED BY SIZE

            WS-PRENOM(WS-IDX-ETUD) DELIMITED BY SIZE 
            WS-FILLER-PRENOM DELIMITED BY SIZE

            WS-AGE(WS-IDX-ETUD) DELIMITED BY SIZE 
            WS-FILLER-AGE DELIMITED BY SIZE

            WS-NOTE-ED(WS-IDX-ETUD 1) DELIMITED BY SIZE
            WS-FILLER-NOTE DELIMITED BY SIZE

            WS-NOTE-ED(WS-IDX-ETUD 2) DELIMITED BY SIZE
            WS-FILLER-NOTE DELIMITED BY SIZE

            WS-NOTE-ED(WS-IDX-ETUD 3) DELIMITED BY SIZE
            WS-FILLER-NOTE DELIMITED BY SIZE

            WS-NOTE-ED(WS-IDX-ETUD 4) DELIMITED BY SIZE
            WS-FILLER-NOTE DELIMITED BY SIZE

            WS-NOTE-ED(WS-IDX-ETUD 5) DELIMITED BY SIZE
            WS-FILLER-NOTE DELIMITED BY SIZE

            WS-NOTE-ED(WS-IDX-ETUD 6) DELIMITED BY SIZE
            WS-FILLER-NOTE DELIMITED BY SIZE

            WS-MOYENNE-ED(WS-IDX-ETUD) DELIMITED BY SIZE
            
           INTO F-SORTIE
           END-STRING

         WRITE F-SORTIE
       END-PERFORM.
       
       MOVE ALL "-" TO F-SORTIE.
       WRITE F-SORTIE.
       
       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       
      *Ecriture de la ligne des moyennes par matière et de la moyenne de classe 
       STRING 
        
           WS-ENT-ECRI-CLASSE DELIMITED BY SIZE
           WS-FILLER-CLASSE   DELIMITED BY SIZE
           
           WS-MOY-MAT-ED(WS-IDX-ETUD 1) DELIMITED BY SIZE
           WS-FILLER-NOTE DELIMITED BY SIZE
       
           WS-MOY-MAT-ED(WS-IDX-ETUD 2) DELIMITED BY SIZE
           WS-FILLER-NOTE DELIMITED BY SIZE
       
           WS-MOY-MAT-ED(WS-IDX-ETUD 3) DELIMITED BY SIZE
           WS-FILLER-NOTE DELIMITED BY SIZE
       
           WS-MOY-MAT-ED(WS-IDX-ETUD 4) DELIMITED BY SIZE
           WS-FILLER-NOTE DELIMITED BY SIZE
       
           WS-MOY-MAT-ED(WS-IDX-ETUD 5) DELIMITED BY SIZE
           WS-FILLER-NOTE DELIMITED BY SIZE
       
           WS-MOY-MAT-ED(WS-IDX-ETUD 6) DELIMITED BY SIZE
           WS-FILLER-NOTE DELIMITED BY SIZE
       
           
           WS-MOYENNE-CLASSE-ED DELIMITED BY SIZE

       INTO F-SORTIE 
       END-STRING.
       
       WRITE F-SORTIE.
       
       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE ALL "*" TO F-SORTIE.
       WRITE F-SORTIE.

      *Ecriture de la légende (cours et coefficients) 
      *MOVE ALL SPACES TO F-SORTIE.
      *WRITE F-SORTIE.
       
       PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
               UNTIL WS-IDX-COURS > WS-NBRE-COURS

           MOVE ALL SPACES TO F-SORTIE
           WRITE F-SORTIE

           STRING 
               
       
             WS-ENT-COURS-NUM(WS-IDX-COURS) DELIMITED BY SIZE
             ": "
             WS-MATIERE(1 WS-IDX-COURS) DELIMITED BY SIZE

             WS-FILLER-NOM DELIMITED BY SIZE
     
             WS-ENT-ECRI-COEF DELIMITED BY SIZE
             ": "
             WS-COEF-ED(1 WS-IDX-COURS) DELIMITED BY SIZE
       
              
           INTO F-SORTIE
           END-STRING
           
           WRITE F-SORTIE 
       END-PERFORM.
       
       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE ALL "*" TO F-SORTIE.
       WRITE F-SORTIE.

      *Ecriture des statistiques sur le nombre d'élèves, de notes et de cours 
      
       COMPUTE WS-NBRE-NOTES = WS-NBRE-ETUDIANT * WS-NBRE-COURS.
       

       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       STRING      
         "NOMBRE D'ETUDIANTS : "
         WS-NBRE-ETUDIANT 
       INTO F-SORTIE 
       END-STRING.

       WRITE F-SORTIE.

       
       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       STRING     
         "NOMBRE DE COURS : "
         WS-NBRE-COURS 
       INTO F-SORTIE 
       END-STRING.

       WRITE F-SORTIE. 


       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       STRING         
         "NOMBRE DE NOTES : "
         WS-NBRE-NOTES 
       INTO F-SORTIE 
       END-STRING.
       
       WRITE F-SORTIE.

      *Ecriture du bas de page 
       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.
       
       MOVE ALL "*" TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.
       MOVE WS-BAS-PAGE TO F-SORTIE(108:14).
       WRITE F-SORTIE.


       MOVE ALL SPACES TO F-SORTIE.
       WRITE F-SORTIE.

       MOVE ALL "*" TO F-SORTIE.
       WRITE F-SORTIE.

       DISPLAY "Fermeture du fichier".
       CLOSE FICHIER-SORTIE.

       0400-ECRITURE-FIN.
       EXIT.


