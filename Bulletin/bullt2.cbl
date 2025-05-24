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
                   
       
      *    SELECT FICHIER-SORTIE
      *    ASSIGN TO 'output.dat'
      *    ACCESS MODE IS SEQUENTIAL
      *    ORGANIZATION IS LINE SEQUENTIAL
      *    FILE STATUS IS F-OUTPUT-STATUS.     



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
              
       
      *FD  FICHIER-SORTIE
      *    RECORD CONTAINS 100 CHARACTERS
      *    RECORDING MODE IS F.   

      *    01 F-SORTIE        PIC X(100).
      *    
         

      *----------------------------------------------------------------- 
       WORKING-STORAGE SECTION.

      *Définition de la variable de contrôle de statut du fichier 
      *Utile pour définir la fin de boucle de lecture du fichier 
       01  F-STATUT-ENTREE      PIC X(02) VALUE SPACE.
           88 F-STATUT-ENTREE-OK    VALUE '00'.        
           88 F-STATUT-ENTREE-FF    VALUE '10'.

      *01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
      *    88 F-OUTPUT-STATUS-OK    VALUE '00'.        
      *    88 F-OUTPUT-STATUS-EOF   VALUE '10'.


      *Création du tableau bidimensionnel de stockage des informations contenues dans le fichier
      *Celui-ci sera de taille variable selon le nombre d'étudiants et le nombre de cours 
       01  WS-DONNEE-ETUDIANT.
           05 WS-NBRE-ETUDIANT    PIC 9(03)        VALUE 0.
           05 WS-NBRE-COURS       PIC 9(03)        VALUE 0.

           05 WS-ETUDIANT      OCCURS 1 TO 999 TIMES 
                               DEPENDING ON WS-NBRE-ETUDIANT.
               
               10 WS-NOM          PIC X(07).
               10 WS-PRENOM       PIC X(06).      
               10 WS-AGE          PIC 9(02).

      *Création d'une variable permettant de stocker la moyenne pour chaque élève 
               10 WS-MOYENNE      PIC 99V99.

               10 WS-COURS     OCCURS 999 TIMES. 
                               

                 15 WS-MATIERE    PIC X(21).
                 15 WS-COEF       PIC 9V9.
                 15 WS-NOTE       PIC 99V99.        

      *Création d'une variable permettant de stocker le calcul de note * coeff pour chaque matière
                 15 WS-NOTE-POND  PIC 99V999.

      *Création d'une variable permettant de stocker la moyenne pour chaque matière             
                 15 WS-MOY-MAT    PIC 99V99.
       
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

      *Création de variables de stockage temporaire pour le tri des matières 
       01 WS-MATIERE-TEMPO        PIC X(21).
       01 WS-NOTE-TEMPO           PIC 99V99.
       01 WS-COEF-TEMPO           PIC 9V9.
       
      *Création de variables d'en-tête pour l'affichage 
       01 WS-ENTETE-NOM           PIC X(07)      VALUE "Nom".
       01 WS-ENTETE-PRENOM        PIC X(08)      VALUE "Prenom".
       01 WS-ENTETE-AGE           PIC X(03)      VALUE "Age".    
       01 WS-ENTETE-MAT           PIC X(21)      VALUE "Matiere".     
       01 WS-ENTETE-COEF          PIC X(05)      VALUE "Coef". 
       01 WS-ENTETE-NOTE          PIC X(04)      VALUE "Note". 
       01 WS-ENTETE-MOYENNE       PIC X(07)      VALUE "Moyenne". 
       01 WS-ENTETE-MOY-CLS       PIC X(16)      VALUE "Moyenne classe". 
       01 WS-ETOILE               PIC X(31)      VALUE ALL "*".
       01 WS-TIRET                PIC X(31)      VALUE ALL "-".

       

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
