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
           FILE STATUS IS F-OUTPUT-STATUS.     



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
       
      *Données correspondant aux lignes pour chaque étudiant (clé, nom, prénom, âge)
      *    01  F-LIGNE-ETUDIANT.
      *        03 F-ETU-CLE        PIC 9(02).       
      *        03 F-NOM            PIC X(07).       
      *        03 F-PRENOM         PIC X(06).       
      *        03 F-AGE            PIC 9(02).       
       
      *Données correspondant aux lignes pour chaque cours (clé, matière, coefficient, note) 
      *    01  F-LIGNE-COURS.
      *        03 F-C-CLE          PIC 9(02).       
      *        03 F-MATIERE        PIC X(21).       
      *        03 F-COEF           PIC 9,9.       
      *        03 F-NOTE           PIC 99,99.       
       
       FD  FICHIER-SORTIE
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F.   

           01 F-SORTIE        PIC X(100).
           
         

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

      * Création d'une variable permettant de stocker le calcul de note * coeff pour chaque matière
                 15 WS-NOTECOEF   PIC 99V99.
       
       
      *Création d'une variable permettant de stocker temporairement
      * le calcul de somme pondérée pour chaque matière     
      
       01 WS-SOM-TEMPO  PIC 999V99.

      *Création d'un tableau pour récupérer les matières 
       01 WS-TABLEAU-MATIERE.
           05 WS-NBRE-MAT         PIC 9(03)        VALUE 0.
           05 WS-MATIERE-UNIQ     OCCURS 999 TIMES.
               10 WS-NOM-MAT-UNIQ PIC X(21). 


       01 WS-TEMPO                PIC X(21). 

       01 WS-TROUVE               PIC X            VALUE "N". *> indicateur de recherche

      *Création de variables d'en-tête pour l'affichage 
       01 WS-ENTETE-NOM           PIC X(07)       VALUE "Nom".
       01 WS-ENTETE-PRENOM        PIC X(08)       VALUE "Prenom".
       01 WS-ENTETE-AGE           PIC X(03)       VALUE "Age".    
       01 WS-ENTETE-MAT           PIC X(21)       VALUE "Matiere".     
       01 WS-ENTETE-COEF          PIC X(05)       VALUE "Coef". 
       01 WS-ENTETE-NOTE          PIC X(04)       VALUE "Note". 
       01 WS-ENTETE-MOYENNE       PIC X(07)       VALUE "Moyenne". 
       01 WS-ETOILE               PIC X(31)       VALUE ALL "*".
       01 WS-TIRET                PIC X(31)       VALUE ALL "-".

       
      *Création d'une variable permettant de stocker le total des coeff 
       01 WS-TOT-COEF             PIC 9V9. 
      

      *Création d'une variable permettant de stocker
      * le calcul de somme pondérée pour chaque matière   
       01 WS-SOMME               PIC 999V99.
       

      *Création d'index pour parcourir le tableau selon les dimensions
       77 WS-IDX-ETUD            PIC 9(03)        VALUE 0.      
       77 WS-IDX-COURS           PIC 9(03)        VALUE 0.

      *Création d'index pour parcourir le tableau de collecte des matières
      *uniques
       77 WS-IDX-MAT         PIC 9(03)        VALUE 0. 
      


      ****************************************************************** 
      *                      PROCEDURE DIVISION                        *
      ****************************************************************** 
       PROCEDURE DIVISION.

       PERFORM 0100-LECTURE-DEBUT
          THRU 0100-LECTURE-FIN.

       PERFORM 0200-AFFICHE-DEBUT
          THRU 0200-AFFICHE-FIN.

       PERFORM 0300-MOYENNE-ETU-DEBUT
          THRU 0300-MOYENNE-ETU-FIN.

       PERFORM 0400-AFFICHE2-DEBUT
          THRU 0400-AFFICHE2-FIN.

      *PERFORM 0500-MOYENNE-MAT-DEBUT
      *   THRU 0500-MOYENNE-MAT-FIN.  

      *PERFORM 0510-RECUP-MAT-DEBUT
      *   THRU 0510-RECUP-MAT-FIN.

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
       0200-AFFICHE-DEBUT.

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

       0200-AFFICHE-FIN.
       EXIT.

      *-----------------------------------------------------------------
       0300-MOYENNE-ETU-DEBUT.
      
       PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
               UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT
        
            
           
        MOVE 0 TO WS-SOMME 
        MOVE 0 TO WS-TOT-COEF
        MOVE 0 TO WS-SOM-TEMPO
        PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
            UNTIL WS-IDX-COURS > WS-NBRE-COURS 

           
           COMPUTE WS-NOTECOEF(WS-IDX-ETUD,WS-IDX-COURS) =
                   WS-COEF(WS-IDX-ETUD,WS-IDX-COURS) * 
                   WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
           
           

           ADD WS-NOTECOEF(WS-IDX-ETUD,WS-IDX-COURS)
           TO  WS-SOM-TEMPO

           
           ADD WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)
           TO  WS-TOT-COEF 
           

        END-PERFORM 
        MOVE WS-SOM-TEMPO
        TO   WS-SOMME 
        COMPUTE WS-MOYENNE(WS-IDX-ETUD) ROUNDED =
                WS-SOMME / WS-TOT-COEF

               
       END-PERFORM.
      
       0300-MOYENNE-ETU-FIN.
       EXIT.

      *-----------------------------------------------------------------

       0400-AFFICHE2-DEBUT.
       DISPLAY "Affichage des moyennes :".

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
       
       0400-AFFICHE2-FIN.
       EXIT.

      *-----------------------------------------------------------------
      *0500-MOYENNE-MAT-DEBUT.

      *PERFORM 0510-RECUP-MAT-DEBUT
      *   THRU 0510-RECUP-MAT-FIN. 

      *PERFORM VARYING WS-IDX-MAT FROM 1 BY 1 
      *                UNTIL WS-IDX-MAT > WS-NBRE-MAT 

       
           
          
      * MOVE 0 TO WS-SOMME 
      * MOVE 0 TO WS-TOT-COEF
      * MOVE 0 TO WS-SOM-TEMPO

      * PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
      *     UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT

      *    PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
      *     UNTIL WS-IDX-COURS > WS-NBRE-COURS 
      *    COMPUTE WS-NOTECOEF(WS-IDX-ETUD,WS-IDX-COURS) =
      *            WS-COEF(WS-IDX-ETUD,WS-IDX-COURS) * 
      *            WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
      *    
      *    

      *    ADD WS-NOTECOEF(WS-IDX-ETUD,WS-IDX-COURS)
      *    TO  WS-SOM-TEMPO

      *    
      *    ADD WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)
      *    TO  WS-TOT-COEF 
      *    
      *    END-PERFORM 
      * END-PERFORM 

      * MOVE WS-SOM-TEMPO
      * TO   WS-SOMME 
      * COMPUTE WS-MOYENNE(WS-IDX-ETUD) ROUNDED  =
      *         WS-SOMME / WS-TOT-COEF
      * 
      *        
      *END-PERFORM.


      *0500-MOYENNE-MAT-FIN.
      *EXIT.
       
      *-----------------------------------------------------------------
      *0510-RECUP-MAT-DEBUT.
      *PERFORM VARYING WS-IDX-ETUD FROM 1 BY 1 
      *        UNTIL WS-IDX-ETUD > WS-NBRE-ETUDIANT
      *    
      *    PERFORM VARYING WS-IDX-COURS FROM 1 BY 1 
      *            UNTIL   WS-IDX-COURS > WS-NBRE-COURS

      *Récupération de chaque matière dans le tableau tampon dans une variable temporaire     
      *     MOVE WS-MATIERE(WS-IDX-ETUD, WS-IDX-COURS)
      *     TO WS-TEMPO 

      *Initialisation de la première valeur du tableau tampon 
      *     IF WS-NBRE-MAT = 0 
      *        ADD 1 TO WS-NBRE-MAT
      *        MOVE WS-TEMPO TO WS-NOM-MAT-UNIQ(WS-NBRE-MAT)

      *     ELSE 
      *        MOVE "N" TO WS-TROUVE 
      *Comparaison des matières récupérées avec les matières dans le tableau tampon
      *        PERFORM VARYING WS-IDX-MAT FROM 1 BY 1 
      *                UNTIL WS-IDX-MAT > WS-NBRE-MAT 
      *                OR WS-TROUVE = "Y"
      *           
      *           IF WS-NOM-MAT-UNIQ(WS-IDX-MAT) = WS-TEMPO 
      *               MOVE "Y" TO WS-TROUVE 
      *           END-IF 
      *
      *        END-PERFORM
             
      *Lorsque pas de matière correspondante trouvée, ajout de la matière dans le tableau tampon  
      *        IF WS-TROUVE = "N"
      *           ADD 1 TO WS-NBRE-MAT
      *           MOVE WS-TEMPO TO WS-NOM-MAT-UNIQ(WS-NBRE-MAT)
      *        END-IF 

      *     END-IF
      *    END-PERFORM 

      *END-PERFORM.

      *Affichage des matières récupérées dans le tableau 
      *PERFORM VARYING WS-IDX-MAT FROM 1 BY 1 
      *                UNTIL WS-IDX-MAT > WS-NBRE-MAT 

      *        DISPLAY WS-NOM-MAT-UNIQ(WS-IDX-MAT)
      *END-PERFORM.


      *0510-RECUP-MAT-FIN.
      *EXIT.


      *-----------------------------------------------------------------

