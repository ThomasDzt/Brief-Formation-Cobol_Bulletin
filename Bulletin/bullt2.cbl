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

      *--------------------------- 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *Création de l'alias pour le fichier d'entrée 
      *Les enregistrements des fichiers sont définis de manière séquentielle pour le traitement 
      *Les enregistrements suivent une organisation séquentielle (ligne par ligne)
      *Création d'une variable de contrôle du statut du fichier 


           SELECT FICHIER-ENTREE ASSIGN TO 'input.txt'
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS F-STATUT-ENTREE.     
                   
       
      *    SELECT F-OUTPUT
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
       
      *    01  ENR-F-ENTREE-2       PIC 9(02).
      *    01  ENR-F-ENTREE-10      PIC X(10).
      *    01  ENR-F-ENTREE-100     PIC X(100).
           01  ENR-F-ENTREE-1000    PIC X(1000).
       
      *Données correspondant aux lignes pour chaque étudiant (clé, nom, prénom, âge)
           01  F-LIGNE-ETUDIANT.
               03 F-ETU-CLE        PIC 9(02).       
               03 F-NOM            PIC X(07).       
               03 F-PRENOM         PIC X(06).       
               03 F-AGE            PIC 9(02).       
       
      *Données correspondant aux lignes pour chaque cours (clé, matière, coefficient, note) 
           01  F-LIGNE-COURS.
               03 F-C-CLE          PIC 9(02).       
               03 F-MATIERE        PIC X(21).       
               03 F-COEF           PIC 9,9.       
               03 F-NOTE           PIC 99,99.       
       
      *FD  F-OUTPUT
      *    RECORD CONTAINS 250 CHARACTERS
      *    RECORDING MODE IS F.

      *01  REC-F-OUTPUT        PIC X(250).

       
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

               10 WS-COURS     OCCURS 999 TIMES. 
                               

                 15 WS-MATIERE    PIC X(21).
                 15 WS-COEF       PIC 9,9.
                 15 WS-NOTE       PIC 99,99.        

       01 WS-ENTETE-NOM           PIC X(07)       VALUE "Nom".
       01 WS-ENTETE-PRENOM        PIC X(08)       VALUE "Prenom".
       01 WS-ENTETE-AGE           PIC X(03)       VALUE "Age".    
       01 WS-ENTETE-MAT           PIC X(21)       VALUE "Matiere".     
       01 WS-ENTETE-COEF          PIC X(05)       VALUE "Coef". 
       01 WS-ENTETE-NOTE          PIC X(04)       VALUE "Note". 

       01 WS-ETOILE               PIC X(31)       VALUE ALL "*".
       01 WS-TIRET                PIC X(31)       VALUE ALL "-".

      *Création d'index pour parcourir le tableau selon les dimensions
       77  WS-IDX-ETUD            PIC 9(03)        VALUE 0.      
       77  WS-IDX-COURS           PIC 9(03)        VALUE 0.

      


      ****************************************************************** 
      *                      PROCEDURE DIVISION                        *
      ****************************************************************** 
       PROCEDURE DIVISION.

       PERFORM 0100-LECTURE-DEBUT
          THRU 0100-LECTURE-FIN.

       PERFORM 0200-AFFICHE-DEBUT
          THRU 0200-AFFICHE-FIN.


       STOP RUN. 


      ****************************************************************** 
      *                          PARAGRAPHES                           *
      ****************************************************************** 

       0100-LECTURE-DEBUT.

       DISPLAY "Ouverture du fichier :".
       OPEN INPUT FICHIER-ENTREE.

      *IF NOT F-STATUT-ENTREE-OK
      *    DISPLAY "Erreur à l'ouverture du fichier :" F-STATUT-ENTREE
      *END-IF. 

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

      *---------------------------------------------
       0110-TRAITEMENT-LECT-DEBUT.
       EVALUATE TRUE 
           WHEN ENR-F-ENTREE-1000(1:2) = 01 
            MOVE 0 TO WS-NBRE-COURS
            MOVE 0 TO WS-IDX-COURS
       
            ADD 1 TO WS-IDX-ETUD
            
            ADD 1 TO WS-NBRE-ETUDIANT
            MOVE ENR-F-ENTREE-1000(3:7) TO WS-NOM(WS-IDX-ETUD)
            MOVE ENR-F-ENTREE-1000(10:6) TO WS-PRENOM(WS-IDX-ETUD)
            MOVE ENR-F-ENTREE-1000(16:2)TO WS-AGE(WS-IDX-ETUD)
       
            
            
           WHEN ENR-F-ENTREE-1000(1:2) = 02 
              
            ADD 1 TO WS-IDX-COURS
            
            ADD 1 TO WS-NBRE-COURS
            MOVE ENR-F-ENTREE-1000(3:21)
            TO   WS-MATIERE(WS-IDX-ETUD,WS-IDX-COURS)
       
            MOVE ENR-F-ENTREE-1000(24:3)
            TO WS-COEF(WS-IDX-ETUD,WS-IDX-COURS)
       
            MOVE ENR-F-ENTREE-1000(27:5)
            TO WS-NOTE(WS-IDX-ETUD,WS-IDX-COURS)
                   
                
       END-EVALUATE.


       0110-TRAITEMENT-LECT-FIN.
       EXIT.

      *---------------------------------------------
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
