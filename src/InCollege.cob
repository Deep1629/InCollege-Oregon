       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD      PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD     PIC X(200).

       FD USER-FILE.
       01 USER-RECORD.
           05 U-NAME        PIC X(20).
           05 U-PASS        PIC X(12).

       WORKING-STORAGE SECTION.

       01 EOF-FLAG          PIC X VALUE 'N'.
       01 USER-COUNT        PIC 9 VALUE 0.
       01 MENU-CHOICE       PIC 9.
       01 TEMP-USER         PIC X(20).
       01 TEMP-PASS         PIC X(12).

       01 DISPLAY-LINE      PIC X(200).

       PROCEDURE DIVISION.

       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           OPEN I-O USER-FILE

           PERFORM SHOW-MAIN-MENU

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           CLOSE USER-FILE
           STOP RUN.

       SHOW-MAIN-MENU.
           PERFORM WRITE-LINE
               USING "Welcome to InCollege!"
           PERFORM WRITE-LINE
               USING "1. Log In"
           PERFORM WRITE-LINE
               USING "2. Create New Account"
           PERFORM WRITE-LINE
               USING "Enter your choice:"

           READ INPUT-FILE INTO INPUT-RECORD
           MOVE INPUT-RECORD TO MENU-CHOICE

           IF MENU-CHOICE = 1
               PERFORM LOGIN
           ELSE
               IF MENU-CHOICE = 2
                   PERFORM CREATE-ACCOUNT
               END-IF
           END-IF.

       LOGIN.
           PERFORM WRITE-LINE
               USING "Please enter your username:"
           READ INPUT-FILE INTO TEMP-USER

           PERFORM WRITE-LINE
               USING "Please enter your password:"
           READ INPUT-FILE INTO TEMP-PASS

           PERFORM WRITE-LINE
               USING "You have successfully logged in."

           PERFORM POST-LOGIN-MENU.

       CREATE-ACCOUNT.
           IF USER-COUNT >= 5
               PERFORM WRITE-LINE
                   USING "All permitted accounts have been created, please come back later"
               GO TO MAIN
           END-IF

           PERFORM WRITE-LINE
               USING "Enter new username:"
           READ INPUT-FILE INTO TEMP-USER

           PERFORM WRITE-LINE
               USING "Enter new password:"
           READ INPUT-FILE INTO TEMP-PASS

           ADD 1 TO USER-COUNT

           WRITE USER-RECORD
               FROM TEMP-USER

           PERFORM WRITE-LINE
               USING "Account successfully created."

           PERFORM MAIN.

       POST-LOGIN-MENU.
           PERFORM WRITE-LINE
               USING "1. Search for a job"
           PERFORM WRITE-LINE
               USING "2. Find someone you know"
           PERFORM WRITE-LINE
               USING "3. Learn a new skill"
           PERFORM WRITE-LINE
               USING "Enter your choice:"

           READ INPUT-FILE INTO MENU-CHOICE

           IF MENU-CHOICE = 1
               PERFORM WRITE-LINE
                   USING "Job search is under construction."
           ELSE
               IF MENU-CHOICE = 2
                   PERFORM WRITE-LINE
                       USING "Find someone you know is under construction."
               ELSE
                   IF MENU-CHOICE = 3
                       PERFORM SKILL-MENU
                   END-IF
               END-IF
           END-IF.

       SKILL-MENU.
           PERFORM WRITE-LINE
               USING "Learn a New Skill:"
           PERFORM WRITE-LINE
               USING "1. Skill One"
           PERFORM WRITE-LINE
               USING "2. Skill Two"
           PERFORM WRITE-LINE
               USING "3. Skill Three"
           PERFORM WRITE-LINE
               USING "4. Skill Four"
           PERFORM WRITE-LINE
               USING "5. Skill Five"
           PERFORM WRITE-LINE
               USING "6. Go Back"

           READ INPUT-FILE INTO MENU-CHOICE

           PERFORM WRITE-LINE
               USING "This feature is under construction."

           PERFORM POST-LOGIN-MENU.

       WRITE-LINE.
           WRITE OUTPUT-RECORD FROM DISPLAY-LINE
           DISPLAY DISPLAY-LINE.

