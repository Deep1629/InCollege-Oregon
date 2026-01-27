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
       01 INPUT-REC PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-REC PIC X(200).

       FD USER-FILE.
       01 USER-RECORD.
           05 U-NAME        PIC X(20).
           05 U-PASS        PIC X(12).
      
       WORKING-STORAGE SECTION.
       01 WS-CHOICE-X PIC X(1).
       01 WS-CHOICE   PIC 9.
       01 WS-LINE     PIC X(200).
       01 WS-EOF      PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           OPEN I-O USER-FILE

           PERFORM UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-MENU
               READ INPUT-FILE INTO INPUT-REC
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE INPUT-REC(1:1) TO WS-CHOICE-X
                       MOVE FUNCTION NUMVAL(WS-CHOICE-X) TO WS-CHOICE
                       PERFORM PROCESS-CHOICE
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           CLOSE USER-FILE
           STOP RUN.

       DISPLAY-MENU.
           MOVE "Welcome to InCollege!" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "Log In" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "Create New Account" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "Enter your choice:" TO WS-LINE
           PERFORM WRITE-LINE.

           READ INPUT-FILE INTO INPUT-REC
           MOVE INPUT-REC TO WS-CHOICE
      
           IF WS-CHOICE = 1
               *> MOVE "Login selected." TO WS-LINE
               *> PERFORM WRITE-LINE
               PERFORM LOGIN
           ELSE
               IF WS-CHOICE = 2
                   *> MOVE "Create account selected." TO WS-LINE
                   *> PERFORM WRITE-LINE
                   PERFORM CREATE-ACCOUNT
               ELSE
                   MOVE "Invalid choice." TO WS-LINE
                   PERFORM WRITE-LINE
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
           
           PERFORM WRITE-LINE
               USING "Welcome, " TEMP-USER.

           PERFORM POST-LOGIN-MENU.

       CREATE-ACCOUNT.
           IF USER-COUNT >= 5
               PERFORM WRITE-LINE
                   USING "All permitted accounts have been created, please come back later."
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
     
           PERFORM WRITE-LINE
               USING "Welcome, " TEMP-USER.

           PERFORM POST-LOGIN-MENU.

       POST-LOGIN-MENU.

           PERFORM WRITE-LINE
               USING "Search for a job"
           PERFORM WRITE-LINE
               USING "Find someone you know"
           PERFORM WRITE-LINE
               USING "Learn a new skill"
           PERFORM WRITE-LINE
               USING "Enter your choice:"

           READ INPUT-FILE INTO MENU-CHOICE

           IF MENU-CHOICE = 1
               PERFORM WRITE-LINE
                   USING "Job search/internship is under construction."
                   PERFORM POST-LOGIN-MENU.
           ELSE
               IF MENU-CHOICE = 2
                   PERFORM WRITE-LINE
                       USING "Find someone you know is under construction."
                       PERFORM POST-LOGIN-MENU.
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
               USING "Data Analysis"
           PERFORM WRITE-LINE
               USING "Digital Marketing"
           PERFORM WRITE-LINE
               USING "Project Management"
           PERFORM WRITE-LINE
               USING "Effective Communication"
           PERFORM WRITE-LINE
               USING "Programming Basics"
           PERFORM WRITE-LINE
               USING "Go Back"

           READ INPUT-FILE INTO MENU-CHOICE

           PERFORM WRITE-LINE
               USING "This feature is under construction."

           PERFORM SKILL-MENU.

      
       WRITE-LINE.
           DISPLAY WS-LINE
           MOVE WS-LINE TO OUTPUT-REC
           WRITE OUTPUT-REC.

