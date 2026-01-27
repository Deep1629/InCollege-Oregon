IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-REC PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-REC PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-CHOICE-X PIC X(1).
       01 WS-CHOICE   PIC 9.
       01 WS-LINE     PIC X(200).
       01 WS-EOF      PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

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
           STOP RUN.

       DISPLAY-MENU.
           MOVE "Welcome to InCollege!" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "1. Log In" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "2. Create New Account" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "Enter your choice:" TO WS-LINE
           PERFORM WRITE-LINE.

       PROCESS-CHOICE.
           IF WS-CHOICE = 1
               MOVE "Login selected." TO WS-LINE
               PERFORM WRITE-LINE
           ELSE
               IF WS-CHOICE = 2
                   MOVE "Create account selected." TO WS-LINE
                   PERFORM WRITE-LINE
               ELSE
                   MOVE "Invalid choice." TO WS-LINE
                   PERFORM WRITE-LINE
               END-IF
           END-IF.

       WRITE-LINE.
           DISPLAY WS-LINE
           MOVE WS-LINE TO OUTPUT-REC
           WRITE OUTPUT-REC.

