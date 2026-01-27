       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OutputFile ASSIGN TO "output/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL UserDataFile ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS UserFileStatus.
       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputRecord PIC X(100).
       FD OutputFile.
       01 OutputRecord PIC X(100).
       FD UserDataFile.
       01 UserRecord.
           05 StoredUser PIC X(25).
           05 StoredPass PIC X(15).
       WORKING-STORAGE SECTION.
       01 WS-Msg PIC X(100).
       01 UserCount PIC 9(3) VALUE 0.
       01 MaxUsers PIC 9(3) VALUE 5.
       01 LoggedIn PIC X VALUE 'N'.
       01 CurrUser PIC X(25).
       01 CurrPass PIC X(15).
       01 MenuOpt PIC 9 VALUE 0.
       01 LoginOK PIC X VALUE 'N'.
       01 EOF-User PIC X VALUE 'N'.
       01 EOF-Input PIC X VALUE 'N'.
       01 InChar PIC X.
       01 FileOpen PIC X VALUE 'N'.
       01 UserExists PIC X VALUE 'N'.
       01 PassLen PIC 99 VALUE 0.
       01 PassOK PIC X VALUE 'N'.
       01 HasCap PIC X VALUE 'N'.
       01 HasDig PIC X VALUE 'N'.
       01 HasSpec PIC X VALUE 'N'.
       01 Idx PIC 99 VALUE 0.
       01 OneChar PIC X.
       01 UserFileStatus PIC XX.
       PROCEDURE DIVISION.
       Main.
           OPEN INPUT InputFile
           PERFORM CountUsers
           MOVE 'N' TO LoggedIn
           PERFORM ShowMenu UNTIL LoggedIn = 'Y' OR MenuOpt = 9
           CLOSE InputFile
           STOP RUN.
       CountUsers.
           OPEN INPUT UserDataFile
           IF UserFileStatus = '00'
               MOVE 'N' TO EOF-User
               PERFORM UNTIL EOF-User = 'Y'
                   READ UserDataFile INTO UserRecord
                       AT END MOVE 'Y' TO EOF-User
                       NOT AT END ADD 1 TO UserCount
                   END-READ
               END-PERFORM
           END-IF
           CLOSE UserDataFile.
       WriteMsg.
           IF FileOpen = 'N'
               OPEN OUTPUT OutputFile
               MOVE 'Y' TO FileOpen
           ELSE
               OPEN EXTEND OutputFile
           END-IF
           INITIALIZE OutputRecord
           MOVE FUNCTION TRIM(WS-Msg) TO OutputRecord
           WRITE OutputRecord
           CLOSE OutputFile
           DISPLAY FUNCTION TRIM(WS-Msg)
           INITIALIZE WS-Msg.
       ShowMenu.
           MOVE "Welcome to InCollege!" TO WS-Msg
           PERFORM WriteMsg
           MOVE "1. Log In" TO WS-Msg
           PERFORM WriteMsg
           MOVE "2. Create New Account" TO WS-Msg
           PERFORM WriteMsg
           MOVE "3. Logout" TO WS-Msg
           PERFORM WriteMsg
           MOVE "Enter your choice:" TO WS-Msg
           PERFORM WriteMsg
           PERFORM ReadOpt
           EVALUATE MenuOpt
               WHEN 1 PERFORM DoLogin
               WHEN 2 PERFORM DoRegister
               WHEN 3 MOVE 9 TO MenuOpt
               WHEN OTHER
                   MOVE "Invalid option." TO WS-Msg
                   PERFORM WriteMsg
           END-EVALUATE.
       DoRegister.
           IF UserCount >= MaxUsers
               MOVE "All permitted accounts created." TO WS-Msg
               PERFORM WriteMsg
           ELSE
               MOVE "Enter new username:" TO WS-Msg
               PERFORM WriteMsg
               PERFORM ReadUser
               MOVE "Enter new password:" TO WS-Msg
               PERFORM WriteMsg
               PERFORM ReadPass
               PERFORM CheckPass
               IF PassOK = 'N'
                   CONTINUE
               ELSE
                   PERFORM CheckUserExists
                   IF UserExists = 'Y'
                       MOVE "Username already exists." TO WS-Msg
                       PERFORM WriteMsg
                   ELSE
                       IF UserCount = 0
                           OPEN OUTPUT UserDataFile
                       ELSE
                           OPEN EXTEND UserDataFile
                       END-IF
                       MOVE CurrUser TO StoredUser
                       MOVE CurrPass TO StoredPass
                       WRITE UserRecord
                       CLOSE UserDataFile
                       ADD 1 TO UserCount
                       MOVE "Account successfully created." TO WS-Msg
                       PERFORM WriteMsg
                   END-IF
               END-IF
           END-IF.
       DoLogin.
           MOVE 'N' TO LoginOK
           MOVE 'N' TO EOF-User
           MOVE "Please enter your username:" TO WS-Msg
           PERFORM WriteMsg
           PERFORM ReadUser
           MOVE "Please enter your password:" TO WS-Msg
           PERFORM WriteMsg
           PERFORM ReadPass
           OPEN INPUT UserDataFile
           IF UserFileStatus = '00'
               PERFORM UNTIL LoginOK = 'Y' OR EOF-User = 'Y'
                   READ UserDataFile INTO UserRecord
                       AT END MOVE 'Y' TO EOF-User
                       NOT AT END
                           IF StoredUser = CurrUser AND
                              StoredPass = CurrPass
                               MOVE "You have successfully logged in."
                                   TO WS-Msg
                               PERFORM WriteMsg
                               MOVE 'Y' TO LoginOK
                               MOVE 'Y' TO LoggedIn
                           END-IF
                   END-READ
               END-PERFORM
           END-IF
           CLOSE UserDataFile
           IF LoginOK = 'N'
               MOVE "Incorrect username/password, please try again."
                   TO WS-Msg
               PERFORM WriteMsg
           END-IF.
       ReadOpt.
           READ InputFile INTO InputRecord
               AT END
                   MOVE 'Y' TO EOF-Input
                   MOVE 9 TO MenuOpt
               NOT AT END
                   MOVE InputRecord(1:1) TO InChar
                   IF InChar IS NUMERIC
                       MOVE InChar TO MenuOpt
                   ELSE
                       MOVE 0 TO MenuOpt
                   END-IF
           END-READ.
       ReadUser.
           READ InputFile INTO InputRecord
               AT END
                   MOVE 'Y' TO EOF-Input
                   INITIALIZE CurrUser
               NOT AT END
                   MOVE FUNCTION TRIM(InputRecord) TO CurrUser
           END-READ.
       ReadPass.
           READ InputFile INTO InputRecord
               AT END
                   MOVE 'Y' TO EOF-Input
                   INITIALIZE CurrPass
               NOT AT END
                   MOVE FUNCTION TRIM(InputRecord) TO CurrPass
           END-READ.
       CheckPass.
           MOVE 'Y' TO PassOK
           MOVE 'N' TO HasCap
           MOVE 'N' TO HasDig
           MOVE 'N' TO HasSpec
           MOVE FUNCTION LENGTH(FUNCTION TRIM(CurrPass)) TO PassLen
           IF PassLen < 8 OR PassLen > 12
               MOVE "Password must be 8-12 characters." TO WS-Msg
               PERFORM WriteMsg
               MOVE 'N' TO PassOK
           ELSE
               PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > PassLen
                   MOVE CurrPass(Idx:1) TO OneChar
                   IF OneChar >= 'A' AND OneChar <= 'Z'
                       MOVE 'Y' TO HasCap
                   END-IF
                   IF OneChar >= '0' AND OneChar <= '9'
                       MOVE 'Y' TO HasDig
                   END-IF
                   IF OneChar = '!' OR OneChar = '@' OR
                      OneChar = '#' OR OneChar = '$' OR
                      OneChar = '%' OR OneChar = '^' OR
                      OneChar = '&' OR OneChar = '*'
                       MOVE 'Y' TO HasSpec
                   END-IF
               END-PERFORM
               IF HasCap = 'N' OR HasDig = 'N' OR HasSpec = 'N'
                   MOVE "Password needs capital, digit, special."
                       TO WS-Msg
                   PERFORM WriteMsg
                   MOVE 'N' TO PassOK
               END-IF
           END-IF.
       CheckUserExists.
           MOVE 'N' TO UserExists
           MOVE 'N' TO EOF-User
           OPEN INPUT UserDataFile
           IF UserFileStatus = '00'
               PERFORM UNTIL EOF-User = 'Y'
                   READ UserDataFile INTO UserRecord
                       AT END MOVE 'Y' TO EOF-User
                       NOT AT END
                           IF StoredUser = CurrUser
                               MOVE 'Y' TO UserExists
                               MOVE 'Y' TO EOF-User
                           END-IF
                   END-READ
               END-PERFORM
           END-IF
           CLOSE UserDataFile.
