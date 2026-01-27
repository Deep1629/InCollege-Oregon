       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OutputFile ASSIGN TO "output/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UserDataFile ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputRecord PIC X(100).

       FD OutputFile.
       01 OutputRecord PIC X(100).

       FD UserDataFile.
       01 UserRecord.
           05 Username PIC X(20).
           05 Password PIC X(20).

       WORKING-STORAGE SECTION.
       01 UserCount PIC 9(3) VALUE 0.
       01 MaxUsers PIC 9(3) VALUE 5.
       01 LoggedIn PIC X VALUE 'N'.
       01 CurrentUsername PIC X(20).
       01 CurrentPassword PIC X(20).
       01 MenuOption PIC 9 VALUE 0.
       01 LoginSuccess PIC X VALUE 'N'.
       01 EOF-UserData PIC X VALUE 'N'.
       01 EOF-InputFile PIC X VALUE 'N'.
       01 InputChar PIC X.
       01 CurrentMessage PIC X(120).
       01 OutputFileInitialized PIC X VALUE 'N'.
       01 UsernameExists PIC X VALUE 'N'.
       01 PasswordLength PIC 99 VALUE 0.
       01 TempString PIC X(100).
       01 PasswordValid PIC X VALUE 'N'.
       01 HasCapital PIC X VALUE 'N'.
       01 HasDigit PIC X VALUE 'N'.
       01 HasSpecial PIC X VALUE 'N'.
       01 I PIC 99 VALUE 0.
       01 CheckChar PIC X VALUE SPACE.

       PROCEDURE DIVISION.
       MainSection.
           OPEN INPUT InputFile
           PERFORM CountExistingUsers
           PERFORM DisplayWelcome
           MOVE 'N' TO LoggedIn
           PERFORM InitialMenu UNTIL LoggedIn = 'Y' OR MenuOption = 9
           CLOSE InputFile
           STOP RUN.

       CountExistingUsers.
           OPEN INPUT UserDataFile
           MOVE 'N' TO EOF-UserData
           PERFORM UNTIL EOF-UserData = 'Y'
               READ UserDataFile INTO UserRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   ADD 1 TO UserCount
           END-PERFORM
           CLOSE UserDataFile.

       DisplayAndLog.
           DISPLAY CurrentMessage
           IF OutputFileInitialized = 'N' THEN
               OPEN OUTPUT OutputFile
               MOVE 'Y' TO OutputFileInitialized
           ELSE
               OPEN EXTEND OutputFile
           END-IF
           MOVE CurrentMessage TO OutputRecord
           WRITE OutputRecord
           CLOSE OutputFile.

       DisplayWelcome.
           STRING "Welcome to InCollege!" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog.

       InitialMenu.
           STRING "1. Log In" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog
           STRING "2. Create New Account" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog
           STRING "3. Logout" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog
           STRING "Enter your choice:" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog
           PERFORM ReadMenuOption
           EVALUATE MenuOption
               WHEN 1
                   PERFORM LoginUser
               WHEN 2
                   PERFORM RegisterUser
               WHEN 3
                   MOVE 9 TO MenuOption
               WHEN OTHER
                   STRING "Invalid option. Please try again."
                       DELIMITED BY SIZE INTO CurrentMessage
                   END-STRING
                   PERFORM DisplayAndLog
           END-EVALUATE.

       RegisterUser.
           IF UserCount >= MaxUsers THEN
               STRING "All permitted accounts have been created, "
                   "please come back later" DELIMITED BY SIZE
                   INTO CurrentMessage
               END-STRING
               PERFORM DisplayAndLog
           ELSE
               STRING "Enter new username:" DELIMITED BY SIZE
                   INTO CurrentMessage
               END-STRING
               PERFORM DisplayAndLog
               PERFORM ReadUsername
               STRING "Enter new password:" DELIMITED BY SIZE
                   INTO CurrentMessage
               END-STRING
               PERFORM DisplayAndLog
               PERFORM ReadPassword
               PERFORM ValidatePassword
               IF PasswordValid = 'N' THEN
                   CONTINUE
               ELSE
                   PERFORM CheckUsernameExists
                   IF UsernameExists = 'Y' THEN
                       STRING "Username already exists. "
                           "Please try a different username."
                           DELIMITED BY SIZE INTO CurrentMessage
                       END-STRING
                       PERFORM DisplayAndLog
                   ELSE
                       OPEN EXTEND UserDataFile
                       MOVE CurrentUsername TO Username
                       MOVE CurrentPassword TO Password
                       WRITE UserRecord
                       CLOSE UserDataFile
                       ADD 1 TO UserCount
                       STRING "Account successfully created."
                           DELIMITED BY SIZE INTO CurrentMessage
                       END-STRING
                       PERFORM DisplayAndLog
                   END-IF
               END-IF
           END-IF.

       LoginUser.
           MOVE 'N' TO LoginSuccess
           MOVE 'N' TO EOF-UserData
           STRING "Please enter your username:" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog
           PERFORM ReadUsername
           STRING "Please enter your password:" DELIMITED BY SIZE
               INTO CurrentMessage
           END-STRING
           PERFORM DisplayAndLog
           PERFORM ReadPassword
           OPEN INPUT UserDataFile
           PERFORM UNTIL LoginSuccess = 'Y' OR EOF-UserData = 'Y'
               READ UserDataFile INTO UserRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   IF Username = CurrentUsername AND
                      Password = CurrentPassword THEN
                       STRING "You have successfully logged in."
                           DELIMITED BY SIZE INTO CurrentMessage
                       END-STRING
                       PERFORM DisplayAndLog
                       STRING "Video is now playing"
                           DELIMITED BY SIZE INTO CurrentMessage
                       END-STRING
                       PERFORM DisplayAndLog
                       STRING "Thank you for watching!"
                           DELIMITED BY SIZE INTO CurrentMessage
                       END-STRING
                       PERFORM DisplayAndLog
                       MOVE 'Y' TO LoginSuccess
                       MOVE 'Y' TO LoggedIn
                   END-IF
           END-PERFORM
           CLOSE UserDataFile
           IF LoginSuccess = 'N' THEN
               STRING "Incorrect username/password, please try again."
                   DELIMITED BY SIZE INTO CurrentMessage
               END-STRING
               PERFORM DisplayAndLog
           END-IF.

       ReadMenuOption.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE 9 TO MenuOption
           NOT AT END
               MOVE InputRecord(1:1) TO InputChar
               MOVE FUNCTION NUMVAL-C(InputChar) TO MenuOption
           END-READ.

       ReadUsername.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentUsername
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentUsername
           END-READ.

       ReadPassword.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentPassword
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentPassword
           END-READ.

       ValidatePassword.
           MOVE 'Y' TO PasswordValid
           MOVE 'N' TO HasCapital
           MOVE 'N' TO HasDigit
           MOVE 'N' TO HasSpecial
           MOVE FUNCTION LENGTH(FUNCTION TRIM(CurrentPassword))
               TO PasswordLength
           
           IF PasswordLength < 8 OR PasswordLength > 12 THEN
               STRING "Password must be between 8 and 12 characters."
                   DELIMITED BY SIZE INTO CurrentMessage
               END-STRING
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           ELSE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > PasswordLength
                   MOVE CurrentPassword(I:1) TO CheckChar
                   IF CheckChar >= 'A' AND CheckChar <= 'Z' THEN
                       MOVE 'Y' TO HasCapital
                   END-IF
                   IF CheckChar >= '0' AND CheckChar <= '9' THEN
                       MOVE 'Y' TO HasDigit
                   END-IF
                   IF CheckChar = '!' OR CheckChar = '@' OR
                      CheckChar = '#' OR CheckChar = '$' OR
                      CheckChar = '%' OR CheckChar = '^' OR
                      CheckChar = '&' OR CheckChar = '*' OR
                      CheckChar = '(' OR CheckChar = ')' OR
                      CheckChar = '-' OR CheckChar = '_' OR
                      CheckChar = '+' OR CheckChar = '=' OR
                      CheckChar = '[' OR CheckChar = ']' OR
                      CheckChar = '{' OR CheckChar = '}' OR
                      CheckChar = ';' OR CheckChar = ':' OR
                      CheckChar = '"' OR CheckChar = '|' OR
                      CheckChar = '<' OR CheckChar = '>' OR
                      CheckChar = ',' OR CheckChar = '.' OR
                      CheckChar = '?' OR CheckChar = '/' OR
                      CheckChar = '~' OR CheckChar = '`' THEN
                       MOVE 'Y' TO HasSpecial
                   END-IF
               END-PERFORM
               
               IF HasCapital = 'N' OR HasDigit = 'N' OR
                  HasSpecial = 'N' THEN
                   STRING "Password must contain at least one "
                       "capital letter, one digit, and one "
                       "special character." DELIMITED BY SIZE
                       INTO CurrentMessage
                   END-STRING
                   PERFORM DisplayAndLog
                   MOVE 'N' TO PasswordValid
               END-IF
           END-IF.

       CheckUsernameExists.
           MOVE 'N' TO UsernameExists
           MOVE 'N' TO EOF-UserData
           OPEN INPUT UserDataFile
           PERFORM UNTIL EOF-UserData = 'Y'
               READ UserDataFile INTO UserRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   IF Username = CurrentUsername THEN
                       MOVE 'Y' TO UsernameExists
                       MOVE 'Y' TO EOF-UserData
                   END-IF
           END-PERFORM
           CLOSE UserDataFile.
