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
           SELECT UserProfileRecordFile ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputRecord PIC X(200).

       FD OutputFile.
       01 OutputRecord PIC X(100).
       FD UserDataFile.
       01 UserRecord.
           05 Username PIC X(20).
           05 Password PIC X(20).

       FD UserProfileRecordFile.
       01 UserProfileRecord.
           05 Username-Profile PIC X(20).
           05 FirstName PIC X(20).
           05 LastName PIC X(20).
           05 University PIC X(30).
           05 Major PIC X(30).
           05 GraduationYear PIC 9(4).
           05 AboutMe PIC X(200).
           05 Experience-Title PIC X(200).
           05 Experience-Company PIC X(200).
           05 Experience-Dates PIC X(100).
           05 Experience-Description PIC X(500).

       WORKING-STORAGE SECTION.
       01 WS-Msg PIC X(100).
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
       01 SkillOption PIC 9 VALUE 0.
       01 CurrentMessage PIC X(100).
       01 OutputFileInitialized PIC X VALUE 'N'.
       01 UsernameExists PIC X VALUE 'N'.
       01 PasswordLength PIC 99 VALUE 0.
       01 TempString PIC X(100).
       01 CurrentFirstName PIC X(20).
       01 LastName PIC X(20).
       01 CurrentLastName PIC X(20).
       01 University PIC X(30).
       01 CurrentUniversity PIC X(30).
       01 Major PIC X(30).
       01 CurrentMajor PIC X(30).
       01 GraduationYear PIC 9(4).
       01 CurrentGraduationYear PIC 9(4).
       01 AboutMe PIC X(200).
       01 CurrentAboutMe PIC X(200).
       01 AboutLength PIC 99 VALUE 0.
       01 Experience-Title PIC X(200).
       01 CurrentTitle PIC X(200).
       01 Experience-Company PIC X(200).
       01 CurrentCompany PIC X(200).
       01 Experience-Dates PIC X(100).
       01 CurrentDates PIC X(100).
       01 Experience-Description PIC X(500).
       01 CurrentDescription PIC X(500).
       01 ExperienceCount PIC 9(1) VALUE 1.


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

       PostLoginMenu.
           MOVE "1. Create/Edit My Profile" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "2. View My Profile" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "3. Search for a job" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "4. Find someone you know" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "5. Learn a new skill" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadMenuOption
           EVALUATE MenuOption
               WHEN 1
                   PERFORM CreateEditMenu
               WHEN 2
                   PERFORM ViewProfile
               WHEN 3
                   PERFORM JobSearch
               WHEN 4
                   PERFORM FindSomeone
               WHEN 5
                   PERFORM LearnSkillMenu
               WHEN 9
                   MOVE "Exiting the program. Goodbye!" TO CurrentMessage
                   PERFORM DisplayAndLog
               WHEN OTHER
                   MOVE "Invalid option. Please try again." TO CurrentMessage
                   PERFORM DisplayAndLog
           END-EVALUATE.

       RegisterUser.
           IF UserCount >= MaxUsers THEN
               MOVE "User limit reached. Cannot register more users." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE "Enter username:" TO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM ReadUsername
               MOVE "Enter password:" TO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM ReadPassword
               PERFORM CheckPasswordLength
               PERFORM CheckUsernameExists
               IF UsernameExists = 'Y' THEN
                   MOVE "Username already exists. Please try a different username." TO CurrentMessage
                   PERFORM DisplayAndLog
               ELSE
                   OPEN EXTEND UserDataFile
                   MOVE CurrentUsername TO Username
                   MOVE CurrentPassword TO Password
                   WRITE UserRecord
                   CLOSE UserDataFile
                   ADD 1 TO UserCount
                   MOVE "Account created successfully." TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "Welcome " TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE CurrentUsername TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE 'Y' TO LoggedIn
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

       LearnSkillMenu.
           MOVE "Choose a skill to learn:" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "1. Time Management" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "2. Public Speaking" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "3. Leadership" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "4. Communication" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "5. Technical Skills" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "6. Back" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadSkillOption
           EVALUATE SkillOption
               WHEN 1 THRU 6
                   MOVE "This skill is under construction." TO CurrentMessage
                   PERFORM DisplayAndLog
               WHEN 6
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid option. Please try again." TO CurrentMessage
                   PERFORM DisplayAndLog
           END-EVALUATE.

       JobSearch.
           MOVE "This feature is under construction." TO CurrentMessage
           PERFORM DisplayAndLog.

       FindSomeone.
           MOVE "This feature is under construction." TO CurrentMessage
           PERFORM DisplayAndLog.
           EVALUATE SkillOption
               WHEN 1
                   DISPLAY "You have chosen to learn Time Management."
               WHEN 2
                   DISPLAY "You have chosen to learn Public Speaking."
               WHEN 3
                   DISPLAY "You have chosen to learn Leadership."
               WHEN 4
                   DISPLAY "You have chosen to learn Communication."
               WHEN 5
                   DISPLAY "You have chosen to learn Technical Skills."
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE.

       CreateEditMenu.
           MOVE "This is under development." TO CurrentMessage
              PERFORM DisplayAndLog
           MOVE "--- Create or Edit Profile ---" TO CurrentMessage
              PERFORM DisplayAndLog
              MOVE CurrentUsername TO CurrentMessage
                 PERFORM DisplayAndLog
           MOVE "First name: " TO CurrentMessage
              PERFORM DisplayAndLog
              PERFORM ReadFirstName
           MOVE "Last name: " TO CurrentMessage
              PERFORM DisplayAndLog
              PERFORM ReadLastName
           MOVE "University/College: " TO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM Readuniversity
           MOVE "Major: " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadMajor
           MOVE "Graduation Year: " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadGradYear
           MOVE "About Me (Optional 200 characters max or blank line to skip): " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadAboutMe
           MOVE "Add Experience (Optional, 3 Entries or Enter 'DONE' to finish): " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadExperience


              OPEN EXTEND UserProfileRecordFile
                   MOVE SPACES TO UserProfileRecord
                   MOVE CurrentUsername TO Username-Profile IN UserProfileRecord
                   MOVE CurrentFirstName TO FirstName IN UserProfileRecord
                   MOVE CurrentLastName TO LastName IN UserProfileRecord
                   MOVE CurrentUniversity TO University IN UserProfileRecord
                   MOVE CurrentMajor TO Major IN UserProfileRecord
                   MOVE CurrentGraduationYear TO GraduationYear IN UserProfileRecord
                   STRING "     " DELIMITED BY SIZE
                       FUNCTION TRIM(CurrentAboutMe) DELIMITED BY SIZE
                       INTO AboutMe IN UserProfileRecord
                   MOVE CurrentTitle TO Experience-Title IN UserProfileRecord
                   MOVE CurrentCompany TO Experience-Company IN UserProfileRecord
                   MOVE CurrentDates TO Experience-Dates IN UserProfileRecord
                   MOVE CurrentDescription TO Experience-Description IN UserProfileRecord



                   WRITE UserProfileRecord
                   CLOSE UserProfileRecordFile
                   MOVE "DONE" TO CurrentMessage
              PERFORM DisplayAndLog.

           ViewProfile.
           Move "This is under construction." TO CurrentMessage
              PERFORM DisplayAndLog.


       ReadMenuOption.
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

       CheckUsernameExists.
           MOVE 'N' TO UsernameExists
           MOVE 'N' TO EOF-UserData
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
