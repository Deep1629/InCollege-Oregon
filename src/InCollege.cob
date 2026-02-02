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
       MainSection.
           OPEN INPUT InputFile
           PERFORM CountExistingUsers
           PERFORM DisplayWelcome
           MOVE 'N' TO LoggedIn
           PERFORM InitialMenu UNTIL LoggedIn = 'Y' OR MenuOption = 9
           IF LoggedIn = 'Y' THEN
               MOVE 0 TO MenuOption
               MOVE 0 TO SkillOption
               PERFORM PostLoginMenu UNTIL MenuOption = 9
           END-IF
           CLOSE InputFile
           STOP RUN.

       WriteOutput.
           OPEN OUTPUT OutputFile
           WRITE OutputRecord
           CLOSE OutputFile.

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
         MOVE "Welcome to InCollege!" TO CurrentMessage
         PERFORM DisplayAndLog.

       InitialMenu.
           MOVE "1. Log In" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "2. Create New Account" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "9. Exit" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadMenuOption
           EVALUATE MenuOption
               WHEN 1
                   PERFORM LoginUser
               WHEN 2
                   PERFORM RegisterUser
               WHEN 9
                   MOVE "Exiting the program. Goodbye!" TO CurrentMessage
                   PERFORM DisplayAndLog
               WHEN OTHER
                   MOVE "Invalid option. Please try again." TO CurrentMessage
                   PERFORM DisplayAndLog
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

       LoginUser.
           MOVE 'N' TO LoginSuccess
           MOVE 'N' TO EOF-UserData
           MOVE "Enter username:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadUsername
           MOVE "Enter password:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadPassword
           OPEN INPUT UserDataFile
           PERFORM UNTIL LoginSuccess = 'Y' OR EOF-UserData = 'Y'
               READ UserDataFile INTO UserRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   IF Username = CurrentUsername AND Password = CurrentPassword THEN
                       MOVE "You have successfully logged in." TO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE "Welcome " TO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE CurrentUsername TO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE 'Y' TO LoginSuccess
                       MOVE 'Y' TO LoggedIn
                   END-IF
           END-PERFORM
           CLOSE UserDataFile
           IF LoginSuccess = 'N' THEN
               MOVE "Invalid credentials. Please try again." TO CurrentMessage
               PERFORM DisplayAndLog
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

       ReadSkillOption.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE 0 TO SkillOption
           NOT AT END
               MOVE InputRecord(1:1) TO InputChar
               MOVE FUNCTION NUMVAL-C(InputChar) TO SkillOption
           END-READ.

       CheckPasswordLength.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(CurrentPassword)) TO PasswordLength
           IF PasswordLength > 12 THEN
               MOVE "Password must be 12 characters or less. Program terminated." TO CurrentMessage
               PERFORM DisplayAndLog
               CLOSE InputFile
               IF OutputFileInitialized = 'Y' THEN
                   OPEN EXTEND OutputFile
                   CLOSE OutputFile
               END-IF
               STOP RUN
           END-IF.

           ReadFirstName.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentFirstName
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentFirstName
           END-READ.

           ReadLastName.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentLastName
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentLastName
           END-READ.

           ReadUniversity.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentUniversity
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentUniversity
           END-READ.

           ReadMajor.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentMajor
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentMajor
           END-READ.
           ReadGradYear.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE 0 TO CurrentGraduationYear
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:4)) TO TempString
               IF FUNCTION TRIM(TempString) IS NUMERIC THEN
                   MOVE FUNCTION NUMVAL(TempString) TO CurrentGraduationYear
               ELSE
                     MOVE "Invalid Graduation Year" TO CurrentMessage
                     PERFORM DisplayAndLog
                END-IF
           END-READ.

           ReadAboutMe.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentAboutMe
           NOT AT END
               IF InputRecord(200:1) NOT = SPACE THEN
                   MOVE "About Me description exceeds 200 characters. Must be 200 or less." TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentAboutMe
               ELSE
                   MOVE FUNCTION TRIM(InputRecord(1:200)) TO CurrentAboutMe
               END-IF
           END-READ.

           ReadExperience.
           MOVE SPACES TO CurrentMessage
                       STRING "Experience #" DELIMITED BY SIZE
                       ExperienceCount DELIMITED BY SIZE
                       " - Title: " DELIMITED BY SIZE INTO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentTitle
                  NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentTitle
               IF CurrentTitle = "DONE" OR ExperienceCount >= 4 THEN
                   MOVE 'Y' TO EOF-InputFile
                   MOVE SPACES TO CurrentTitle
               END-IF

           MOVE SPACES TO CurrentMessage
           STRING "Experience #" DELIMITED BY SIZE
           ExperienceCount DELIMITED BY SIZE
           " - Company/Organization: " DELIMITED BY SIZE INTO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
                MOVE 'Y' TO EOF-InputFile
                MOVE SPACES TO CurrentCompany
              NOT AT END
           MOVE InputRecord(1:20) TO TempString
              MOVE FUNCTION TRIM(TempString) TO CurrentCompany

           MOVE SPACES TO CurrentMessage
           STRING "Experience #" DELIMITED BY SIZE
           ExperienceCount DELIMITED BY SIZE
           " - Dates: " DELIMITED BY SIZE INTO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
                MOVE 'Y' TO EOF-InputFile
                MOVE SPACES TO CurrentDates
              NOT AT END
           MOVE InputRecord(1:20) TO TempString
              MOVE FUNCTION TRIM(TempString) TO CurrentDates

           MOVE SPACES TO CurrentMessage
           STRING "Experience #" DELIMITED BY SIZE
           ExperienceCount DELIMITED BY SIZE
           " - Description (optional 100 characters max, blank line to skip): " DELIMITED BY SIZE INTO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
                MOVE 'Y' TO EOF-InputFile
                MOVE SPACES TO CurrentDescription
              NOT AT END
           MOVE InputRecord(1:20) TO TempString
              MOVE FUNCTION TRIM(TempString) TO CurrentDescription

           END-READ.




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

