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
           SELECT ConnectionRequestFile ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempConnectionFile ASSIGN TO "connections_temp.dat"
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
           05 Education-Degree PIC X(50).
           05 Education-Universiity PIC X(50).
           05 Education-Years PIC X(9).

       FD ConnectionRequestFile.
       01 ConnectionRecord.
           05 FromUsername PIC X(20).
           05 ToUsername PIC X(20).
           05 ConnectionStatus PIC X(10).

       FD TempConnectionFile.
       01 TempConnectionRecord.
           05 FromUsername PIC X(20).
           05 ToUsername PIC X(20).
           05 ConnectionStatus PIC X(10).

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
       01 PasswordValid PIC X VALUE 'Y'.
       01 has_capital PIC X VALUE 'N'.
       01 has_digit PIC X VALUE 'N'.
       01 has_special PIC X VALUE 'N'.
       01 TempChar PIC X.
       01 I PIC 9(2) VALUE 1.
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
       01 Experience-Title-Acc PIC X(200).
       01 CurrentTitle PIC X(200).
       01 Experience-Company-Acc PIC X(200).
       01 CurrentCompany PIC X(200).
       01 Experience-Dates-Acc PIC X(100).
       01 CurrentDates PIC X(100).
       01 Experience-Description-Acc PIC X(500).
       01 CurrentDescription PIC X(500).
       01 ExperienceCount PIC 9(1) VALUE 1.
       01 EducationCount PIC 9(1) VALUE 1.
       01 Education-Degree-Acc PIC X(50).
       01 CurrentEducationDegree PIC X(50).
       01 Education-Universiity-Acc PIC X(50).
       01 CurrentEducationUniversity PIC X(50).
       01 Education-Years-Acc PIC X(9).
       01 CurrentEducationYears PIC X(9).
       01 SearchQuery PIC X(40).
       01 FoundProfile PIC X VALUE 'N'.
       01 SearchedUsername PIC X(20).
       01 EOF-ConnectionFile PIC X VALUE 'N'.
       01 ConnectionRequest-FromUser PIC X(20).
       01 ConnectionRequest-ToUser PIC X(20).
       01 ConnectionRequest-Status PIC X(10).
       01 ConnectionFound PIC X VALUE 'N'.
       01 SendRequest PIC X VALUE 'N'.
       01 MenuInput PIC X.
       01 PendingRequestFound PIC X VALUE 'N'.
       01 ConnectionConnected PIC X VALUE 'N'.
       01 RequestIndex PIC 9(3) VALUE 0.
       01 RequestFound PIC X VALUE 'N'.
       01 ConnectionAccepted PIC X VALUE 'N'.
       01 AcceptFromUsername PIC X(20).
       01 AcceptToUsername PIC X(20).
       01 FoundFirstName PIC X(20).
       01 FoundLastName PIC X(20).
       01 FoundUniversity PIC X(30).
       01 FoundMajor PIC X(30).
       01 FoundGraduationYear PIC 9(4).
       01 FoundAboutMe PIC X(200).
       01 FoundExperienceTitle PIC X(200).
       01 FoundExperienceCompany PIC X(200).
       01 FoundExperienceDates PIC X(100).
       01 FoundExperienceDescription PIC X(500).
       01 FoundEducationDegree PIC X(50).
       01 FoundEducationUniversity PIC X(50).
       01 FoundEducationYears PIC X(9).

       01 IncomingRequestFound PIC X VALUE 'N'.
       01 IncomingFromUsername PIC X(20).


       PROCEDURE DIVISION.
       MainSection.
           OPEN INPUT InputFile
           PERFORM CountExistingUsers
           MOVE 'N' TO LoggedIn
           PERFORM InitialMenu UNTIL LoggedIn = 'Y' OR MenuOption = 9
           PERFORM UNTIL MenuOption = 9
               IF LoggedIn = 'Y' THEN
                   PERFORM PostLoginMenu
               ELSE
                   PERFORM InitialMenu
               END-IF
           END-PERFORM
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
           MOVE "Welcome to InCollege!" TO CurrentMessage
           PERFORM DisplayAndLog
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
           MOVE 'Y' TO LoggedIn
           PERFORM UNTIL LoggedIn = 'N'
               MOVE "-------------------------" TO CurrentMessage
               PERFORM DisplayAndLog
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
               MOVE "6. View My Pending Connection Requests" TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE "7. Logout" TO CurrentMessage
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
                   WHEN 6
                       PERFORM ViewPendingRequests
                   WHEN 7
                       MOVE "Logging out..." TO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE 'N' TO LoggedIn
                   WHEN 9
                       MOVE "Exiting the program. Goodbye!" TO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE 9 TO MenuOption
                   WHEN OTHER
                       MOVE "Invalid option. Please try again." TO CurrentMessage
                       PERFORM DisplayAndLog
               END-EVALUATE
           END-PERFORM.

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
               IF PasswordValid = 'N' THEN
                   MOVE "Registration failed due to invalid password." TO CurrentMessage
                   PERFORM DisplayAndLog
               ELSE
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
                       MOVE SPACES TO CurrentMessage
                       STRING "Welcome, " DELIMITED BY SIZE
                           FUNCTION TRIM(CurrentUsername) DELIMITED BY SIZE
                           "!" DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE 'Y' TO LoggedIn
                   END-IF
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
                       MOVE SPACES TO CurrentMessage
                       STRING "Welcome, " DELIMITED BY SIZE
                           FUNCTION TRIM(CurrentUsername) DELIMITED BY SIZE
                           "!" DELIMITED BY SIZE
                           INTO CurrentMessage
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
           MOVE "Enter the full name of the person you are looking for:" TO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO SearchQuery
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:40)) TO SearchQuery
           END-READ

           IF FUNCTION TRIM(SearchQuery) = SPACES THEN
               MOVE "No one by that name could be found." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE 'N' TO FoundProfile
               MOVE 'N' TO EOF-UserData
               OPEN INPUT UserProfileRecordFile
               PERFORM UNTIL FoundProfile = 'Y' OR EOF-UserData = 'Y'
                   READ UserProfileRecordFile INTO UserProfileRecord
                   AT END
                       MOVE 'Y' TO EOF-UserData
                   NOT AT END
                       IF Username-Profile IN UserProfileRecord NOT = CurrentUsername THEN
                           MOVE SPACES TO TempString
                           STRING FUNCTION TRIM(FirstName IN UserProfileRecord) DELIMITED BY SIZE
                               " " DELIMITED BY SIZE
                               FUNCTION TRIM(LastName IN UserProfileRecord) DELIMITED BY SIZE
                               INTO TempString
                           IF FUNCTION TRIM(TempString) = FUNCTION TRIM(SearchQuery) THEN
                               MOVE 'Y' TO FoundProfile
                               MOVE Username-Profile IN UserProfileRecord TO SearchedUsername
                               MOVE FirstName IN UserProfileRecord TO FoundFirstName
                               MOVE LastName IN UserProfileRecord TO FoundLastName
                               MOVE University IN UserProfileRecord TO FoundUniversity
                               MOVE Major IN UserProfileRecord TO FoundMajor
                               MOVE GraduationYear IN UserProfileRecord TO FoundGraduationYear
                               MOVE AboutMe IN UserProfileRecord TO FoundAboutMe
                               MOVE Experience-Title IN UserProfileRecord TO FoundExperienceTitle
                               MOVE Experience-Company IN UserProfileRecord TO FoundExperienceCompany
                               MOVE Experience-Dates IN UserProfileRecord TO FoundExperienceDates
                               MOVE Experience-Description IN UserProfileRecord TO FoundExperienceDescription
                               MOVE Education-Degree IN UserProfileRecord TO FoundEducationDegree
                               MOVE Education-Universiity IN UserProfileRecord TO FoundEducationUniversity
                               MOVE Education-Years IN UserProfileRecord TO FoundEducationYears
                           END-IF
                       END-IF
               END-PERFORM
               CLOSE UserProfileRecordFile

               IF FoundProfile = 'Y' THEN
                   MOVE "--- Found User Profile ---" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Name: " DELIMITED BY SIZE
                       FUNCTION TRIM(FoundFirstName) DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       FUNCTION TRIM(FoundLastName) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "University: " DELIMITED BY SIZE
                       FUNCTION TRIM(FoundUniversity) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Major: " DELIMITED BY SIZE
                       FUNCTION TRIM(FoundMajor) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Graduation Year: " DELIMITED BY SIZE
                       FUNCTION TRIM(FoundGraduationYear) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "About Me: " DELIMITED BY SIZE
                       FUNCTION TRIM(FoundAboutMe) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "Experience:" TO CurrentMessage
                   PERFORM DisplayAndLog
                   IF FUNCTION TRIM(FoundExperienceTitle) NOT = SPACES THEN
                       MOVE SPACES TO CurrentMessage
                       STRING "Title: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundExperienceTitle) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Company: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundExperienceCompany) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Dates: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundExperienceDates) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Description: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundExperienceDescription) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                   ELSE
                       MOVE "None" TO CurrentMessage
                       PERFORM DisplayAndLog
                   END-IF
                   MOVE "Education:" TO CurrentMessage
                   PERFORM DisplayAndLog
                   IF FUNCTION TRIM(FoundEducationDegree) NOT = SPACES THEN
                       MOVE SPACES TO CurrentMessage
                       STRING "Degree: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundEducationDegree) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "University: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundEducationUniversity) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Years: " DELIMITED BY SIZE
                           FUNCTION TRIM(FoundEducationYears) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                   ELSE
                       MOVE "None" TO CurrentMessage
                       PERFORM DisplayAndLog
                   END-IF

                   MOVE 'N' TO IncomingRequestFound
                   MOVE 'N' TO EOF-ConnectionFile
                   OPEN INPUT ConnectionRequestFile
                   PERFORM UNTIL EOF-ConnectionFile = 'Y'
                       READ ConnectionRequestFile INTO ConnectionRecord
                       AT END
                           MOVE 'Y' TO EOF-ConnectionFile
                       NOT AT END
                           IF FromUsername IN ConnectionRecord = SearchedUsername AND
                              ToUsername IN ConnectionRecord = CurrentUsername AND
                              ConnectionStatus IN ConnectionRecord = "Pending"
                           THEN
                               MOVE 'Y' TO IncomingRequestFound
                               MOVE FromUsername IN ConnectionRecord TO IncomingFromUsername
                           END-IF
                       END-READ
                   END-PERFORM
                   CLOSE ConnectionRequestFile

                   IF IncomingRequestFound = 'Y' THEN
                       MOVE SPACES TO CurrentMessage
                       STRING FUNCTION TRIM(SearchedUsername) DELIMITED BY SIZE
                           " has sent you a connection request. Accept? (Y/N):" DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       READ InputFile INTO InputRecord
                       AT END
                           MOVE 'Y' TO EOF-InputFile
                           MOVE 'N' TO SendRequest
                       NOT AT END
                           MOVE InputRecord(1:1) TO SendRequest
                       END-READ

                       IF SendRequest = 'Y' OR SendRequest = 'y' THEN
                           PERFORM AcceptIncomingRequest
                       ELSE
                           MOVE "Request ignored." TO CurrentMessage
                           PERFORM DisplayAndLog
                       END-IF
                   ELSE
                       MOVE "Send connection request? (Y/N):" TO CurrentMessage
                       PERFORM DisplayAndLog
                       READ InputFile INTO InputRecord
                       AT END
                           MOVE 'Y' TO EOF-InputFile
                           MOVE 'N' TO SendRequest
                       NOT AT END
                           MOVE InputRecord(1:1) TO SendRequest
                       END-READ

                       IF SendRequest = 'Y' OR SendRequest = 'y' THEN
                           PERFORM SendConnectionRequest
                       END-IF
                   END-IF
               ELSE
                   MOVE "No one by that name could be found." TO CurrentMessage
                   PERFORM DisplayAndLog
               END-IF
           END-IF.

       AcceptIncomingRequest.
           MOVE 'N' TO EOF-ConnectionFile
           OPEN INPUT ConnectionRequestFile
           OPEN OUTPUT TempConnectionFile
           PERFORM UNTIL EOF-ConnectionFile = 'Y'
               READ ConnectionRequestFile INTO ConnectionRecord
               AT END
                   MOVE 'Y' TO EOF-ConnectionFile
               NOT AT END
                   IF FromUsername IN ConnectionRecord = SearchedUsername AND
                      ToUsername IN ConnectionRecord = CurrentUsername AND
                      ConnectionStatus IN ConnectionRecord = "Pending"
                   THEN
                       MOVE "Connected" TO ConnectionStatus IN ConnectionRecord
                   END-IF
                   WRITE TempConnectionRecord FROM ConnectionRecord
               END-READ
           END-PERFORM
           CLOSE ConnectionRequestFile
           CLOSE TempConnectionFile

           CALL "CBL_DELETE_FILE" USING "connections.dat"
           CALL "CBL_RENAME_FILE" USING "connections_temp.dat"
               "connections.dat"

           MOVE "Connection request accepted successfully." TO CurrentMessage
           PERFORM DisplayAndLog.

       CreateEditMenu.
           MOVE "--- Create/Edit Profile ---" TO CurrentMessage
              PERFORM DisplayAndLog
           MOVE "Enter First Name: " TO CurrentMessage
              PERFORM DisplayAndLog
              PERFORM ReadFirstName
           MOVE "Enter Last Name: " TO CurrentMessage
              PERFORM DisplayAndLog
              PERFORM ReadLastName
           MOVE "Enter University/College Attended: " TO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM Readuniversity
           MOVE "Enter Major: " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadMajor
           MOVE "Enter Graduation Year (YYYY): " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadGradYear
           MOVE "Enter About Me (optional, max 200 characters, enter blank line to skip): " TO CurrentMessage
                PERFORM DisplayAndLog
                PERFORM ReadAboutMe
           MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish): " TO CurrentMessage
                PERFORM DisplayAndLog
                MOVE SPACES TO Experience-Title-Acc
                MOVE SPACES TO Experience-Company-Acc
                MOVE SPACES TO Experience-Dates-Acc
                MOVE SPACES TO Experience-Description-Acc
                MOVE 1 TO ExperienceCount
                PERFORM UNTIL ExperienceCount > 3
                   MOVE SPACES TO CurrentMessage
                   STRING "Experience " ExperienceCount " - Title:" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentTitle
                   NOT AT END
                       MOVE InputRecord(1:20) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentTitle
                   END-READ
                   IF CurrentTitle = "DONE" OR CurrentTitle = SPACES THEN
                       EXIT PERFORM
                   END-IF
                   IF ExperienceCount > 1 THEN
                       STRING Experience-Title-Acc DELIMITED BY LOW-VALUES " | " CurrentTitle DELIMITED BY LOW-VALUES INTO Experience-Title-Acc
                   ELSE
                       MOVE CurrentTitle TO Experience-Title-Acc
                   END-IF
                   MOVE SPACES TO CurrentMessage
                   STRING "Experience " ExperienceCount " - Company:" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentCompany
                   NOT AT END
                       MOVE InputRecord(1:20) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentCompany
                   END-READ
                   IF ExperienceCount > 1 THEN
                       STRING Experience-Company-Acc DELIMITED BY LOW-VALUES " | " CurrentCompany DELIMITED BY LOW-VALUES INTO Experience-Company-Acc
                   ELSE
                       MOVE CurrentCompany TO Experience-Company-Acc
                   END-IF
                   MOVE SPACES TO CurrentMessage
                   STRING "Experience " ExperienceCount " - Dates:" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentDates
                   NOT AT END
                       MOVE InputRecord(1:20) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentDates
                   END-READ
                   IF ExperienceCount > 1 THEN
                       STRING Experience-Dates-Acc DELIMITED BY LOW-VALUES " | " CurrentDates DELIMITED BY LOW-VALUES INTO Experience-Dates-Acc
                   ELSE
                       MOVE CurrentDates TO Experience-Dates-Acc
                   END-IF
                   MOVE SPACES TO CurrentMessage
                   STRING "Experience " ExperienceCount " - Description (optional 100 characters max, blank line to skip):" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentDescription
                   NOT AT END
                       MOVE InputRecord(1:100) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentDescription
                   END-READ
                   IF ExperienceCount > 1 THEN
                       STRING Experience-Description-Acc DELIMITED BY LOW-VALUES " | " CurrentDescription DELIMITED BY LOW-VALUES INTO Experience-Description-Acc
                   ELSE
                       MOVE CurrentDescription TO Experience-Description-Acc
                   END-IF
                   ADD 1 TO ExperienceCount
                END-PERFORM
           MOVE "Add Education (Optional, 3 Entries or Enter 'DONE' to finish): " TO CurrentMessage
                PERFORM DisplayAndLog
                MOVE SPACES TO Education-Degree-Acc
                MOVE SPACES TO Education-Universiity-Acc
                MOVE SPACES TO Education-Years-Acc
                MOVE 1 TO EducationCount
                PERFORM UNTIL EducationCount > 3
                   MOVE SPACES TO CurrentMessage
                   STRING "Education " EducationCount " - Degree:" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentEducationDegree
                   NOT AT END
                       MOVE InputRecord(1:20) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentEducationDegree
                   END-READ
                   IF CurrentEducationDegree = "DONE" OR CurrentEducationDegree = SPACES THEN
                       EXIT PERFORM
                   END-IF
                   IF EducationCount > 1 THEN
                       STRING Education-Degree-Acc DELIMITED BY LOW-VALUES " | " CurrentEducationDegree DELIMITED BY LOW-VALUES INTO Education-Degree-Acc
                   ELSE
                       MOVE CurrentEducationDegree TO Education-Degree-Acc
                   END-IF
                   MOVE SPACES TO CurrentMessage
                   STRING "Education " EducationCount " - University/College:" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentEducationUniversity
                   NOT AT END
                       MOVE InputRecord(1:20) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentEducationUniversity
                   END-READ
                   IF EducationCount > 1 THEN
                       STRING Education-Universiity-Acc DELIMITED BY LOW-VALUES " | " CurrentEducationUniversity DELIMITED BY LOW-VALUES INTO Education-Universiity-Acc
                   ELSE
                       MOVE CurrentEducationUniversity TO Education-Universiity-Acc
                   END-IF
                   MOVE SPACES TO CurrentMessage
                   STRING "Education " EducationCount " - Years Attended:" INTO CurrentMessage
                   PERFORM DisplayAndLog
                   READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO EOF-InputFile
                       MOVE SPACES TO CurrentEducationYears
                   NOT AT END
                       MOVE InputRecord(1:20) TO TempString
                       MOVE FUNCTION TRIM(TempString) TO CurrentEducationYears
                   END-READ
                   IF EducationCount > 1 THEN
                       STRING Education-Years-Acc DELIMITED BY LOW-VALUES " | " CurrentEducationYears DELIMITED BY LOW-VALUES INTO Education-Years-Acc
                   ELSE
                       MOVE CurrentEducationYears TO Education-Years-Acc
                   END-IF
                   ADD 1 TO EducationCount
                END-PERFORM
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
           WRITE UserProfileRecord
           CLOSE UserProfileRecordFile
           MOVE "Profile saved successfully!" TO CurrentMessage
           PERFORM DisplayAndLog.

           ViewProfile.
           MOVE 'N' TO EOF-UserData
           MOVE 'N' TO LoginSuccess
           OPEN INPUT UserProfileRecordFile
           PERFORM UNTIL LoginSuccess = 'Y' OR EOF-UserData = 'Y'
               READ UserProfileRecordFile INTO UserProfileRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   IF Username-Profile IN UserProfileRecord = CurrentUsername THEN
                       MOVE "--- Your Profile ---" TO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Name: " DELIMITED BY SIZE
                           FUNCTION TRIM(FirstName IN UserProfileRecord) DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(LastName IN UserProfileRecord) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "University: " DELIMITED BY SIZE
                           FUNCTION TRIM(University IN UserProfileRecord) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Major: " DELIMITED BY SIZE
                           FUNCTION TRIM(Major IN UserProfileRecord) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "Graduation Year: " DELIMITED BY SIZE
                           GraduationYear IN UserProfileRecord DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE SPACES TO CurrentMessage
                       STRING "About Me: " DELIMITED BY SIZE
                           FUNCTION TRIM(AboutMe IN UserProfileRecord) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                       MOVE "Experience:" TO CurrentMessage
                       PERFORM DisplayAndLog
                       IF FUNCTION TRIM(Experience-Title IN UserProfileRecord) NOT = SPACES THEN
                           MOVE SPACES TO CurrentMessage
                           STRING "Title: " DELIMITED BY SIZE
                               FUNCTION TRIM(Experience-Title IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE SPACES TO CurrentMessage
                           STRING "Company: " DELIMITED BY SIZE
                               FUNCTION TRIM(Experience-Company IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE SPACES TO CurrentMessage
                           STRING "Dates: " DELIMITED BY SIZE
                               FUNCTION TRIM(Experience-Dates IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE SPACES TO CurrentMessage
                           STRING "Description: " DELIMITED BY SIZE
                               FUNCTION TRIM(Experience-Description IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                       ELSE
                           MOVE "None" TO CurrentMessage
                           PERFORM DisplayAndLog
                       END-IF
                       MOVE "Education:" TO CurrentMessage
                       PERFORM DisplayAndLog
                       IF FUNCTION TRIM(Education-Degree IN UserProfileRecord) NOT = SPACES THEN
                           MOVE SPACES TO CurrentMessage
                           STRING "Degree: " DELIMITED BY SIZE
                               FUNCTION TRIM(Education-Degree IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE SPACES TO CurrentMessage
                           STRING "University: " DELIMITED BY SIZE
                               FUNCTION TRIM(Education-Universiity IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE SPACES TO CurrentMessage
                           STRING "Years: " DELIMITED BY SIZE
                               FUNCTION TRIM(Education-Years IN UserProfileRecord) DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                       ELSE
                           MOVE "None" TO CurrentMessage
                           PERFORM DisplayAndLog
                       END-IF
                       MOVE 'Y' TO LoginSuccess
                   END-IF
           END-PERFORM
           CLOSE UserProfileRecordFile
           IF LoginSuccess = 'N' THEN
               MOVE "No profile found. Please create a profile first." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF.

       COPY "SendRequest.cob".

       COPY "AcceptRequest.cob".

       COPY "ViewRequests.cob".

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
               IF FUNCTION TRIM(CurrentPassword) = SPACES THEN
                   MOVE "Warning: No password provided." TO CurrentMessage
                   PERFORM DisplayAndLog
               END-IF
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
           MOVE 'Y' TO PasswordValid
           IF PasswordLength < 8 THEN
               MOVE "Password must be at least 8 characters." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF
           IF PasswordLength > 12 THEN
               MOVE "Password must be 12 characters or less. Program terminated." TO CurrentMessage
               PERFORM DisplayAndLog
               CLOSE InputFile
               IF OutputFileInitialized = 'Y' THEN
                   OPEN EXTEND OutputFile
                   CLOSE OutputFile
               END-IF
               STOP RUN
           END-IF
           MOVE 'N' TO has_capital
           MOVE 'N' TO has_digit
           MOVE 'N' TO has_special
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PasswordLength
               MOVE CurrentPassword(I:1) TO TempChar
               IF TempChar >= 'A' AND TempChar <= 'Z' THEN
                   MOVE 'Y' TO has_capital
               END-IF
               IF TempChar >= '0' AND TempChar <= '9' THEN
                   MOVE 'Y' TO has_digit
               END-IF
               IF NOT (TempChar >= 'A' AND TempChar <= 'Z' OR TempChar >= 'a' AND TempChar <= 'z' OR TempChar >= '0' AND TempChar <= '9') THEN
                   MOVE 'Y' TO has_special
               END-IF
           END-PERFORM
           IF has_capital = 'N' THEN
               MOVE "Password must contain at least one capital letter." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF
           IF has_digit = 'N' THEN
               MOVE "Password must contain at least one digit." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF
           IF has_special = 'N' THEN
               MOVE "Password must contain at least one special character." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF.

           ReadFirstName.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentFirstName
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentFirstName
           END-READ
           IF FUNCTION TRIM(CurrentFirstName) = SPACES THEN
               MOVE "First name missing; set to N/A." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE "N/A" TO CurrentFirstName
           END-IF.

           ReadLastName.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentLastName
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentLastName
           END-READ
           IF FUNCTION TRIM(CurrentLastName) = SPACES THEN
               MOVE "Last name missing; set to N/A." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE "N/A" TO CurrentLastName
           END-IF.

           ReadUniversity.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentUniversity
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentUniversity
           END-READ
           IF FUNCTION TRIM(CurrentUniversity) = SPACES THEN
               MOVE "University missing; set to N/A." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE "N/A" TO CurrentUniversity
           END-IF.

           ReadMajor.
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentMajor
           NOT AT END
               MOVE InputRecord(1:20) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentMajor
           END-READ
           IF FUNCTION TRIM(CurrentMajor) = SPACES THEN
               MOVE "Major missing; set to N/A." TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE "N/A" TO CurrentMajor
           END-IF.

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
           END-READ
           *> Validate graduation year range
           IF CurrentGraduationYear < 1900 OR CurrentGraduationYear > 2100 THEN
               IF CurrentGraduationYear = 0 THEN
                   MOVE "Graduation Year not provided; set to 0." TO CurrentMessage
               ELSE
                   MOVE "Graduation Year out of range; set to 0." TO CurrentMessage
               END-IF
               PERFORM DisplayAndLog
               MOVE 0 TO CurrentGraduationYear
           END-IF.

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
