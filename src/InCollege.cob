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
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS UserFileStatus.
           SELECT UserProfileRecordFile ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ProfileFileStatus.
           SELECT TempProfileFile ASSIGN TO "profiles.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TempFileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputRecord PIC X(200).

       FD OutputFile.
       01 OutputRecord PIC X(200).

       FD UserDataFile.
       01 UserRecord.
           05 Username PIC X(20).
           05 Password PIC X(20).

       FD UserProfileRecordFile.
       01 UserProfileRecord.
           05 ProfileUsername PIC X(20).
           05 ProfileFirstName PIC X(20).
           05 ProfileLastName PIC X(20).
           05 ProfileUniversity PIC X(30).
           05 ProfileMajor PIC X(30).
           05 ProfileGraduationYear PIC 9(4).
           05 ProfileAboutMe PIC X(200).
           05 ProfileExperienceTitles PIC X(200).
           05 ProfileExperienceCompanies PIC X(200).
           05 ProfileExperienceDates PIC X(200).
           05 ProfileExperienceDescriptions PIC X(200).
           05 ProfileEducationDegrees PIC X(200).
           05 ProfileEducationUniversities PIC X(200).
           05 ProfileEducationYears PIC X(200).

       FD TempProfileFile.
       01 TempProfileRecord.
           05 TempProfileUsername PIC X(20).
           05 TempProfileFirstName PIC X(20).
           05 TempProfileLastName PIC X(20).
           05 TempProfileUniversity PIC X(30).
           05 TempProfileMajor PIC X(30).
           05 TempProfileGraduationYear PIC 9(4).
           05 TempProfileAboutMe PIC X(200).
           05 TempProfileExperienceTitles PIC X(200).
           05 TempProfileExperienceCompanies PIC X(200).
           05 TempProfileExperienceDates PIC X(200).
           05 TempProfileExperienceDescriptions PIC X(200).
           05 TempProfileEducationDegrees PIC X(200).
           05 TempProfileEducationUniversities PIC X(200).
           05 TempProfileEducationYears PIC X(200).

       WORKING-STORAGE SECTION.
       01 UserCount PIC 9(3) VALUE 0.
       01 MaxUsers PIC 9(3) VALUE 999.
       01 LoggedIn PIC X VALUE 'N'.
       01 UserFileStatus PIC XX.
       01 ProfileFileStatus PIC XX.
       01 TempFileStatus PIC XX.
       01 CurrentUsername PIC X(20) VALUE SPACES.
       01 CurrentPassword PIC X(20) VALUE SPACES.
       01 MenuOption PIC 9 VALUE 0.
       01 LoginSuccess PIC X VALUE 'N'.
       01 EOF-UserData PIC X VALUE 'N'.
       01 EOF-InputFile PIC X VALUE 'N'.
       01 EOF-ProfileData PIC X VALUE 'N'.
       01 InputChar PIC X.
       01 CurrentMessage PIC X(200) VALUE SPACES.
       01 OutputFileInitialized PIC X VALUE 'N'.
       01 UsernameExists PIC X VALUE 'N'.
       01 PasswordLength PIC 99 VALUE 0.
       01 TempString PIC X(200) VALUE SPACES.
       01 PasswordValid PIC X VALUE 'Y'.
       01 has_capital PIC X VALUE 'N'.
       01 has_digit PIC X VALUE 'N'.
       01 has_special PIC X VALUE 'N'.
       01 TempChar PIC X.
       01 I PIC 9(3) VALUE 1.
       01 CurrentFirstName PIC X(20) VALUE SPACES.
       01 CurrentLastName PIC X(20) VALUE SPACES.
       01 CurrentUniversity PIC X(30) VALUE SPACES.
       01 CurrentMajor PIC X(30) VALUE SPACES.
       01 CurrentGraduationYear PIC 9(4) VALUE 0.
       01 CurrentAboutMe PIC X(200) VALUE SPACES.
       01 ExperienceCount PIC 9(1) VALUE 0.
       01 EducationCount PIC 9(1) VALUE 0.
       01 ExperienceTitles PIC X(200) VALUE SPACES.
       01 ExperienceCompanies PIC X(200) VALUE SPACES.
       01 ExperienceDates PIC X(200) VALUE SPACES.
       01 ExperienceDescriptions PIC X(200) VALUE SPACES.
       01 EducationDegrees PIC X(200) VALUE SPACES.
       01 EducationUniversities PIC X(200) VALUE SPACES.
       01 EducationYears PIC X(200) VALUE SPACES.
       01 EditProfile PIC X VALUE 'N'.
       01 ProfileFound PIC X VALUE 'N'.
       01 SearchQuery PIC X(40) VALUE SPACES.
       01 FoundName PIC X(40) VALUE SPACES.
       01 J PIC 9(3) VALUE 1.
       01 K PIC 9(3) VALUE 1.
       01 SearchFound PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MainSection.
           OPEN INPUT InputFile
           PERFORM CountExistingUsers
           PERFORM DisplayWelcome
           PERFORM UNTIL EOF-InputFile = 'Y'
               MOVE 'N' TO LoggedIn
               MOVE 0 TO MenuOption
               PERFORM InitialMenu UNTIL LoggedIn = 'Y' OR MenuOption = 9
               IF LoggedIn = 'Y' THEN
                   MOVE 0 TO MenuOption
                   PERFORM PostLoginMenu UNTIL MenuOption = 9
                   MOVE 'N' TO LoggedIn
                   MOVE 0 TO MenuOption
               END-IF
               IF MenuOption = 9 THEN
                   MOVE "Exiting the program. Goodbye!" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE 'Y' TO EOF-InputFile
               END-IF
           END-PERFORM
           CLOSE InputFile
           STOP RUN.

       CountExistingUsers.
           MOVE 0 TO UserCount
           OPEN INPUT UserDataFile
           IF UserFileStatus NOT = "00" THEN
               CLOSE UserDataFile
               OPEN OUTPUT UserDataFile
               CLOSE UserDataFile
           ELSE
               MOVE 'N' TO EOF-UserData
               PERFORM UNTIL EOF-UserData = 'Y'
                   READ UserDataFile INTO UserRecord
                   AT END
                       MOVE 'Y' TO EOF-UserData
                   NOT AT END
                       ADD 1 TO UserCount
                   END-READ
               END-PERFORM
               CLOSE UserDataFile
           END-IF.

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
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid option. Please try again." TO CurrentMessage
                   PERFORM DisplayAndLog
           END-EVALUATE.

       RegisterUser.
           IF UserCount >= 5
               MOVE "All permitted accounts have been created, please come back later" TO CurrentMessage
               PERFORM DisplayAndLog
               EXIT PARAGRAPH
           END-IF
           MOVE "Enter username:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadUsername
           MOVE 'N' TO UsernameExists
           IF FUNCTION TRIM(CurrentUsername) = SPACES THEN
               MOVE "Warning: No username provided."
                   TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'Y' TO UsernameExists
           ELSE
               PERFORM CheckUsernameExists
           END-IF
           IF UsernameExists = 'Y' THEN
               MOVE "Username already exists. Please try a different username."
                   TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE "Enter password:" TO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM ReadPassword
               PERFORM CheckPasswordLength
               IF PasswordValid = 'N' THEN
                   MOVE "Registration failed due to invalid password."
                       TO CurrentMessage
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
                   IF FUNCTION TRIM(Username) =
                       FUNCTION TRIM(CurrentUsername) AND
                       FUNCTION TRIM(Password) =
                       FUNCTION TRIM(CurrentPassword)
                   THEN
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

       PostLoginMenu.
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
           PERFORM ReadMenuOption
           EVALUATE MenuOption
               WHEN 1
                   PERFORM CreateEditProfile
               WHEN 2
                   PERFORM ViewProfile
               WHEN 3
                   MOVE "This feature is under construction." TO CurrentMessage
                   PERFORM DisplayAndLog
               WHEN 4
                   PERFORM SearchUser
               WHEN 5
                   PERFORM LearnSkillMenu
               WHEN 9
                   MOVE "Exiting the program. Goodbye!" TO CurrentMessage
                   PERFORM DisplayAndLog
               WHEN OTHER
                   MOVE "Invalid option. Please try again." TO CurrentMessage
                   PERFORM DisplayAndLog
           END-EVALUATE.

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
           PERFORM ReadMenuOption
           EVALUATE MenuOption
               WHEN 1 THRU 5
                   MOVE "This skill is under construction." TO CurrentMessage
                   PERFORM DisplayAndLog
               WHEN 6
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid option. Please try again." TO CurrentMessage
                   PERFORM DisplayAndLog
           END-EVALUATE.

       CreateEditProfile.
           MOVE "--- Create/Edit Profile ---" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE 'N' TO EditProfile
           MOVE 'N' TO EOF-ProfileData

           OPEN INPUT UserProfileRecordFile
           IF ProfileFileStatus NOT = "00" THEN
               CLOSE UserProfileRecordFile
               OPEN OUTPUT UserProfileRecordFile
               CLOSE UserProfileRecordFile
           ELSE
               PERFORM UNTIL EditProfile = 'Y' OR EOF-ProfileData = 'Y'
                   READ UserProfileRecordFile INTO UserProfileRecord
                   AT END
                       MOVE 'Y' TO EOF-ProfileData
                   NOT AT END
                       IF FUNCTION TRIM(ProfileUsername) =
                           FUNCTION TRIM(CurrentUsername)
                       THEN
                           MOVE "Found your username" TO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE 'Y' TO EditProfile
                           MOVE FUNCTION TRIM(ProfileFirstName)
                               TO CurrentFirstName
                           MOVE FUNCTION TRIM(ProfileLastName)
                               TO CurrentLastName
                           MOVE FUNCTION TRIM(ProfileUniversity)
                               TO CurrentUniversity
                           MOVE FUNCTION TRIM(ProfileMajor) TO CurrentMajor
                           MOVE ProfileGraduationYear TO CurrentGraduationYear
                           MOVE FUNCTION TRIM(ProfileAboutMe) TO CurrentAboutMe
                           MOVE FUNCTION TRIM(ProfileExperienceTitles)
                               TO ExperienceTitles
                           MOVE FUNCTION TRIM(ProfileExperienceCompanies)
                               TO ExperienceCompanies
                           MOVE FUNCTION TRIM(ProfileExperienceDates)
                               TO ExperienceDates
                           MOVE FUNCTION TRIM(ProfileExperienceDescriptions)
                               TO ExperienceDescriptions
                           MOVE FUNCTION TRIM(ProfileEducationDegrees)
                               TO EducationDegrees
                           MOVE FUNCTION TRIM(ProfileEducationUniversities)
                               TO EducationUniversities
                           MOVE FUNCTION TRIM(ProfileEducationYears)
                               TO EducationYears
                       END-IF
                   END-READ
               END-PERFORM
               CLOSE UserProfileRecordFile
           END-IF

           MOVE "Enter First Name:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadFirstName

           MOVE "Enter Last Name:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadLastName

           MOVE "Enter University/College Attended:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadUniversity

           MOVE "Enter Major:" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadMajor

           MOVE "Enter Graduation Year (YYYY):" TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadGradYear

           MOVE "Enter About Me (optional, max 200 characters, enter blank line to skip):"
               TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadAboutMe

           MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):"
               TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE SPACES TO ExperienceTitles
           MOVE SPACES TO ExperienceCompanies
           MOVE SPACES TO ExperienceDates
           MOVE SPACES TO ExperienceDescriptions
           MOVE 1 TO ExperienceCount

           PERFORM UNTIL ExperienceCount > 3
               MOVE SPACES TO CurrentMessage
               STRING "Experience " DELIMITED BY SIZE
                   FUNCTION TRIM(ExperienceCount) DELIMITED BY SIZE
                   " - Title:" DELIMITED BY SIZE
                   INTO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM ReadExperienceTitle
               IF FUNCTION TRIM(TempString) = "DONE" OR
                   FUNCTION TRIM(TempString) = SPACES
               THEN
                   EXIT PERFORM
               END-IF
               PERFORM AppendExperienceData
               ADD 1 TO ExperienceCount
           END-PERFORM

           MOVE "Add Education (Optional, 3 Entries or Enter 'DONE' to finish):"
               TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE SPACES TO EducationDegrees
           MOVE SPACES TO EducationUniversities
           MOVE SPACES TO EducationYears
           MOVE 1 TO EducationCount

           PERFORM UNTIL EducationCount > 3
               MOVE SPACES TO CurrentMessage
               STRING "Education " DELIMITED BY SIZE
                   FUNCTION TRIM(EducationCount) DELIMITED BY SIZE
                   " - Degree:" DELIMITED BY SIZE
                   INTO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM ReadEducationDegree
               IF FUNCTION TRIM(TempString) = "DONE" OR
                   FUNCTION TRIM(TempString) = SPACES
               THEN
                   EXIT PERFORM
               END-IF
               PERFORM AppendEducationData
               ADD 1 TO EducationCount
           END-PERFORM

           MOVE "Profile saved successfully." TO CurrentMessage
           PERFORM DisplayAndLog

           PERFORM SaveProfile.

       SaveProfile.
           IF EditProfile = 'Y' THEN
               MOVE 'N' TO EOF-ProfileData
               OPEN INPUT UserProfileRecordFile
               OPEN OUTPUT TempProfileFile
               PERFORM UNTIL EOF-ProfileData = 'Y'
                   READ UserProfileRecordFile INTO UserProfileRecord
                   AT END
                       MOVE 'Y' TO EOF-ProfileData
                   NOT AT END
                       IF FUNCTION TRIM(ProfileUsername) NOT =
                           FUNCTION TRIM(CurrentUsername)
                       THEN
                           WRITE TempProfileRecord FROM UserProfileRecord
                       END-IF
                   END-READ
               END-PERFORM
               CLOSE UserProfileRecordFile
               CLOSE TempProfileFile
               MOVE 'N' TO EOF-ProfileData
               OPEN INPUT TempProfileFile
               OPEN OUTPUT UserProfileRecordFile
               PERFORM UNTIL EOF-ProfileData = 'Y'
                   READ TempProfileFile INTO TempProfileRecord
                   AT END
                       MOVE 'Y' TO EOF-ProfileData
                   NOT AT END
                       WRITE UserProfileRecord FROM TempProfileRecord
                   END-READ
               END-PERFORM
               CLOSE TempProfileFile
               MOVE SPACES TO UserProfileRecord
               MOVE FUNCTION TRIM(CurrentUsername) TO ProfileUsername
               MOVE FUNCTION TRIM(CurrentFirstName) TO ProfileFirstName
               MOVE FUNCTION TRIM(CurrentLastName) TO ProfileLastName
               MOVE FUNCTION TRIM(CurrentUniversity) TO ProfileUniversity
               MOVE FUNCTION TRIM(CurrentMajor) TO ProfileMajor
               MOVE CurrentGraduationYear TO ProfileGraduationYear
               MOVE FUNCTION TRIM(CurrentAboutMe) TO ProfileAboutMe
               MOVE ExperienceTitles TO ProfileExperienceTitles
               MOVE ExperienceCompanies TO ProfileExperienceCompanies
               MOVE ExperienceDates TO ProfileExperienceDates
               MOVE ExperienceDescriptions TO ProfileExperienceDescriptions
               MOVE EducationDegrees TO ProfileEducationDegrees
               MOVE EducationUniversities TO ProfileEducationUniversities
               MOVE EducationYears TO ProfileEducationYears
               OPEN EXTEND UserProfileRecordFile
               WRITE UserProfileRecord
               CLOSE UserProfileRecordFile
           ELSE
               MOVE SPACES TO UserProfileRecord
               MOVE FUNCTION TRIM(CurrentUsername) TO ProfileUsername
               MOVE FUNCTION TRIM(CurrentFirstName) TO ProfileFirstName
               MOVE FUNCTION TRIM(CurrentLastName) TO ProfileLastName
               MOVE FUNCTION TRIM(CurrentUniversity) TO ProfileUniversity
               MOVE FUNCTION TRIM(CurrentMajor) TO ProfileMajor
               MOVE CurrentGraduationYear TO ProfileGraduationYear
               MOVE FUNCTION TRIM(CurrentAboutMe) TO ProfileAboutMe
               MOVE ExperienceTitles TO ProfileExperienceTitles
               MOVE ExperienceCompanies TO ProfileExperienceCompanies
               MOVE ExperienceDates TO ProfileExperienceDates
               MOVE ExperienceDescriptions TO ProfileExperienceDescriptions
               MOVE EducationDegrees TO ProfileEducationDegrees
               MOVE EducationUniversities TO ProfileEducationUniversities
               MOVE EducationYears TO ProfileEducationYears
               OPEN EXTEND UserProfileRecordFile
               WRITE UserProfileRecord
               CLOSE UserProfileRecordFile
           END-IF.

       AppendExperienceData.
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF ExperienceTitles = SPACES THEN
                   MOVE TempString TO ExperienceTitles
               ELSE
                   STRING FUNCTION TRIM(ExperienceTitles)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO ExperienceTitles
               END-IF
           END-IF

           MOVE SPACES TO CurrentMessage
           STRING "Experience " DELIMITED BY SIZE
               FUNCTION TRIM(ExperienceCount) DELIMITED BY SIZE
               " - Company:" DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadExperienceCompany
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF ExperienceCompanies = SPACES THEN
                   MOVE TempString TO ExperienceCompanies
               ELSE
                   STRING FUNCTION TRIM(ExperienceCompanies)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO ExperienceCompanies
               END-IF
           END-IF

           MOVE SPACES TO CurrentMessage
           STRING "Experience " DELIMITED BY SIZE
               FUNCTION TRIM(ExperienceCount) DELIMITED BY SIZE
               " - Dates:" DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadExperienceDates
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF ExperienceDates = SPACES THEN
                   MOVE TempString TO ExperienceDates
               ELSE
                   STRING FUNCTION TRIM(ExperienceDates)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO ExperienceDates
               END-IF
           END-IF

           MOVE SPACES TO CurrentMessage
           STRING "Experience " DELIMITED BY SIZE
               FUNCTION TRIM(ExperienceCount) DELIMITED BY SIZE
               " - Description (optional 100 characters max, blank line to skip):"
               DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadExperienceDescription
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF ExperienceDescriptions = SPACES THEN
                   MOVE TempString TO ExperienceDescriptions
               ELSE
                   STRING FUNCTION TRIM(ExperienceDescriptions)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO ExperienceDescriptions
               END-IF
           END-IF.

       AppendEducationData.
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF EducationDegrees = SPACES THEN
                   MOVE TempString TO EducationDegrees
               ELSE
                   STRING FUNCTION TRIM(EducationDegrees)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO EducationDegrees
               END-IF
           END-IF

           MOVE SPACES TO CurrentMessage
           STRING "Education " DELIMITED BY SIZE
               FUNCTION TRIM(EducationCount) DELIMITED BY SIZE
               " - University/College:" DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadEducationUniversity
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF EducationUniversities = SPACES THEN
                   MOVE TempString TO EducationUniversities
               ELSE
                   STRING FUNCTION TRIM(EducationUniversities)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO EducationUniversities
               END-IF
           END-IF

           MOVE SPACES TO CurrentMessage
           STRING "Education " DELIMITED BY SIZE
               FUNCTION TRIM(EducationCount) DELIMITED BY SIZE
               " - Years Attended:" DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadEducationYears
           IF FUNCTION TRIM(TempString) NOT = SPACES THEN
               IF EducationYears = SPACES THEN
                   MOVE TempString TO EducationYears
               ELSE
                   STRING FUNCTION TRIM(EducationYears)
                       DELIMITED BY SIZE
                       " | " DELIMITED BY SIZE
                       FUNCTION TRIM(TempString) DELIMITED BY SIZE
                       INTO EducationYears
               END-IF
           END-IF.

       ViewProfile.
           MOVE 'N' TO EOF-ProfileData
           MOVE 'N' TO ProfileFound
           OPEN INPUT UserProfileRecordFile
           IF ProfileFileStatus NOT = "00" THEN
               CLOSE UserProfileRecordFile
               MOVE "No profile found. Please create a profile first."
                   TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               PERFORM UNTIL ProfileFound = 'Y' OR EOF-ProfileData = 'Y'
                   READ UserProfileRecordFile INTO UserProfileRecord
                   AT END
                       MOVE 'Y' TO EOF-ProfileData
                   NOT AT END
                       IF FUNCTION TRIM(ProfileUsername) =
                           FUNCTION TRIM(CurrentUsername)
                       THEN
                           MOVE 'Y' TO ProfileFound
                           PERFORM DisplayProfileData
                       END-IF
                   END-READ
               END-PERFORM
               CLOSE UserProfileRecordFile
               IF ProfileFound = 'N' THEN
                   MOVE "No profile found. Please create a profile first."
                       TO CurrentMessage
                   PERFORM DisplayAndLog
               END-IF
           END-IF.

       DisplayProfileData.
           MOVE "--- Your Profile ---" TO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Name: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileFirstName) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileLastName) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "University: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileUniversity) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Major: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileMajor) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Graduation Year: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileGraduationYear) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "About Me: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileAboutMe) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE "Experience:" TO CurrentMessage
           PERFORM DisplayAndLog

           IF FUNCTION TRIM(ProfileExperienceTitles) NOT = SPACES THEN
               PERFORM DisplayExperienceFields
           ELSE
               MOVE "None" TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF

           MOVE "Education:" TO CurrentMessage
           PERFORM DisplayAndLog

           IF FUNCTION TRIM(ProfileEducationDegrees) NOT = SPACES THEN
               PERFORM DisplayEducationFields
           ELSE
               MOVE "None" TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF.

       DisplayExperienceFields.
           MOVE SPACES TO CurrentMessage
           STRING "Title: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileExperienceTitles)
               DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Company: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileExperienceCompanies)
               DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Dates: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileExperienceDates) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Description: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileExperienceDescriptions)
               DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog.

       DisplayEducationFields.
           MOVE SPACES TO CurrentMessage
           STRING "Degree: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileEducationDegrees) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "University: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileEducationUniversities)
               DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog

           MOVE SPACES TO CurrentMessage
           STRING "Years: " DELIMITED BY SIZE
               FUNCTION TRIM(ProfileEducationYears) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog.

       SearchUser.
           MOVE "Enter the full name of the person you are looking for:"
               TO CurrentMessage
           PERFORM DisplayAndLog
           PERFORM ReadSearchQuery

           MOVE 'N' TO SearchFound
           MOVE 'N' TO EOF-ProfileData
           OPEN INPUT UserProfileRecordFile
           PERFORM UNTIL SearchFound = 'Y' OR EOF-ProfileData = 'Y'
               READ UserProfileRecordFile INTO UserProfileRecord
               AT END
                   MOVE 'Y' TO EOF-ProfileData
               NOT AT END
                   MOVE SPACES TO FoundName
                   STRING FUNCTION TRIM(ProfileFirstName)
                       DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       FUNCTION TRIM(ProfileLastName)
                       DELIMITED BY SIZE
                       INTO FoundName
                   IF FUNCTION TRIM(FUNCTION UPPER-CASE(FoundName)) =
                       FUNCTION TRIM(FUNCTION UPPER-CASE(SearchQuery))
                   THEN
                       MOVE 'Y' TO SearchFound
                       MOVE "--- Found User Profile ---" TO CurrentMessage
                       PERFORM DisplayAndLog
                       PERFORM DisplayProfileData
                   END-IF
               END-READ
           END-PERFORM
           CLOSE UserProfileRecordFile

           IF SearchFound = 'N' THEN
               MOVE "No one by that name could be found." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF.

       ReadMenuOption.
           MOVE 0 TO MenuOption
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE 9 TO MenuOption
           NOT AT END
               MOVE InputRecord(1:1) TO InputChar
               EVALUATE InputChar
                   WHEN "1"
                       MOVE 1 TO MenuOption
                   WHEN "2"
                       MOVE 2 TO MenuOption
                   WHEN "3"
                       MOVE 3 TO MenuOption
                   WHEN "4"
                       MOVE 4 TO MenuOption
                   WHEN "5"
                       MOVE 5 TO MenuOption
                   WHEN "6"
                       MOVE 6 TO MenuOption
                   WHEN "9"
                       MOVE 9 TO MenuOption
                   WHEN OTHER
                       MOVE 0 TO MenuOption
               END-EVALUATE
           END-READ.

       ReadUsername.
           MOVE SPACES TO CurrentUsername
           MOVE SPACES TO TempString
           IF EOF-InputFile = 'Y'
               EXIT PARAGRAPH
           END-IF
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentUsername
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:20)) TO CurrentUsername
               IF FUNCTION TRIM(CurrentUsername) = SPACES THEN
                   MOVE "Warning: No username provided." TO CurrentMessage
                   PERFORM DisplayAndLog
               END-IF
           END-READ.

       ReadPassword.
           MOVE SPACES TO CurrentPassword
           IF EOF-InputFile = 'Y'
               EXIT PARAGRAPH
           END-IF
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentPassword
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:20)) TO CurrentPassword
               IF FUNCTION TRIM(CurrentPassword) = SPACES THEN
                   MOVE "Warning: No password provided." TO CurrentMessage
                   PERFORM DisplayAndLog
               END-IF
           END-READ.

       CheckPasswordLength.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(CurrentPassword))
               TO PasswordLength
           MOVE 'Y' TO PasswordValid
           IF PasswordLength < 8 THEN
               MOVE "Password must be at least 8 characters."
                   TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF
           IF PasswordLength > 12 THEN
               MOVE "Password must be 12 characters or less. Program terminated."
                   TO CurrentMessage
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
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > PasswordLength
               MOVE CurrentPassword(I:1) TO TempChar
               IF TempChar >= 'A' AND TempChar <= 'Z'
               THEN
                   MOVE 'Y' TO has_capital
               END-IF
               IF TempChar >= '0' AND TempChar <= '9'
               THEN
                   MOVE 'Y' TO has_digit
               END-IF
               IF NOT (TempChar >= 'A' AND TempChar <= 'Z' OR
                   TempChar >= 'a' AND TempChar <= 'z' OR
                   TempChar >= '0' AND TempChar <= '9')
               THEN
                   MOVE 'Y' TO has_special
               END-IF
           END-PERFORM
           IF has_capital = 'N' THEN
               MOVE "Password must contain at least one capital letter."
                   TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF
           IF has_digit = 'N' THEN
               MOVE "Password must contain at least one digit."
                   TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF
           IF has_special = 'N' THEN
               MOVE "Password must contain at least one special character."
                   TO CurrentMessage
               PERFORM DisplayAndLog
               MOVE 'N' TO PasswordValid
           END-IF.

       ReadFirstName.
           MOVE SPACES TO CurrentFirstName
           MOVE SPACES TO TempString
           IF EOF-InputFile = 'Y'
               EXIT PARAGRAPH
           END-IF
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentFirstName
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:20)) TO CurrentFirstName
               IF FUNCTION TRIM(CurrentFirstName) = SPACES THEN
                   MOVE "First name missing; set to N/A." TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "N/A" TO CurrentFirstName
               END-IF
               MOVE SPACES TO CurrentMessage
               MOVE CurrentFirstName TO CurrentMessage
               PERFORM DisplayAndLog
           END-READ.

       ReadLastName.
           MOVE SPACES TO CurrentLastName
           MOVE SPACES TO TempString
           IF EOF-InputFile = 'Y'
               EXIT PARAGRAPH
           END-IF
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentLastName
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:20)) TO CurrentLastName
               IF FUNCTION TRIM(CurrentLastName) = SPACES THEN
                   MOVE "Last name missing; set to N/A." TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "N/A" TO CurrentLastName
               END-IF
               MOVE SPACES TO CurrentMessage
               MOVE CurrentLastName TO CurrentMessage
               PERFORM DisplayAndLog
           END-READ.

       ReadUniversity.
           MOVE SPACES TO CurrentUniversity
           MOVE SPACES TO TempString
           IF EOF-InputFile = 'Y'
               EXIT PARAGRAPH
           END-IF
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentUniversity
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:30)) TO CurrentUniversity
               IF FUNCTION TRIM(CurrentUniversity) = SPACES THEN
                   MOVE "University missing; set to N/A." TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "N/A" TO CurrentUniversity
               END-IF
               MOVE SPACES TO CurrentMessage
               MOVE CurrentUniversity TO CurrentMessage
               PERFORM DisplayAndLog
           END-READ.

       ReadMajor.
           MOVE SPACES TO CurrentMajor
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentMajor
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:30)) TO CurrentMajor
               IF FUNCTION TRIM(CurrentMajor) = SPACES THEN
                   MOVE "Major missing; set to N/A." TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "N/A" TO CurrentMajor
               END-IF
               MOVE SPACES TO CurrentMessage
               MOVE CurrentMajor TO CurrentMessage
               PERFORM DisplayAndLog
           END-READ.

       ReadGradYear.
           MOVE 0 TO CurrentGraduationYear
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE 0 TO CurrentGraduationYear
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:4)) TO TempString
               IF FUNCTION TRIM(TempString) IS NUMERIC THEN
                   MOVE FUNCTION NUMVAL(TempString)
                       TO CurrentGraduationYear
               ELSE
                   MOVE "Invalid Graduation Year" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE 0 TO CurrentGraduationYear
               END-IF
               MOVE SPACES TO CurrentMessage
               MOVE CurrentGraduationYear TO CurrentMessage
               PERFORM DisplayAndLog
           END-READ
           IF CurrentGraduationYear < 1900 OR
               CurrentGraduationYear > 2100
           THEN
               IF CurrentGraduationYear = 0 THEN
                   MOVE "Graduation Year not provided; set to 0."
                       TO CurrentMessage
               ELSE
                   MOVE "Graduation Year out of range; set to 0."
                       TO CurrentMessage
               END-IF
               PERFORM DisplayAndLog
               MOVE 0 TO CurrentGraduationYear
           END-IF.

       ReadAboutMe.
           MOVE SPACES TO CurrentAboutMe
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentAboutMe
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO CurrentAboutMe
           MOVE SPACES TO CurrentMessage
           MOVE CurrentAboutMe TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadExperienceTitle.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadExperienceCompany.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadExperienceDates.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadExperienceDescription.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadEducationDegree.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadEducationUniversity.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadEducationYears.
           MOVE SPACES TO TempString
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO TempString
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:200)) TO TempString
           MOVE SPACES TO CurrentMessage
           MOVE TempString TO CurrentMessage
           PERFORM DisplayAndLog
           END-READ.

       ReadSearchQuery.
           MOVE SPACES TO SearchQuery
           MOVE SPACES TO TempString
           IF EOF-InputFile = 'Y'
               EXIT PARAGRAPH
           END-IF
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO SearchQuery
           NOT AT END
               MOVE FUNCTION TRIM(InputRecord(1:40)) TO SearchQuery
               MOVE SPACES TO CurrentMessage
               MOVE SearchQuery TO CurrentMessage
               PERFORM DisplayAndLog
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
                   IF FUNCTION TRIM(Username) =
                       FUNCTION TRIM(CurrentUsername)
                   THEN
                       MOVE 'Y' TO UsernameExists
                       MOVE 'Y' TO EOF-UserData
                   END-IF
               END-READ
           END-PERFORM
           CLOSE UserDataFile.
