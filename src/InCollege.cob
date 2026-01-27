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
       01 MaxUsers PIC 9(3) VALUE 100.
       01 CurrentUsername PIC X(20).
       01 CurrentPassword PIC X(20).
       01 MenuOption PIC 9 VALUE 0.
       01 LoginSuccess PIC X VALUE 'N'.
       01 EOF-UserData PIC X VALUE 'N'.
       01 SkillOption PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MainSection.
           OPEN INPUT InputFile
           OPEN I-O UserDataFile
           PERFORM CountExistingUsers
           PERFORM DisplayWelcome
           PERFORM MainMenu UNTIL MenuOption = 9
           CLOSE InputFile
           CLOSE UserDataFile
           STOP RUN.

       WriteOutput.
           OPEN OUTPUT OutputFile
           WRITE OutputRecord
           CLOSE OutputFile.

       CountExistingUsers.
           PERFORM UNTIL EOF-UserData = 'Y'
               READ UserDataFile INTO UserRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   ADD 1 TO UserCount
           END-PERFORM.

       DisplayWelcome.
           DISPLAY "Welcome to InCollege!".

       MainMenu.
           DISPLAY "1. Register"
           DISPLAY "2. Login"
           DISPLAY "3. Learn a Skill"
           DISPLAY "9. Exit"
           ACCEPT MenuOption
           EVALUATE MenuOption
               WHEN 1
                   PERFORM RegisterUser
               WHEN 2
                   PERFORM LoginUser
               WHEN 3
                   PERFORM LearnSkill
               WHEN 9
                   DISPLAY "Exiting the program. Goodbye!"
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE.

       RegisterUser.
           IF UserCount >= MaxUsers THEN
               DISPLAY "User limit reached. Cannot register more users."
           ELSE
               DISPLAY "Enter username:"
               ACCEPT CurrentUsername
               DISPLAY "Enter password:"
               ACCEPT CurrentPassword
               MOVE CurrentUsername TO Username
               MOVE CurrentPassword TO Password
               WRITE UserRecord
               ADD 1 TO UserCount
               DISPLAY "Registration successful!"
           END-IF.

       LoginUser.
           MOVE 'N' TO LoginSuccess
           MOVE 'N' TO EOF-UserData
           DISPLAY "Enter username:"
           ACCEPT CurrentUsername
           DISPLAY "Enter password:"
           ACCEPT CurrentPassword
           OPEN INPUT UserDataFile
           PERFORM UNTIL LoginSuccess = 'Y' OR EOF-UserData = 'Y'
               READ UserDataFile INTO UserRecord
               AT END
                   MOVE 'Y' TO EOF-UserData
               NOT AT END
                   IF Username = CurrentUsername AND Password = CurrentPassword THEN
                       DISPLAY "Login successful!"
                       MOVE 'Y' TO LoginSuccess
                   END-IF
           END-PERFORM
           CLOSE UserDataFile
           IF LoginSuccess = 'N' THEN
               DISPLAY "Invalid credentials. Please try again."
           END-IF.

       LearnSkill.
           DISPLAY "Choose a skill to learn:"
           DISPLAY "1. Time Management"
           DISPLAY "2. Public Speaking"
           DISPLAY "3. Leadership"
           DISPLAY "4. Communication"
           DISPLAY "5. Technical Skills"
           ACCEPT SkillOption
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

