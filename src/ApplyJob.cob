       ApplyJob.
           MOVE 'N' TO JobFound
           MOVE 0 TO CurrentJobIndex
           MOVE 'N' TO EOF-JobFile
           OPEN INPUT JobFile
           PERFORM UNTIL EOF-JobFile = 'Y' OR JobFound = 'Y'
               READ JobFile INTO JobRecord
               AT END
                   MOVE 'Y' TO EOF-JobFile
               NOT AT END
                   ADD 1 TO CurrentJobIndex
                   IF CurrentJobIndex = SelectedJobIndex THEN
                       MOVE JobTitle IN JobRecord TO CurrentJobTitle
                       MOVE JobDescription IN JobRecord TO CurrentJobDescription
                       MOVE JobEmployer IN JobRecord TO CurrentJobEmployer
                       MOVE JobLocation IN JobRecord TO CurrentJobLocation
                       MOVE JobSalary IN JobRecord TO CurrentJobSalary
                       MOVE 'Y' TO JobFound
                   END-IF
           END-PERFORM
           CLOSE JobFile

           IF JobFound = 'N' THEN
               MOVE "Unable to locate selected job." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE 0 TO JobActionOption
               PERFORM UNTIL JobActionOption = 2
                   MOVE "--- Job Details ---" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Title: " DELIMITED BY SIZE
                       FUNCTION TRIM(CurrentJobTitle) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Description: " DELIMITED BY SIZE
                       FUNCTION TRIM(CurrentJobDescription) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Employer: " DELIMITED BY SIZE
                       FUNCTION TRIM(CurrentJobEmployer) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Location: " DELIMITED BY SIZE
                       FUNCTION TRIM(CurrentJobLocation) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE SPACES TO CurrentMessage
                   STRING "Salary: " DELIMITED BY SIZE
                       FUNCTION TRIM(CurrentJobSalary) DELIMITED BY SIZE
                       INTO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "------------------" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "1. Apply for this Job" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "2. Back to Job List" TO CurrentMessage
                   PERFORM DisplayAndLog
                   MOVE "Enter your choice:" TO CurrentMessage
                   PERFORM DisplayAndLog

                   PERFORM ReadMenuOption
                   MOVE MenuOption TO JobActionOption
                   EVALUATE JobActionOption
                       WHEN 1
                           PERFORM RecordJobApplication
                           MOVE 2 TO JobActionOption
                       WHEN 2
                           CONTINUE
                       WHEN OTHER
                           MOVE "Invalid option. Please try again." TO CurrentMessage
                           PERFORM DisplayAndLog
                           MOVE 0 TO JobActionOption
                   END-EVALUATE
               END-PERFORM
           END-IF.

       RecordJobApplication.
           MOVE 'N' TO AlreadyApplied
           MOVE 'N' TO EOF-ApplicationFile
           OPEN INPUT ApplicationFile
           PERFORM UNTIL EOF-ApplicationFile = 'Y' OR AlreadyApplied = 'Y'
               READ ApplicationFile INTO ApplicationRecord
               AT END
                   MOVE 'Y' TO EOF-ApplicationFile
               NOT AT END
                   IF FUNCTION TRIM(AppUsername IN ApplicationRecord) = FUNCTION TRIM(CurrentUsername)
                       AND FUNCTION TRIM(AppJobTitle IN ApplicationRecord) = FUNCTION TRIM(CurrentJobTitle)
                       AND FUNCTION TRIM(AppJobEmployer IN ApplicationRecord) = FUNCTION TRIM(CurrentJobEmployer)
                       AND FUNCTION TRIM(AppJobLocation IN ApplicationRecord) = FUNCTION TRIM(CurrentJobLocation) THEN
                       MOVE 'Y' TO AlreadyApplied
                   END-IF
           END-PERFORM
           CLOSE ApplicationFile

           IF AlreadyApplied = 'Y' THEN
               MOVE "You have already applied to this job." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               OPEN EXTEND ApplicationFile
               MOVE SPACES TO ApplicationRecord
               MOVE CurrentUsername TO AppUsername IN ApplicationRecord
               MOVE CurrentJobTitle TO AppJobTitle IN ApplicationRecord
               MOVE CurrentJobEmployer TO AppJobEmployer IN ApplicationRecord
               MOVE CurrentJobLocation TO AppJobLocation IN ApplicationRecord
               WRITE ApplicationRecord
               CLOSE ApplicationFile

               MOVE SPACES TO CurrentMessage
               STRING "Your application for " DELIMITED BY SIZE
                   FUNCTION TRIM(CurrentJobTitle) DELIMITED BY SIZE
                   " at " DELIMITED BY SIZE
                   FUNCTION TRIM(CurrentJobEmployer) DELIMITED BY SIZE
                   " has been submitted." DELIMITED BY SIZE
                   INTO CurrentMessage
               PERFORM DisplayAndLog
           END-IF.
