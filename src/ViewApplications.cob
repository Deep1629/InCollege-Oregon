       ViewApplications.
           MOVE "--- Your Job Applications ---" TO CurrentMessage
           PERFORM DisplayAndLog

           MOVE 0 TO ApplicationCount
           MOVE 'N' TO EOF-ApplicationFile
           OPEN INPUT ApplicationFile
           PERFORM UNTIL EOF-ApplicationFile = 'Y'
               READ ApplicationFile INTO ApplicationRecord
               AT END
                   MOVE 'Y' TO EOF-ApplicationFile
               NOT AT END
                   IF FUNCTION TRIM(AppUsername IN ApplicationRecord) = FUNCTION TRIM(CurrentUsername) THEN
                       ADD 1 TO ApplicationCount
                       MOVE ApplicationCount TO ApplicationCountDisplay
                       MOVE SPACES TO CurrentMessage
                       STRING FUNCTION TRIM(ApplicationCountDisplay) DELIMITED BY SIZE
                           ". " DELIMITED BY SIZE
                           FUNCTION TRIM(AppJobTitle IN ApplicationRecord) DELIMITED BY SIZE
                           " at " DELIMITED BY SIZE
                           FUNCTION TRIM(AppJobEmployer IN ApplicationRecord) DELIMITED BY SIZE
                           " (" DELIMITED BY SIZE
                           FUNCTION TRIM(AppJobLocation IN ApplicationRecord) DELIMITED BY SIZE
                           ")" DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                   END-IF
           END-PERFORM
           CLOSE ApplicationFile

           IF ApplicationCount = 0 THEN
               MOVE "You have not applied to any jobs yet." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF

           MOVE SPACES TO CurrentMessage
           MOVE ApplicationCount TO ApplicationCountDisplay
           STRING "Total applications: " DELIMITED BY SIZE
               FUNCTION TRIM(ApplicationCountDisplay) DELIMITED BY SIZE
               INTO CurrentMessage
           PERFORM DisplayAndLog.
