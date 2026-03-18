	   BrowseJobs.
		   MOVE 'N' TO BackToBrowseMenu
		   PERFORM UNTIL BackToBrowseMenu = 'Y'
			   MOVE "--- Available Job Listings ---" TO CurrentMessage
			   PERFORM DisplayAndLog

			   MOVE 0 TO JobListCount
			   MOVE 'N' TO EOF-JobFile
			   OPEN INPUT JobFile
			   PERFORM UNTIL EOF-JobFile = 'Y'
				   READ JobFile INTO JobRecord
				   AT END
					   MOVE 'Y' TO EOF-JobFile
				   NOT AT END
					   ADD 1 TO JobListCount
					   MOVE JobListCount TO JobListCountDisplay
					   MOVE SPACES TO CurrentMessage
					   STRING FUNCTION TRIM(JobListCountDisplay) DELIMITED BY SIZE
						   ". " DELIMITED BY SIZE
						   FUNCTION TRIM(JobTitle IN JobRecord) DELIMITED BY SIZE
						   " at " DELIMITED BY SIZE
						   FUNCTION TRIM(JobEmployer IN JobRecord) DELIMITED BY SIZE
						   " (" DELIMITED BY SIZE
						   FUNCTION TRIM(JobLocation IN JobRecord) DELIMITED BY SIZE
						   ")" DELIMITED BY SIZE
						   INTO CurrentMessage
					   PERFORM DisplayAndLog
			   END-PERFORM
			   CLOSE JobFile

			   IF JobListCount = 0 THEN
				   MOVE "No jobs/internships are currently available." TO CurrentMessage
				   PERFORM DisplayAndLog
				   MOVE 'Y' TO BackToBrowseMenu
			   ELSE
				   MOVE "----------------------------" TO CurrentMessage
				   PERFORM DisplayAndLog
				   MOVE "Enter job # for details, or 0 to go back:" TO CurrentMessage
				   PERFORM DisplayAndLog
				   READ InputFile INTO InputRecord
				   AT END
					   MOVE 'Y' TO EOF-InputFile
					   MOVE 0 TO SelectedJobIndex
				   NOT AT END
					   MOVE InputRecord(1:3) TO TempString
					   MOVE FUNCTION NUMVAL-C(TempString) TO SelectedJobIndex
				   END-READ

				   IF SelectedJobIndex = 0 THEN
					   MOVE 'Y' TO BackToBrowseMenu
				   ELSE
					   IF SelectedJobIndex > 0 AND SelectedJobIndex <= JobListCount THEN
						   PERFORM ApplyJob
					   ELSE
						   MOVE "Invalid job number. Please try again." TO CurrentMessage
						   PERFORM DisplayAndLog
					   END-IF
				   END-IF
			   END-IF
		   END-PERFORM.
