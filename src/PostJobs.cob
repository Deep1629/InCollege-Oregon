       PostJob.
           MOVE "--- Post a Job/Internship ---" TO CurrentMessage
           PERFORM DisplayAndLog
           MOVE "Enter Job Title: " TO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentJobTitle
           NOT AT END
               MOVE InputRecord(1:50) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentJobTitle
               IF CurrentJobTitle = SPACES
                   PERFORM UNTIL CurrentJobTitle NOT = SPACES
                       MOVE "Job Title cannot be empty. Please enter a valid Job Title: " TO CurrentMessage
                       PERFORM DisplayAndLog
                       READ InputFile INTO InputRecord
                       AT END
                           MOVE 'Y' TO EOF-InputFile
                           MOVE SPACES TO CurrentJobTitle
                       NOT AT END
                           MOVE InputRecord(1:50) TO TempString
                           MOVE FUNCTION TRIM(TempString) TO CurrentJobTitle
                   END-PERFORM
           END-READ
           MOVE "Enter Description (max 200 chars): " TO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentJobDescription
           NOT AT END
               MOVE InputRecord(1:200) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentJobDescription
               IF CurrentJobDescription = SPACES OR FUNCTION LENGTH(CurrentJobDescription) > 200
                   PERFORM UNTIL CurrentJobDescription NOT = SPACES AND FUNCTION LENGTH(CurrentJobDescription) <= 200
                       IF CurrentJobDescription = SPACES
                            MOVE "Description cannot be empty. Please enter a valid Description: " TO CurrentMessage
                       ELSE
                            MOVE "Description exceeds 200 characters. Please enter a shorter Description: " TO CurrentMessage
                       END-IF
                       PERFORM DisplayAndLog
                       READ InputFile INTO InputRecord
                       AT END
                           MOVE 'Y' TO EOF-InputFile
                           MOVE SPACES TO CurrentJobDescription
                       NOT AT END
                           MOVE InputRecord(1:200) TO TempString
                           MOVE FUNCTION TRIM(TempString) TO CurrentJobDescription
                   END-PERFORM
           END-READ
           MOVE "Enter Employer Name: " TO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentJobEmployer
           NOT AT END
               MOVE InputRecord(1:50) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentJobEmployer
               IF CurrentJobEmployer = SPACES
                   PERFORM UNTIL CurrentJobEmployer NOT = SPACES
                       MOVE "Employer Name cannot be empty. Please enter a valid Employer Name: " TO CurrentMessage
                       PERFORM DisplayAndLog
                       READ InputFile INTO InputRecord
                       AT END
                           MOVE 'Y' TO EOF-InputFile
                           MOVE SPACES TO CurrentJobEmployer
                       NOT AT END
                           MOVE InputRecord(1:50) TO TempString
                           MOVE FUNCTION TRIM(TempString) TO CurrentJobEmployer
                   END-PERFORM
           END-READ
           MOVE "Enter Job Location: " TO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
               MOVE SPACES TO CurrentJobLocation
           NOT AT END
               MOVE InputRecord(1:50) TO TempString
               MOVE FUNCTION TRIM(TempString) TO CurrentJobLocation
               IF CurrentJobLocation = SPACES
                   PERFORM UNTIL CurrentJobLocation NOT = SPACES
                       MOVE "Job Location cannot be empty. Please enter a valid Job Location: " TO CurrentMessage
                       PERFORM DisplayAndLog
                       READ InputFile INTO InputRecord
                       AT END
                           MOVE 'Y' TO EOF-InputFile
                           MOVE SPACES TO CurrentJobLocation
                       NOT AT END
                           MOVE InputRecord(1:50) TO TempString
                           MOVE FUNCTION TRIM(TempString) TO CurrentJobLocation
                   END-PERFORM
           END-READ
           MOVE "Enter Job Salary (optional, enter 'NONE' to skip): " TO CurrentMessage
           PERFORM DisplayAndLog
           READ InputFile INTO InputRecord
           AT END
               MOVE 'Y' TO EOF-InputFile
           NOT AT END
               IF InputRecord = "NONE"
                   MOVE "N/A" TO CurrentJobSalary
               ELSE
                   MOVE InputRecord TO CurrentJobSalary
               END-IF
           END-READ
           IF EOF-InputFile = 'Y'
               MOVE "N/A" TO CurrentJobSalary
           END-IF
           OPEN EXTEND JobFile
           MOVE SPACES TO JobRecord
           MOVE CurrentUsername TO JobUsername IN JobRecord
           MOVE CurrentJobTitle TO JobTitle IN JobRecord
           STRING "     " DELIMITED BY SIZE
               FUNCTION TRIM(CurrentJobDescription) DELIMITED BY SIZE
               INTO JobDescription IN JobRecord
           MOVE CurrentJobEmployer TO JobEmployer IN JobRecord
           MOVE CurrentJobLocation TO JobLocation IN JobRecord
           MOVE CurrentJobSalary TO JobSalary IN JobRecord
           WRITE JobRecord
           CLOSE JobFile
           MOVE "Job posted successfully!" TO CurrentMessage
           PERFORM DisplayAndLog.
           