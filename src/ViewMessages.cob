	   ViewMyMessages.
		   MOVE "--- My Messages ---" TO CurrentMessage
		   PERFORM DisplayAndLog

		   MOVE 0 TO MessageCount
		   MOVE 'N' TO EOF-MessageFile
		   OPEN INPUT MessageFile
		   PERFORM UNTIL EOF-MessageFile = 'Y'
			   READ MessageFile INTO MessageRecord
			   AT END
				   MOVE 'Y' TO EOF-MessageFile
			   NOT AT END
				   IF MsgRecipient = CurrentUsername
					   ADD 1 TO MessageCount

					   MOVE SPACES TO CurrentMessage
					   STRING "Message " DELIMITED BY SIZE
						   MessageCount DELIMITED BY SIZE
						   ":" DELIMITED BY SIZE
						   INTO CurrentMessage
					   PERFORM DisplayAndLog

					   MOVE SPACES TO CurrentMessage
					   STRING "From: " DELIMITED BY SIZE
						   FUNCTION TRIM(MsgSender) DELIMITED BY SIZE
						   INTO CurrentMessage
					   PERFORM DisplayAndLog

					   MOVE FUNCTION TRIM(MsgContent(1:100)) TO CurrentMessage
					   PERFORM DisplayAndLog

					   IF FUNCTION TRIM(MsgContent(101:100)) NOT = SPACES
						   MOVE FUNCTION TRIM(MsgContent(101:100))
							   TO CurrentMessage
						   PERFORM DisplayAndLog
					   END-IF

					   MOVE "-------------------" TO CurrentMessage
					   PERFORM DisplayAndLog
				   END-IF
			   END-READ
		   END-PERFORM
		   CLOSE MessageFile

		   IF MessageCount = 0
			   MOVE "You have no messages." TO CurrentMessage
			   PERFORM DisplayAndLog
		   END-IF.
