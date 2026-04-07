	   MessagesMenu.
		   MOVE 'N' TO BackToMainMenu
		   PERFORM UNTIL BackToMainMenu = 'Y'
			   MOVE "--- Messages Menu ---" TO CurrentMessage
			   PERFORM DisplayAndLog
			   MOVE "1. Send a New Message" TO CurrentMessage
			   PERFORM DisplayAndLog
			   MOVE "2. View My Messages" TO CurrentMessage
			   PERFORM DisplayAndLog
			   MOVE "3. Back to Main Menu" TO CurrentMessage
			   PERFORM DisplayAndLog
			   PERFORM ReadMenuOption
			   EVALUATE MenuOption
				   WHEN 1
					   PERFORM SendNewMessage
				   WHEN 2
					   PERFORM ViewMyMessages
				   WHEN 3
					   MOVE 'Y' TO BackToMainMenu
				   WHEN OTHER
					   MOVE "Invalid option. Please try again." TO CurrentMessage
					   PERFORM DisplayAndLog
			   END-EVALUATE
		   END-PERFORM.

	   SendNewMessage.
		   MOVE "Enter recipient's username (must be a connection):" TO CurrentMessage
		   PERFORM DisplayAndLog
		   READ InputFile INTO InputRecord
		   AT END
			   MOVE 'Y' TO EOF-InputFile
			   MOVE SPACES TO RecipientUsername
		   NOT AT END
			   MOVE FUNCTION TRIM(InputRecord(1:100)) TO RecipientUsername
		   END-READ

		   MOVE 'N' TO RecipientExists
		   MOVE 'N' TO RecipientConnected

		   MOVE 'N' TO EOF-UserData
		   OPEN INPUT UserDataFile
		   PERFORM UNTIL EOF-UserData = 'Y' OR RecipientExists = 'Y'
			   READ UserDataFile INTO UserRecord
			   AT END
				   MOVE 'Y' TO EOF-UserData
			   NOT AT END
				   IF Username IN UserRecord = RecipientUsername
					   MOVE 'Y' TO RecipientExists
				   END-IF
			   END-READ
		   END-PERFORM
		   CLOSE UserDataFile

		   IF RecipientExists = 'N' THEN
			   MOVE "User not found in your network." TO CurrentMessage
			   PERFORM DisplayAndLog
		   ELSE
			   MOVE 'N' TO EOF-ConnectionFile
			   OPEN INPUT ConnectionRequestFile
			   PERFORM UNTIL EOF-ConnectionFile = 'Y' OR
							RecipientConnected = 'Y'
				   READ ConnectionRequestFile INTO ConnectionRecord
				   AT END
					   MOVE 'Y' TO EOF-ConnectionFile
				   NOT AT END
					   IF ConnectionStatus IN ConnectionRecord = "Connected"
						   IF (FromUsername IN ConnectionRecord =
							   CurrentUsername AND
							   ToUsername IN ConnectionRecord =
							   RecipientUsername) OR
							  (ToUsername IN ConnectionRecord =
							   CurrentUsername AND
							   FromUsername IN ConnectionRecord =
							   RecipientUsername)
							   MOVE 'Y' TO RecipientConnected
						   END-IF
					   END-IF
				   END-READ
			   END-PERFORM
			   CLOSE ConnectionRequestFile

			   IF RecipientConnected = 'N' THEN
				   MOVE "You can only message users you are connected with."
					   TO CurrentMessage
				   PERFORM DisplayAndLog
			   ELSE
				   MOVE "Enter your message (max 200 chars):" TO CurrentMessage
				   PERFORM DisplayAndLog
				   MOVE 'N' TO EOF-InputFile
				   READ InputFile INTO InputRecord
				   AT END
					   MOVE 'Y' TO EOF-InputFile
					   MOVE SPACES TO MessageText
				   NOT AT END
					   MOVE FUNCTION TRIM(InputRecord(1:200)) TO MessageText
				   END-READ

				   PERFORM UNTIL MessageText NOT = SPACES OR EOF-InputFile = 'Y'
					   MOVE "Message cannot be blank. Please try again:" TO CurrentMessage
					   PERFORM DisplayAndLog
					   READ InputFile INTO InputRecord
					   AT END
						   MOVE 'Y' TO EOF-InputFile
						   MOVE SPACES TO MessageText
					   NOT AT END
						   MOVE FUNCTION TRIM(InputRecord(1:200)) TO MessageText
					   END-READ
				   END-PERFORM

				   IF MessageText = SPACES THEN
					   MOVE "Message cannot be blank." TO CurrentMessage
					   PERFORM DisplayAndLog
				   ELSE
					   OPEN EXTEND MessageFile
					   MOVE CurrentUsername TO MsgSender
					   MOVE RecipientUsername TO MsgRecipient
					   MOVE MessageText TO MsgContent
					   MOVE SPACES TO FixedMessageTimestamp
					   MOVE SPACES TO FormattedMessageTimestamp
					   ACCEPT FixedMessageTimestamp
						   FROM ENVIRONMENT "INCOLLEGE_FIXED_TIMESTAMP"
					   IF FUNCTION TRIM(FixedMessageTimestamp) = SPACES
						   MOVE FUNCTION CURRENT-DATE TO CurrentDateTime
						   STRING CurrentDateTime(1:4) DELIMITED BY SIZE
							   "-" DELIMITED BY SIZE
							   CurrentDateTime(5:2) DELIMITED BY SIZE
							   "-" DELIMITED BY SIZE
							   CurrentDateTime(7:2) DELIMITED BY SIZE
							   " " DELIMITED BY SIZE
							   CurrentDateTime(9:2) DELIMITED BY SIZE
							   ":" DELIMITED BY SIZE
							   CurrentDateTime(11:2) DELIMITED BY SIZE
							   INTO FormattedMessageTimestamp
					   ELSE
						   MOVE FixedMessageTimestamp
							   TO FormattedMessageTimestamp
					   END-IF
					   MOVE FormattedMessageTimestamp TO MsgTimestamp
					   WRITE MessageRecord
					   CLOSE MessageFile

					   MOVE SPACES TO CurrentMessage
					   STRING "Message sent to " DELIMITED BY SIZE
						   FUNCTION TRIM(RecipientUsername) DELIMITED BY SIZE
						   " successfully!" DELIMITED BY SIZE
						   INTO CurrentMessage
					   PERFORM DisplayAndLog
				   END-IF
			   END-IF
		   END-IF.
