       ViewPendingRequests.
           MOVE 'N' TO PendingRequestFound
           MOVE 'N' TO EOF-ConnectionFile
           MOVE 0 TO RequestIndex
           MOVE "Your Pending Connection Requests:" TO CurrentMessage
           PERFORM DisplayAndLog
           OPEN INPUT ConnectionRequestFile
           PERFORM UNTIL EOF-ConnectionFile = 'Y'
               READ ConnectionRequestFile INTO ConnectionRecord
               AT END
                   MOVE 'Y' TO EOF-ConnectionFile
               NOT AT END
                   IF ToUsername IN ConnectionRecord = CurrentUsername AND
                      ConnectionStatus IN ConnectionRecord = "Pending"
                   THEN
                       MOVE 'Y' TO PendingRequestFound
                       ADD 1 TO RequestIndex
                       MOVE SPACES TO CurrentMessage
                       STRING FUNCTION TRIM(RequestIndex) DELIMITED BY SIZE
                           ". From: " DELIMITED BY SIZE
                           FUNCTION TRIM(FromUsername IN ConnectionRecord) DELIMITED BY SIZE
                           INTO CurrentMessage
                       PERFORM DisplayAndLog
                   END-IF
               END-READ
           END-PERFORM
           CLOSE ConnectionRequestFile

           IF PendingRequestFound = 'N' THEN
               MOVE "You have no pending connection requests." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE "Accept request? (Enter number or 0 to skip):" TO CurrentMessage
               PERFORM DisplayAndLog
               PERFORM ReadMenuOption
               IF MenuOption > 0 AND MenuOption <= RequestIndex THEN
                   PERFORM AcceptConnectionRequest
               END-IF.
