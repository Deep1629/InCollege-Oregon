       SendConnectionRequest.
           MOVE 'N' TO ConnectionFound
           MOVE 'N' TO EOF-ConnectionFile
           MOVE 'N' TO ConnectionConnected
           OPEN INPUT ConnectionRequestFile
           PERFORM UNTIL EOF-ConnectionFile = 'Y'
               READ ConnectionRequestFile INTO ConnectionRecord
               AT END
                   MOVE 'Y' TO EOF-ConnectionFile
               NOT AT END
                   IF FromUsername IN ConnectionRecord = CurrentUsername AND
                      ToUsername IN ConnectionRecord = SearchedUsername
                   THEN
                       IF ConnectionStatus IN ConnectionRecord = "Pending" THEN
                           MOVE 'Y' TO ConnectionFound
                       END-IF
                       IF ConnectionStatus IN ConnectionRecord = "Connected" THEN
                           MOVE 'Y' TO ConnectionConnected
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE ConnectionRequestFile

           IF ConnectionConnected = 'Y' THEN
               MOVE "You are already connected with this user." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE IF ConnectionFound = 'Y' THEN
               MOVE "You have already sent a connection request to this user." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               OPEN EXTEND ConnectionRequestFile
               MOVE CurrentUsername TO FromUsername IN ConnectionRecord
               MOVE SearchedUsername TO ToUsername IN ConnectionRecord
               MOVE "Pending" TO ConnectionStatus IN ConnectionRecord
               WRITE ConnectionRecord
               CLOSE ConnectionRequestFile
               MOVE "Connection request sent successfully." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF
           END-IF.
