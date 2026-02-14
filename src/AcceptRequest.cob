       AcceptConnectionRequest.
           MOVE 0 TO RequestIndex
           MOVE 'N' TO EOF-ConnectionFile
           MOVE 'N' TO RequestFound
           MOVE 'N' TO ConnectionAccepted

           OPEN INPUT ConnectionRequestFile
           PERFORM UNTIL EOF-ConnectionFile = 'Y' OR RequestFound = 'Y'
               READ ConnectionRequestFile INTO ConnectionRecord
               AT END
                   MOVE 'Y' TO EOF-ConnectionFile
               NOT AT END
                   IF ToUsername IN ConnectionRecord = CurrentUsername AND
                      ConnectionStatus IN ConnectionRecord = "Pending"
                   THEN
                       ADD 1 TO RequestIndex
                       IF RequestIndex = MenuOption THEN
                           MOVE CurrentUsername TO AcceptToUsername
                           MOVE FromUsername IN ConnectionRecord
                               TO AcceptFromUsername
                           MOVE 'Y' TO RequestFound
                           MOVE 'Y' TO ConnectionAccepted
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE ConnectionRequestFile

           IF ConnectionAccepted = 'Y' THEN
               PERFORM UPDATE-CONNECTION-STATUS
               MOVE "Connection accepted successfully." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE "Invalid selection." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF.

       UPDATE-CONNECTION-STATUS.
           MOVE 'N' TO EOF-ConnectionFile

           OPEN INPUT ConnectionRequestFile
           OPEN OUTPUT TempConnectionFile

           PERFORM UNTIL EOF-ConnectionFile = 'Y'
               READ ConnectionRequestFile INTO ConnectionRecord
               AT END
                   MOVE 'Y' TO EOF-ConnectionFile
               NOT AT END
                   IF FromUsername IN ConnectionRecord = AcceptFromUsername AND
                      ToUsername IN ConnectionRecord = AcceptToUsername AND
                      ConnectionStatus IN ConnectionRecord = "Pending"
                   THEN
                       MOVE "Connected" TO ConnectionStatus IN ConnectionRecord
                   END-IF
                   WRITE TempConnectionRecord FROM ConnectionRecord
               END-READ
           END-PERFORM

           CLOSE ConnectionRequestFile
           CLOSE TempConnectionFile

           PERFORM SWAP-CONNECTION-FILES.

       SWAP-CONNECTION-FILES.
           CALL "CBL_DELETE_FILE" USING "connections.dat"
           CALL "CBL_RENAME_FILE" USING "connections_temp.dat"
               "connections.dat".


