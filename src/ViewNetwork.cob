       ViewMyNetwork.
           MOVE 'N' TO ConnectionFound
           MOVE 'N' TO EOF-ConnectionFile
           MOVE 0 TO RequestIndex
           MOVE "--- Your Network ---" TO CurrentMessage
           PERFORM DisplayAndLog
           OPEN INPUT ConnectionRequestFile
           PERFORM UNTIL EOF-ConnectionFile = 'Y'
               READ ConnectionRequestFile INTO ConnectionRecord
               AT END
                   MOVE 'Y' TO EOF-ConnectionFile
               NOT AT END
                   IF ConnectionStatus IN ConnectionRecord = "Connected"
                   THEN
                       IF FromUsername IN ConnectionRecord = CurrentUsername
                       THEN
                           MOVE 'Y' TO ConnectionFound
                           ADD 1 TO RequestIndex
                           MOVE SPACES TO CurrentMessage
                           STRING RequestIndex DELIMITED BY SIZE
                               ". " DELIMITED BY SIZE
                               FUNCTION TRIM(ToUsername IN ConnectionRecord) 
                               DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                       END-IF
                       IF ToUsername IN ConnectionRecord = CurrentUsername
                       THEN
                           MOVE 'Y' TO ConnectionFound
                           ADD 1 TO RequestIndex
                           MOVE SPACES TO CurrentMessage
                           STRING RequestIndex DELIMITED BY SIZE
                               ". " DELIMITED BY SIZE
                               FUNCTION TRIM(FromUsername IN ConnectionRecord) 
                               DELIMITED BY SIZE
                               INTO CurrentMessage
                           PERFORM DisplayAndLog
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE ConnectionRequestFile

           IF ConnectionFound = 'N' THEN
               MOVE "You have no connections yet." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF
           MOVE "---------------------" TO CurrentMessage
           PERFORM DisplayAndLog.

