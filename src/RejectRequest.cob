       RejectConnectionRequest.
           MOVE 0 TO RequestIndex
           MOVE 'N' TO EOF-ConnectionFile
           MOVE 'N' TO RequestFound

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
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE ConnectionRequestFile

           IF RequestFound = 'Y' THEN
               PERFORM DELETE-CONNECTION-REQUEST
               MOVE "Connection request rejected." TO CurrentMessage
               PERFORM DisplayAndLog
           ELSE
               MOVE "Invalid selection." TO CurrentMessage
               PERFORM DisplayAndLog
           END-IF.

       DELETE-CONNECTION-REQUEST.
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
                       CONTINUE
                   ELSE
                       WRITE TempConnectionRecord FROM ConnectionRecord
                   END-IF
               END-READ
           END-PERFORM

           CLOSE ConnectionRequestFile
           CLOSE TempConnectionFile

           PERFORM SWAP-CONNECTION-FILES.

