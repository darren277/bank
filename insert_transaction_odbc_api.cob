       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-API.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PSQL-RESULT-FILE ASSIGN TO "./psql_result.tmp"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD  PSQL-RESULT-FILE.
           01  PSQL-RESULT-RECORD    PIC X.

       WORKING-STORAGE SECTION.
           01  WS-ENV-VAR-DEBUG-BOOL  PIC X.
           01  WS-REQUEST-METHOD      PIC X(10).
           01  WS-QUERY-STRING        PIC X(256).
           01  WS-RESPONSE            PIC X(2048).
           01  WS-AMOUNT              PIC 9(15)V99 COMP-3.
           01  WS-AMOUNT-STR          PIC X(17). *> 15 digits + decimal point + 2 decimal places
           01  WS-TALLY               PIC 9(4) COMP.
           01  WS-AMOUNT-LEN          PIC 9(4) COMP.
           01  WS-FORMATTED-AMOUNT    PIC Z(13)9.99.
           01  WS-I                   PIC 9(4) COMP.
           01  WS-TSX-TYPE            PIC X(1).
           01  WS-TEMP-AMOUNT         PIC X(80).
           01  WS-TEMP-TSX-TYPE       PIC X(80).
           01  WS-TEMP-ACCOUNT        PIC X(80).
           01  WS-TOKEN-1             PIC X(100).
           01  WS-TOKEN-2             PIC X(100).
           01  WS-TOKEN-3             PIC X(100).
           01  WS-CURRENT-TOKEN       PIC X(100).
           01  i                      PIC 9 VALUE 1.
           01  DUMMY-KEY              PIC X(50).
           01  DUMMY-VAL              PIC X(50).
           01  WS-SQL-COMMAND         PIC X(500).
           01  WS-SQL-COMMAND-CHECK   PIC X(500).
           01  WS-SHELL-COMMAND       PIC X(600).
           01  WS-RETURN-CODE         PIC S9(9) COMP-5.
           01  WS-TEMP-STRING         PIC X(256).
           01  WS-JSON-RESPONSE       PIC X(256).
           01  WS-ACCOUNT-NUMBER      PIC X(10) VALUE SPACES.
           01  WS-ERROR-MESSAGE       PIC X(100).
           01  CRLF                   PIC X(2) VALUE X"0D0A".
           01  WS-DOUBLE-QUOTE        PIC X(1) VALUE '"'.
           01  WS-ACCOUNT-EXISTS      PIC X VALUE 'N'.
           01  WS-START-POS          PIC 9(4).
           01  WS-PSQL-RESULT        PIC X.
           01  WS-LENGTH-NUM       PIC 9(5) COMP-3.
           01  WS-LENGTH-DISPLAY   PIC X(5).
           01  WS-DEBUG-MODE          PIC X VALUE 'Y'.
               88  DEBUG-ON           VALUE 'Y'.
               88  DEBUG-OFF          VALUE 'N'.

       LINKAGE SECTION.
           01  LS-ACCOUNT-NUMBER      PIC X(10).
           01  LS-TSX-TYPE            PIC X.
           01  LS-AMOUNT              PIC 9(15)V99 COMP-3.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-ENV-VAR-DEBUG-BOOL FROM ENVIRONMENT "DEBUG_MODE"
           IF WS-ENV-VAR-DEBUG-BOOL = 'Y'
               SET DEBUG-ON TO TRUE
               DISPLAY "[DEBUG] Debug mode enabled."
           ELSE
               SET DEBUG-OFF TO TRUE
           END-IF

           DISPLAY "[SPECIAL DEBUG]" WS-ENV-VAR-DEBUG-BOOL UPON SYSERR
           
           PERFORM GET-ENVIRONMENT-PARA
           IF WS-REQUEST-METHOD = "GET"
               PERFORM HANDLE-GET-PARA
           ELSE 
               IF WS-REQUEST-METHOD = "POST"
                   PERFORM HANDLE-POST-PARA
               ELSE
                   MOVE "Unsupported HTTP Method." TO WS-ERROR-MESSAGE
                   PERFORM SEND-ERROR-PARA
               END-IF
           END-IF
           GOBACK.

       GET-ENVIRONMENT-PARA.
           ACCEPT WS-REQUEST-METHOD FROM ENVIRONMENT "REQUEST_METHOD"
           ACCEPT WS-QUERY-STRING FROM ENVIRONMENT "QUERY_STRING".

       HANDLE-GET-PARA.
           *> Example: /cgi-bin/interest_api.cgi?
           *> amount=1000&transaction_type=D&account=1234567890
           PERFORM PARSE-QUERY-STRING-PARA
           *> Temporarily bypassing this: PERFORM CHECK-ACCOUNT-PARA
           *> Hardcoding `WS-ACCOUNT-EXISTS` to 'Y' for now
           MOVE 'Y' TO WS-ACCOUNT-EXISTS
           IF WS-ACCOUNT-EXISTS = 'Y'
               PERFORM RECORD-TRANSACTION-PARA
               PERFORM SEND-JSON-RESPONSE-PARA
           ELSE
               MOVE "Account does not exist." TO WS-ERROR-MESSAGE
               PERFORM SEND-ERROR-PARA
           END-IF.

       HANDLE-POST-PARA.
           *> Handle POST data from standard input
           PERFORM READ-POST-DATA-PARA
           PERFORM PARSE-POST-DATA-PARA
           *> Temporarily bypassing this: PERFORM CHECK-ACCOUNT-PARA
           *> Hardcoding `WS-ACCOUNT-EXISTS` to 'Y' for now
           MOVE 'Y' TO WS-ACCOUNT-EXISTS
           IF WS-ACCOUNT-EXISTS = 'Y'
               PERFORM RECORD-TRANSACTION-PARA
               PERFORM SEND-JSON-RESPONSE-PARA
           ELSE
               MOVE "Account does not exist." TO WS-ERROR-MESSAGE
               PERFORM SEND-ERROR-PARA
           END-IF.

       PARSE-QUERY-STRING-PARA.
           *> Simple parser: assumes query string format is
           *> amount=1000&transaction_type=D&account=1234567890
           
           *> Remove any newlines from query string
           INSPECT WS-QUERY-STRING REPLACING ALL X"0A" BY SPACE
           INSPECT WS-QUERY-STRING REPLACING ALL X"0D" BY SPACE
           
           IF DEBUG-ON DISPLAY "[DEBUG] Cleaned query string: '" WS-QUERY-STRING "'" CRLF END-IF
           
           UNSTRING WS-QUERY-STRING
               DELIMITED BY "&"
               INTO WS-TOKEN-1
                    WS-TOKEN-2
                    WS-TOKEN-3
           END-UNSTRING

           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
              EVALUATE i
                WHEN 1
                   MOVE WS-TOKEN-1 TO WS-CURRENT-TOKEN
                   PERFORM PROCESS-TOKEN-PARA
                WHEN 2
                   MOVE WS-TOKEN-2 TO WS-CURRENT-TOKEN
                   PERFORM PROCESS-TOKEN-PARA
                WHEN 3
                   MOVE WS-TOKEN-3 TO WS-CURRENT-TOKEN
                   PERFORM PROCESS-TOKEN-PARA
                WHEN OTHER
                  EXIT PERFORM  *> or do nothing
              END-EVALUATE
           END-PERFORM
           
           *> Now parse each "key=value" token separately.

           MOVE FUNCTION TRIM(FUNCTION REVERSE(
               FUNCTION TRIM(FUNCTION REVERSE(
                   FUNCTION TRIM(WS-ACCOUNT-NUMBER))))) 
               TO WS-ACCOUNT-NUMBER
           
           IF DEBUG-ON
               DISPLAY "[DEBUG] Final account number: '" WS-ACCOUNT-NUMBER "'" CRLF
               
               DISPLAY "[DEBUG] Parsed amount: " WS-AMOUNT
               DISPLAY "[DEBUG] Parsed type: " WS-TSX-TYPE
               DISPLAY "[DEBUG] Parsed account: " WS-ACCOUNT-NUMBER
           END-IF
           
           *> Strip any spaces from account number
           MOVE FUNCTION TRIM(FUNCTION REVERSE(
               FUNCTION TRIM(FUNCTION REVERSE(
                   FUNCTION TRIM(WS-ACCOUNT-NUMBER))))) 
               TO WS-ACCOUNT-NUMBER
           
           IF DEBUG-ON
               DISPLAY "[DEBUG] Final account number: '" WS-ACCOUNT-NUMBER "'" CRLF
               DISPLAY "[DEBUG] Raw query string: '" WS-QUERY-STRING "'".

       PROCESS-TOKEN-PARA.
          UNSTRING WS-CURRENT-TOKEN DELIMITED BY "="
              INTO DUMMY-KEY
                   DUMMY-VAL
          END-UNSTRING
          IF DUMMY-KEY = "amount"
               *> Initialize numeric field first
               MOVE 0 TO WS-AMOUNT
               *> Convert straight to numeric, which should ignore trailing spaces
               *> DECIMAL DIVISION NO LONGER REQUIRED: COMPUTE WS-AMOUNT = FUNCTION NUMVAL(DUMMY-VAL) / 100
               COMPUTE WS-AMOUNT = FUNCTION NUMVAL(DUMMY-VAL)
               
               IF DEBUG-ON DISPLAY "[DEBUG] Amount after NUMVAL: " WS-AMOUNT END-IF
          ELSE
              IF DUMMY-KEY = "transaction_type"
                  MOVE DUMMY-VAL TO WS-TSX-TYPE
              ELSE
                  IF DUMMY-KEY = "account"
                      MOVE DUMMY-VAL TO WS-ACCOUNT-NUMBER
                  END-IF
              END-IF
          END-IF.
       
       READ-POST-DATA-PARA.
           *> Read Content-Length to know how much to read from stdin
           ACCEPT WS-RESPONSE FROM ENVIRONMENT "CONTENT_LENGTH".
           *> Read the POST data (assuming it's in the format
           *> amount=1000&transaction_type=D&account=1234567890)
           ACCEPT WS-QUERY-STRING FROM CONSOLE.

       PARSE-POST-DATA-PARA.
           PERFORM PARSE-QUERY-STRING-PARA.
       
       *> TODO: Convert this to use a C interface...
       CHECK-ACCOUNT-PARA.
           *> Construct the SQL command
           STRING "SELECT CASE WHEN EXISTS "
                 "(SELECT 1 FROM accounts WHERE account_number = '"
                 WS-ACCOUNT-NUMBER
                 "') THEN 'Y' ELSE 'N' END;"
                 INTO WS-SQL-COMMAND-CHECK.
           
           STRING "PGPASSWORD=mypassword psql -U myusername -d bank -c "
                 WS-DOUBLE-QUOTE FUNCTION TRIM(WS-SQL-COMMAND-CHECK) WS-DOUBLE-QUOTE " -t -A > ./psql_result.tmp"
                 INTO WS-SHELL-COMMAND.
           
           IF DEBUG-ON DISPLAY "[DEBUG] Executing: " WS-SHELL-COMMAND END-IF

           CALL "SYSTEM" USING WS-SHELL-COMMAND
               RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               OPEN INPUT PSQL-RESULT-FILE
               READ PSQL-RESULT-FILE INTO WS-PSQL-RESULT
                   AT END MOVE "N" TO WS-PSQL-RESULT
               END-READ
               CLOSE PSQL-RESULT-FILE
               
               *> Debug the exact content
               IF DEBUG-ON DISPLAY "[DEBUG] Raw PSQL Result: [" WS-PSQL-RESULT "]" END-IF
               
               *> Trim any spaces and check
               MOVE FUNCTION TRIM(WS-PSQL-RESULT) TO WS-PSQL-RESULT
               
               IF DEBUG-ON DISPLAY "[DEBUG] Trimmed PSQL Result: [" WS-PSQL-RESULT "]" END-IF
               
               IF WS-PSQL-RESULT = "Y"
                   MOVE "Y" TO WS-ACCOUNT-EXISTS
               ELSE
                   MOVE "N" TO WS-ACCOUNT-EXISTS
               END-IF
               
               *> Clean up temp file
               *> STRING "rm ./psql_result.tmp" 
               *>     INTO WS-SHELL-COMMAND
               *> CALL "SYSTEM" USING WS-SHELL-COMMAND
           ELSE
               MOVE 'N' TO WS-ACCOUNT-EXISTS
           END-IF.

       RECORD-TRANSACTION-PARA.
           IF DEBUG-ON
               DISPLAY "[DEBUG] Recording transaction..."
               DISPLAY "[DEBUG] Recording transaction..."

               DISPLAY "[DEBUG] Recording transaction..."

               DISPLAY "[DEBUG] Before CALL - Amount = " WS-AMOUNT
           END-IF

           *> Insert the interest as a transaction
           *> Call the C function to insert transaction
           SET ADDRESS OF LS-ACCOUNT-NUMBER TO ADDRESS OF WS-ACCOUNT-NUMBER
           SET ADDRESS OF LS-TSX-TYPE TO ADDRESS OF WS-TSX-TYPE
           SET ADDRESS OF LS-AMOUNT TO ADDRESS OF WS-AMOUNT
           
           CALL "insert_transaction" 
               USING LS-ACCOUNT-NUMBER
                     LS-TSX-TYPE
                     LS-AMOUNT
               RETURNING WS-RETURN-CODE

           IF WS-RETURN-CODE = 0
               IF DEBUG-ON DISPLAY "[DEBUG] Transaction recorded successfully." END-IF
           ELSE
               IF DEBUG-ON DISPLAY "[DEBUG] Error recording transaction. Return code: " WS-RETURN-CODE END-IF
           END-IF.
       
       *> TODO: Update account balance...

       SEND-JSON-RESPONSE-PARA.
           IF DEBUG-ON
               DISPLAY "[DEBUG] Sending JSON response..."
               DISPLAY "[DEBUG] Amount = " WS-AMOUNT
               DISPLAY "[DEBUG] Transaction Type = " WS-TSX-TYPE
           END-IF

           *> First format the amount properly
           MOVE WS-AMOUNT TO WS-FORMATTED-AMOUNT
           
           *> Build JSON response piece by piece
           MOVE SPACES TO WS-JSON-RESPONSE
           STRING 
               '{"amount": '
               FUNCTION TRIM(WS-FORMATTED-AMOUNT)
               ', "transaction_type": "'
               WS-TSX-TYPE
               '"}'
               DELIMITED BY SIZE
               INTO WS-JSON-RESPONSE
           END-STRING

           IF DEBUG-ON DISPLAY "[DEBUG] JSON Response: " WS-JSON-RESPONSE END-IF

           COMPUTE WS-LENGTH-NUM = FUNCTION LENGTH(WS-JSON-RESPONSE).
           MOVE WS-LENGTH-NUM TO WS-LENGTH-DISPLAY.

           STRING
               "Content-Type: application/json"
               CRLF
               "Content-Length: " WS-LENGTH-DISPLAY
               CRLF
               CRLF
               WS-JSON-RESPONSE
               INTO WS-RESPONSE.
           DISPLAY WS-RESPONSE.

       SEND-ERROR-PARA.
           *> Display HTTP error response
           DISPLAY "Content-Type: application/json"
           DISPLAY CRLF
           *>DISPLAY "Error: " WS-ERROR-MESSAGE
           DISPLAY '{"error": "' WS-ERROR-MESSAGE '"}'
           STOP RUN.
