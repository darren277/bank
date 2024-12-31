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
       01  WS-REQUEST-METHOD      PIC X(10).
       01  WS-QUERY-STRING        PIC X(256).
       01  WS-RESPONSE            PIC X(2048).
       01  WS-AMOUNT              PIC 9(15)V99 VALUE 0.
       01  WS-AMOUNT-STR          PIC X(30) VALUE SPACES.
       01  WS-TSX-TYPE            PIC X(1).
       01  WS-TEMP-AMOUNT         PIC X(80).
       01  WS-TEMP-TSX-TYPE       PIC X(80).
       01  WS-TEMP-ACCOUNT        PIC X(80).
       01  DUMMY-KEY              PIC X(50).
       01  DUMMY-VAL              PIC X(50).
       01  WS-SQL-COMMAND         PIC X(500).
       01  WS-SQL-COMMAND-CHECK   PIC X(500).
       01  WS-SHELL-COMMAND       PIC X(600).
       01  WS-RETURN-CODE         PIC S9(4) COMP.
       01  WS-JSON-RESPONSE       PIC X(256).
       01  WS-ACCOUNT-NUMBER      PIC X(10) VALUE SPACES.
       01  WS-ERROR-MESSAGE       PIC X(100).
       01  CRLF                   PIC X(2) VALUE X"0D0A".
       01  WS-DOUBLE-QUOTE        PIC X(1) VALUE '"'.
       01  WS-ACCOUNT-EXISTS      PIC X VALUE 'N'.
       01  WS-START-POS          PIC 9(4).
       01  WS-PSQL-RESULT        PIC X.

       PROCEDURE DIVISION.
       MAIN-PARA.
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
           PERFORM CHECK-ACCOUNT-PARA
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
           PERFORM CHECK-ACCOUNT-PARA
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

           DISPLAY "Cleaned query string: '" WS-QUERY-STRING "'" CRLF.
           
           UNSTRING WS-QUERY-STRING DELIMITED BY "&" INTO
                   WS-TEMP-AMOUNT
                   WS-TEMP-TSX-TYPE
                   WS-TEMP-ACCOUNT
               ON OVERFLOW
                   DISPLAY "Could not split query string by &"
           END-UNSTRING.
           
           *> Now parse each "key=value" token separately:

           UNSTRING WS-TEMP-AMOUNT DELIMITED BY "="
             INTO DUMMY-KEY
                  WS-AMOUNT-STR
           END-UNSTRING

           UNSTRING WS-TEMP-TSX-TYPE DELIMITED BY "="
             INTO DUMMY-KEY
                  WS-TSX-TYPE
           END-UNSTRING

           UNSTRING WS-TEMP-ACCOUNT DELIMITED BY "="
             INTO DUMMY-KEY
                  WS-ACCOUNT-NUMBER
           END-UNSTRING
           
           IF WS-AMOUNT-STR NUMERIC
              MOVE WS-AMOUNT-STR TO WS-AMOUNT
           ELSE
              MOVE 0 TO WS-AMOUNT
           END-IF.
           
           UNSTRING WS-TEMP-TSX-TYPE DELIMITED BY "=" INTO
                WS-TEMP-TSX-TYPE
                WS-TSX-TYPE.
           
           DISPLAY "[DEBUG] Parsed amount: " WS-AMOUNT
           DISPLAY "[DEBUG] Parsed type: " WS-TSX-TYPE
           DISPLAY "[DEBUG] Parsed account: " WS-ACCOUNT-NUMBER
           
           *> Strip any spaces from account number
           MOVE FUNCTION TRIM(FUNCTION REVERSE(
               FUNCTION TRIM(FUNCTION REVERSE(
                   FUNCTION TRIM(WS-ACCOUNT-NUMBER))))) 
               TO WS-ACCOUNT-NUMBER
           DISPLAY "Final account number: '" WS-ACCOUNT-NUMBER "'" CRLF
           DISPLAY "Raw query string: '" WS-QUERY-STRING "'".

       READ-POST-DATA-PARA.
           *> Read Content-Length to know how much to read from stdin
           ACCEPT WS-RESPONSE FROM ENVIRONMENT "CONTENT_LENGTH".
           *> Read the POST data (assuming it's in the format
           *> amount=1000&transaction_type=D&account=1234567890)
           ACCEPT WS-QUERY-STRING FROM CONSOLE.

       PARSE-POST-DATA-PARA.
           PERFORM PARSE-QUERY-STRING-PARA.
       
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
           
           DISPLAY "Executing: " WS-SHELL-COMMAND.

           CALL "SYSTEM" USING WS-SHELL-COMMAND
               RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               OPEN INPUT PSQL-RESULT-FILE
               READ PSQL-RESULT-FILE INTO WS-PSQL-RESULT
                   AT END MOVE "N" TO WS-PSQL-RESULT
               END-READ
               CLOSE PSQL-RESULT-FILE
               
               *> Debug the exact content
               DISPLAY "Raw PSQL Result: [" WS-PSQL-RESULT "]"
               
               *> Trim any spaces and check
               MOVE FUNCTION TRIM(WS-PSQL-RESULT) TO WS-PSQL-RESULT
               DISPLAY "Trimmed PSQL Result: [" WS-PSQL-RESULT "]"
               
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
           *> Insert the interest as a transaction
           *> (assuming 'D' for deposit)
           STRING "INSERT INTO transactions (account_number, "
                "transaction_type, amount) "
                *> "VALUES ('" WS-ACCOUNT-NUMBER "', 'D', " WS-INTEREST ");"
                "VALUES ('"
                WS-ACCOUNT-NUMBER
                "', "
                "'" WS-TSX-TYPE "'"
                ", "
                WS-AMOUNT
                ");"
               INTO WS-SQL-COMMAND.

           *> Construct the shell command
           STRING
               "PGPASSWORD=mypassword psql -U myusername -d bank -c "
               WS-DOUBLE-QUOTE WS-SQL-COMMAND WS-DOUBLE-QUOTE
               INTO WS-SHELL-COMMAND.

           *> Execute the shell command
           CALL "SYSTEM" USING WS-SHELL-COMMAND
               RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE NOT = 0
               MOVE "Error recording transaction." TO WS-ERROR-MESSAGE
               PERFORM SEND-ERROR-PARA
           END-IF.
       
       *> TODO: Update account balance...

       SEND-JSON-RESPONSE-PARA.
           STRING
               "{""amount"": " WS-AMOUNT
               ", ""transaction_type"": " WS-TSX-TYPE
               "}"
               INTO WS-JSON-RESPONSE.
           STRING
               "Content-Type: application/json"
               CRLF
               "Content-Length: " FUNCTION LENGTH(WS-JSON-RESPONSE)
               CRLF
               CRLF
               WS-JSON-RESPONSE
               INTO WS-RESPONSE.
           DISPLAY WS-RESPONSE.

       SEND-ERROR-PARA.
           *> Display HTTP error response
           DISPLAY "Content-Type: text/plain"
           DISPLAY CRLF
           DISPLAY "Error: " WS-ERROR-MESSAGE
           STOP RUN.
