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
       01  WS-PRINCIPAL           PIC 9(15)V99.
       01  WS-RATE                PIC 9(5)V9999.
       01  WS-TIME                PIC 9(5).
       01  WS-POINTERS.
           05 WS-P-PRINCIPAL      PIC 9(2) VALUE 10.  *> Length of "principal="
           05 WS-P-RATE          PIC 9(2) VALUE 5.   *> Length of "rate="
           05 WS-P-TIME          PIC 9(2) VALUE 5.   *> Length of "time="
           05 WS-P-ACCOUNT       PIC 9(2) VALUE 8.   *> Length of "account="
       01  WS-TEMP.
           05 WS-TEMP-PRINCIPAL   PIC X(30) VALUE SPACES.
           05 WS-TEMP-RATE        PIC X(30) VALUE SPACES.
           05 WS-TEMP-TIME        PIC X(30) VALUE SPACES.
           05 WS-TEMP-ACCOUNT     PIC X(30) VALUE SPACES.
       01  WS-INTEREST            PIC 9(15)V99.
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
           *> principal=1000&rate=0.05&time=2&account=1234567890
           PERFORM PARSE-QUERY-STRING-PARA
           PERFORM CHECK-ACCOUNT-PARA
           IF WS-ACCOUNT-EXISTS = 'Y'
               PERFORM CALCULATE-INTEREST-PARA
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
               PERFORM CALCULATE-INTEREST-PARA
               PERFORM RECORD-TRANSACTION-PARA
               PERFORM SEND-JSON-RESPONSE-PARA
           ELSE
               MOVE "Account does not exist." TO WS-ERROR-MESSAGE
               PERFORM SEND-ERROR-PARA
           END-IF.

       PARSE-QUERY-STRING-PARA.
           *> Simple parser: assumes query string format is
           *> principal=XXX&rate=YYY&time=ZZZ&account=AAAA
           INITIALIZE WS-TEMP-PRINCIPAL WS-TEMP-RATE WS-TEMP-TIME 
                      WS-TEMP-ACCOUNT WS-ACCOUNT-NUMBER
           
           *> Remove any newlines from query string
           INSPECT WS-QUERY-STRING REPLACING ALL X"0A" BY SPACE
           INSPECT WS-QUERY-STRING REPLACING ALL X"0D" BY SPACE

           DISPLAY "Cleaned query string: '" WS-QUERY-STRING "'" CRLF.
           
           UNSTRING WS-QUERY-STRING DELIMITED BY "&" INTO
               WS-TEMP-PRINCIPAL
               WS-TEMP-RATE
               WS-TEMP-TIME
               WS-TEMP-ACCOUNT.
           
           DISPLAY "Debug 1: Account parameter: '" WS-TEMP-ACCOUNT "'" CRLF

           *> Find start position after "account="
           COMPUTE WS-START-POS = FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-ACCOUNT))
           SUBTRACT FUNCTION LENGTH("account=") FROM WS-START-POS

           *> Extract the account number using reference modification
           MOVE WS-TEMP-ACCOUNT(9:10) TO WS-ACCOUNT-NUMBER
           
           DISPLAY "Debug 3: Final account: '" WS-ACCOUNT-NUMBER "'" CRLF
           
           *> Extract actual values by removing prefixes
           UNSTRING WS-TEMP-PRINCIPAL DELIMITED BY "=" INTO
               WS-TEMP-PRINCIPAL
               WS-PRINCIPAL.
           
           UNSTRING WS-TEMP-RATE DELIMITED BY "=" INTO
               WS-TEMP-RATE
               WS-RATE.
           
           UNSTRING WS-TEMP-TIME DELIMITED BY "=" INTO
               WS-TEMP-TIME
               WS-TIME.
           
           *> Strip any spaces from account number
           MOVE FUNCTION TRIM(FUNCTION REVERSE(
               FUNCTION TRIM(FUNCTION REVERSE(
                   FUNCTION TRIM(WS-ACCOUNT-NUMBER))))) 
               TO WS-ACCOUNT-NUMBER
           DISPLAY "Final account number: '" WS-ACCOUNT-NUMBER "'" CRLF
           DISPLAY "Raw query string: '" WS-QUERY-STRING "'".
       *> Skip "principal="
       *> Skip "rate="
       *> Skip "time="
       *> Skip "account="
       *> Note: This is a simplistic parser
       *> and doesn't handle URL encoding or errors.

       READ-POST-DATA-PARA.
           *> Read Content-Length to know how much to read from stdin
           ACCEPT WS-RESPONSE FROM ENVIRONMENT "CONTENT_LENGTH".
           *> Read the POST data (assuming it's in the format
           *> principal=XXX&rate=YYY&time=ZZZ&account=AAAA)
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

       CALCULATE-INTEREST-PARA.
           *> Compound interest: A = P * (1 + r)^t
           COMPUTE WS-INTEREST = WS-PRINCIPAL *
               FUNCTION EXP ( FUNCTION LOG (1.0 + WS-RATE ) * WS-TIME )
               - WS-PRINCIPAL.

       RECORD-TRANSACTION-PARA.
           *> Insert the interest as a transaction
           *> (assuming 'D' for deposit)
           STRING "INSERT INTO transactions (account_number, "
               "transaction_type, amount) "
               "VALUES ('" WS-ACCOUNT-NUMBER "', 'D', " WS-INTEREST ");"
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

       SEND-JSON-RESPONSE-PARA.
           STRING
               "{""principal"": " WS-PRINCIPAL
               ", ""rate"": " WS-RATE
               ", ""time"": " WS-TIME
               ", ""interest"": " WS-INTEREST
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
