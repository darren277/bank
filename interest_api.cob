       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-API.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

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
           05 WS-TEMP-PRINCIPAL   PIC X(20).
           05 WS-TEMP-RATE       PIC X(20).
           05 WS-TEMP-TIME       PIC X(20).
           05 WS-TEMP-ACCOUNT    PIC X(20).
       01  WS-INTEREST            PIC 9(15)V99.
       01  WS-SQL-COMMAND         PIC X(500).
       01  WS-SHELL-COMMAND       PIC X(600).
       01  WS-RETURN-CODE         PIC S9(4) COMP.
       01  WS-JSON-RESPONSE       PIC X(256).
       01  WS-ACCOUNT-NUMBER      PIC X(10).
       01  WS-ERROR-MESSAGE       PIC X(100).
       01  CRLF                   PIC X(2) VALUE X"0D0A".
       01  WS-DOUBLE-QUOTE        PIC X(1) VALUE '"'.

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
           PERFORM CALCULATE-INTEREST-PARA
           PERFORM RECORD-TRANSACTION-PARA
           PERFORM SEND-JSON-RESPONSE-PARA.

       HANDLE-POST-PARA.
           *> Handle POST data from standard input
           PERFORM READ-POST-DATA-PARA
           PERFORM PARSE-POST-DATA-PARA
           PERFORM CALCULATE-INTEREST-PARA
           PERFORM RECORD-TRANSACTION-PARA
           PERFORM SEND-JSON-RESPONSE-PARA.

       PARSE-QUERY-STRING-PARA.
           *> Simple parser: assumes query string format is
           *> principal=XXX&rate=YYY&time=ZZZ&account=AAAA
           UNSTRING WS-QUERY-STRING DELIMITED BY "&" INTO
               WS-TEMP-PRINCIPAL
               WS-TEMP-RATE
               WS-TEMP-TIME
               WS-TEMP-ACCOUNT.
           
           *> Extract actual values by removing prefixes
           UNSTRING WS-TEMP-PRINCIPAL DELIMITED BY "=" INTO
               WS-TEMP-PRINCIPAL
               WS-PRINCIPAL
               WITH POINTER WS-P-PRINCIPAL.
           
           UNSTRING WS-TEMP-RATE DELIMITED BY "=" INTO
               WS-TEMP-RATE
               WS-RATE
               WITH POINTER WS-P-RATE.
           
           UNSTRING WS-TEMP-TIME DELIMITED BY "=" INTO
               WS-TEMP-TIME
               WS-TIME
               WITH POINTER WS-P-TIME.
           
           UNSTRING WS-TEMP-ACCOUNT DELIMITED BY "=" INTO
               WS-TEMP-ACCOUNT
               WS-ACCOUNT-NUMBER
               WITH POINTER WS-P-ACCOUNT.
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
