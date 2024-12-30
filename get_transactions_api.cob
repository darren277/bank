       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-TRANSACTIONS-API.
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
       01  WS-ACCOUNT-NUMBER      PIC X(10).
       01  WS-SQL-COMMAND         PIC X(500).
       01  WS-SHELL-COMMAND       PIC X(600).
       01  WS-RETURN-CODE         PIC S9(4) COMP.
       01  WS-OUTPUT              PIC X(1024).
       01  WS-END-OF-FILE         PIC X VALUE 'N'.
       01  WS-PROCESS-OUTPUT-RECORD PIC X(1024).
       01  CRLF                   PIC X(2) VALUE X"0D0A".
       01  WS-DOUBLE-QUOTE        PIC X(1) VALUE '"'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM GET-ENVIRONMENT-PARA
           IF WS-REQUEST-METHOD = "GET"
               PERFORM HANDLE-GET-PARA
           ELSE
               PERFORM SEND-ERROR-PARA "Unsupported HTTP Method."
           END-IF
           GOBACK.

       GET-ENVIRONMENT-PARA.
           ACCEPT WS-REQUEST-METHOD FROM ENVIRONMENT "REQUEST_METHOD".
           ACCEPT WS-QUERY-STRING FROM ENVIRONMENT "QUERY_STRING".

       HANDLE-GET-PARA.
           *> Example:
           *> /cgi-bin/get_transactions_api.cgi?account=1234567890
           PERFORM PARSE-QUERY-STRING-PARA
           PERFORM RETRIEVE-TRANSACTIONS-PARA
           PERFORM SEND-JSON-RESPONSE-PARA.

       PARSE-QUERY-STRING-PARA.
           *> Simple parser: assumes query string format is account=AAAA
           UNSTRING WS-QUERY-STRING DELIMITED BY "=" INTO
               WS-ACCOUNT-NUMBER
           WITH POINTER
               8.  *> Skip "account="

       RETRIEVE-TRANSACTIONS-PARA.
           *> Construct the SQL command
           STRING
               "SELECT transaction_id, transaction_type, "
               "amount, timestamp "
               "FROM transactions WHERE account_number = '"
               WS-ACCOUNT-NUMBER "';"
               INTO WS-SQL-COMMAND.

           *> Construct the shell command
           STRING
               "psql -d banking_db -c "
               WS-DOUBLE-QUOTE WS-SQL-COMMAND WS-DOUBLE-QUOTE " -t -A"
               INTO WS-SHELL-COMMAND.

           DISPLAY "Executing: " WS-SHELL-COMMAND.

           *> Open a pipe to read the output of the shell command
           CALL "popen" USING WS-SHELL-COMMAND, "r"
               RETURNING WS-OUTPUT.

           IF WS-OUTPUT = NULL
               PERFORM SEND-ERROR-PARA "Error executing psql command."
               STOP RUN
           END-IF.

           *> Initialize JSON array
           STRING "["
               INTO WS-RESPONSE.

           PERFORM UNTIL WS-END-OF-FILE = "Y"
               CALL "fgets" USING
                   WS-PROCESS-OUTPUT-RECORD, 1024, WS-OUTPUT
                   RETURNING WS-PROCESS-OUTPUT-RECORD.
               IF WS-PROCESS-OUTPUT-RECORD = NULL
                   MOVE "Y" TO WS-END-OF-FILE
               ELSE
                   IF WS-RESPONSE NOT = "["
                       STRING "," INTO WS-RESPONSE.
                   END-IF
                   *> Parse the record
                   UNSTRING WS-PROCESS-OUTPUT-RECORD
                       DELIMITED BY "|" INTO
                       WS-TRANSACTION-ID
                       WS-TRANSACTION-TYPE
                       WS-AMOUNT
                       WS-TIMESTAMP
                   WITH POINTER
                       3   *> Assuming transaction_id is up to 3 digits

                   *> Construct JSON object
                   STRING
                       "{" WS-DOUBLE-QUOTE "id" WS-DOUBLE-QUOTE ":"
                       WS-DOUBLE-QUOTE WS-TRANSACTION-ID WS-DOUBLE-QUOTE
                       ", " WS-DOUBLE-QUOTE "type" WS-DOUBLE-QUOTE ": "
                       WS-DOUBLE-QUOTE WS-TRANSACTION-TYPE
                       WS-DOUBLE-QUOTE ", " WS-DOUBLE-QUOTE
                       "amount" WS-DOUBLE-QUOTE ": " WS-AMOUNT
                       ", " WS-DOUBLE-QUOTE "timestamp" WS-DOUBLE-QUOTE
                       ": " WS-DOUBLE-QUOTE WS-TIMESTAMP WS-DOUBLE-QUOTE
                       "}"
                       INTO WS-JSON-OBJECT.

                   *> Append to JSON array
                   STRING WS-RESPONSE WS-JSON-OBJECT INTO WS-RESPONSE.
               END-IF
           END-PERFORM.

           *> Close the pipe
           CALL "pclose" USING WS-OUTPUT
               RETURNING WS-RETURN-CODE.

           *> Close JSON array
           STRING "]" INTO WS-RESPONSE.

       SEND-JSON-RESPONSE-PARA.
           STRING
               "Content-Type: application/json"
               CRLF
               "Content-Length: " FUNCTION LENGTH(WS-RESPONSE)
               CRLF
               CRLF
               WS-RESPONSE
               INTO WS-RESPONSE.
           DISPLAY WS-RESPONSE.

       SEND-ERROR-PARA.
           *> Display HTTP error response
           DISPLAY "Content-Type: text/plain"
           DISPLAY CRLF
           DISPLAY "Error: " WS-RESPONSE.
           STOP RUN.

       *> Additional working storage for JSON construction
       01  WS-TRANSACTION-ID      PIC 9(5).
       01  WS-TRANSACTION-TYPE    PIC X(1).
       01  WS-AMOUNT              PIC 9(15)V99.
       01  WS-TIMESTAMP           PIC X(30).
       01  WS-JSON-OBJECT         PIC X(256).
