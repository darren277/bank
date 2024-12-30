       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-TRANSACTIONS.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD  OUTPUT-FILE.
           01  OUTPUT-RECORD           PIC X(1024).

       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER     PIC X(10).
       01  WS-SQL-COMMAND        PIC X(500).
       01  WS-SHELL-COMMAND      PIC X(600).
       01  WS-RETURN-CODE        PIC S9(4) COMP.
       01  WS-PROCESS-OUTPUT        PIC S9(18) COMP.
       01  WS-PROCESS-OUTPUT-RECORD PIC X(1024).
       01  WS-END-OF-FILE        PIC X VALUE 'N'.
       01  WS-DOUBLE-QUOTE    PIC X(1) VALUE '"'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Account Number to retrieve transactions:"
           ACCEPT WS-ACCOUNT-NUMBER

           *> Construct the SQL command
           STRING "SELECT transaction_id, transaction_type, amount, timestamp "
               "FROM transactions WHERE account_number = '" WS-ACCOUNT-NUMBER "';"
               INTO WS-SQL-COMMAND.

           *> Construct the shell command to execute psql and capture output
           *> STRING "psql -d banking_db -c \"" WS-SQL-COMMAND "\" -t -A"
           STRING "PGPASSWORD=mypassword psql -U myusername -d bank -c " WS-DOUBLE-QUOTE FUNCTION TRIM(WS-SQL-COMMAND) WS-DOUBLE-QUOTE " -t -A > output.txt"
              INTO WS-SHELL-COMMAND
              END-STRING.

           DISPLAY "Executing: " WS-SHELL-COMMAND.

           *> Execute the command
           CALL "SYSTEM" USING WS-SHELL-COMMAND
               RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Error executing psql command. Return code: " WS-RETURN-CODE
               GOBACK
           END-IF.

           *> Read and display the output
           OPEN INPUT OUTPUT-FILE
           PERFORM UNTIL WS-END-OF-FILE = "Y"
               READ OUTPUT-FILE INTO OUTPUT-RECORD
                   AT END
                       MOVE "Y" TO WS-END-OF-FILE
                   NOT AT END
                       DISPLAY OUTPUT-RECORD
               END-READ
           END-PERFORM.
           CLOSE OUTPUT-FILE.

           GOBACK.

       END-PROGRAM.
