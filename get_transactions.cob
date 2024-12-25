       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-TRANSACTIONS.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER     PIC X(10).
       01  WS-SQL-COMMAND        PIC X(500).
       01  WS-SHELL-COMMAND      PIC X(600).
       01  WS-RETURN-CODE        PIC S9(4) COMP.
       *> 01  WS-PROCESS-OUTPUT     PIC X(1024).
       *> 01  WS-PROCESS-OUTPUT      USAGE POINTER.
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
           STRING "PGPASSWORD=mypassword psql -U myusername -d bank -c " 
                  WS-DOUBLE-QUOTE 
                  WS-SQL-COMMAND 
                  WS-DOUBLE-QUOTE
                  " -t -A"
              INTO WS-SHELL-COMMAND
              END-STRING.

           DISPLAY "Executing: " WS-SHELL-COMMAND.

           *> Open a pipe to read the output of the shell command
           CALL "popen" USING WS-SHELL-COMMAND, "r"
               RETURNING WS-PROCESS-OUTPUT.

           IF WS-PROCESS-OUTPUT = 0
               DISPLAY "Error executing psql command. Return code: " WS-RETURN-CODE
               MOVE "Y" TO WS-END-OF-FILE
               GOBACK
           END-IF.

           DISPLAY "Transaction History:"
           PERFORM UNTIL WS-END-OF-FILE = "Y"
               DISPLAY "Simulated transaction output line."
               MOVE "Y" TO WS-END-OF-FILE
           END-PERFORM.

           *> Close the pipe
           *> CALL "pclose" USING WS-PROCESS-OUTPUT
           *>    RETURNING WS-RETURN-CODE.

           *> Close the pipe (not needed for `SYSTEM` but keeping structure)
           DISPLAY "Completed reading transactions."

           GOBACK.

       END-PROGRAM.
