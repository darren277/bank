       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-TRANSACTION.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER     PIC X(10).
       01  WS-TRANS-TYPE         PIC X(1).
       01  WS-AMOUNT             PIC 9(15)V99.
       01  WS-SQL-COMMAND        PIC X(500).
       01  WS-SHELL-COMMAND      PIC X(600).
       01  WS-RETURN-CODE        PIC S9(4) COMP.
       01  WS-DOUBLE-QUOTE    PIC X(1) VALUE '"'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Account Number:"
           ACCEPT WS-ACCOUNT-NUMBER

           DISPLAY "Enter Transaction Type (D=Deposit, W=Withdrawal):"
           ACCEPT WS-TRANS-TYPE

           DISPLAY "Enter Transaction Amount:"
           ACCEPT WS-AMOUNT

           *> Construct the SQL command
           STRING 
               "INSERT INTO transactions (account_number, transaction_type, amount) "
               "VALUES ('" DELIMITED BY SIZE
               WS-ACCOUNT-NUMBER DELIMITED BY SIZE
               "', '" DELIMITED BY SIZE
               WS-TRANS-TYPE DELIMITED BY SIZE
               "', " DELIMITED BY SIZE
               WS-AMOUNT DELIMITED BY SIZE
               ");" DELIMITED BY SIZE
           INTO WS-SQL-COMMAND
           END-STRING.

           *> Construct the shell command to execute psql
           STRING "PGPASSWORD=mypassword psql -U myusername -d bank -c " 
                  WS-DOUBLE-QUOTE
                  WS-SQL-COMMAND
                  WS-DOUBLE-QUOTE
              INTO WS-SHELL-COMMAND
              END-STRING.

           DISPLAY "Executing: " WS-SHELL-COMMAND.

           *> Execute the shell command
           CALL "SYSTEM" USING WS-SHELL-COMMAND
               RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               DISPLAY "Transaction recorded successfully."
           ELSE
               DISPLAY "Error recording transaction. Return code: " WS-RETURN-CODE
           END-IF.

           GOBACK.
