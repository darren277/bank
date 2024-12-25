       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-TRANSACTION-ODBC.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER     PIC X(10).
       01  WS-TRANS-TYPE         PIC X(1).
       01  WS-AMOUNT             PIC 9(7)V99 COMP-3.
       01  WS-AMOUNT-TEXT        PIC X(20).
       01  WS-RETURN-CODE        PIC S9(4) COMP.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Account Number:"
           ACCEPT WS-ACCOUNT-NUMBER

           DISPLAY "Enter Transaction Type (D=Deposit, W=Withdrawal):"
           ACCEPT WS-TRANS-TYPE

           DISPLAY "Enter Transaction Amount:"
           ACCEPT WS-AMOUNT-TEXT
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-AMOUNT-TEXT).

           DISPLAY "Debug: Account Number = " WS-ACCOUNT-NUMBER
           DISPLAY "Debug: Amount = " WS-AMOUNT.

           *> Call the C function to insert transaction
           CALL "insert_transaction" 
               USING BY REFERENCE WS-ACCOUNT-NUMBER
                     BY REFERENCE WS-TRANS-TYPE
                     BY REFERENCE WS-AMOUNT
               RETURNING WS-RETURN-CODE

           IF WS-RETURN-CODE = 0
               DISPLAY "Transaction recorded successfully."
           ELSE
               DISPLAY "Error recording transaction. Return code: " WS-RETURN-CODE
           END-IF.

           GOBACK.
