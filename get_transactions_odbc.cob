       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-TRANSACTIONS-ODBC.
       AUTHOR. DARREN-MACKENZIE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER     PIC X(10).
       01  WS-ACCOUNT-NUMBER-Z   PIC X(11) VALUE LOW-VALUES.
       01  WS-DEBUG-BUFFER      PIC X(20).
       01  WS-OUTPUT             PIC X(1024) VALUE SPACES.
       01  WS-RETURN-CODE        PIC S9(4) COMP.

       PROCEDURE DIVISION.
       MAIN-PARA.
           INITIALIZE WS-OUTPUT.

           DISPLAY "Enter Account Number to retrieve transactions:"
           ACCEPT WS-ACCOUNT-NUMBER

           MOVE WS-ACCOUNT-NUMBER TO WS-ACCOUNT-NUMBER-Z
           MOVE LOW-VALUE TO WS-ACCOUNT-NUMBER-Z(FUNCTION LENGTH(WS-ACCOUNT-NUMBER) + 1:1)
           DISPLAY "Debug: Null-Terminated Account Number = " WS-ACCOUNT-NUMBER-Z.
           DISPLAY "Debug: Size of Account Number (Z) = " FUNCTION LENGTH(WS-ACCOUNT-NUMBER-Z).
           DISPLAY "Debug: Account Number = " WS-ACCOUNT-NUMBER.
           DISPLAY "Debug: Before CALL Account Number = " WS-ACCOUNT-NUMBER-Z.

           MOVE ALL SPACES TO WS-OUTPUT

           *> Call the C function to get transactions
           CALL "get_transactions" USING
               BY REFERENCE WS-ACCOUNT-NUMBER-Z    *> char*
               BY REFERENCE WS-OUTPUT              *> char*
               BY VALUE 1024                       *> int
               BY REFERENCE WS-RETURN-CODE.

           DISPLAY "Return code: " WS-RETURN-CODE

           IF WS-RETURN-CODE = 0
               DISPLAY "Transaction History:"
               DISPLAY "Output length: " FUNCTION LENGTH(WS-OUTPUT)
               DISPLAY FUNCTION TRIM(WS-OUTPUT)
           ELSE
               DISPLAY "Error retrieving transactions. Return code: " WS-RETURN-CODE
           END-IF.

           DISPLAY "Debug: Output Buffer = " WS-OUTPUT.

           GOBACK.
