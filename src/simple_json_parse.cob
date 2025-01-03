       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-PARSE-SUB.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-ACCOUNT-KEY     PIC X(15) VALUE '"account"'.
       01  WS-TX-TYPE-KEY     PIC X(20) VALUE '"transaction_type"'.
       01  WS-AMOUNT-KEY      PIC X(10) VALUE '"amount"'.
       01  WS-SEARCH-STRING   PIC X(8192).
       01  WS-FOUND-FIELD     PIC X(200).
       01  WS-FOUND-VALUE     PIC X(200).
       01  WS-TRIM-BUFFER     PIC X(200).
       01  WS-PARSE-OK        PIC X(3)   VALUE "200".

       * Temporary counters/indexes for searching
       01  WS-POS             PIC 9(4)   VALUE ZERO.
       01  WS-LEN             PIC 9(4)   VALUE ZERO.
       01  WS-TEMP            PIC X(8192).

       LINKAGE SECTION.
       *>-----------------------------------------------------------------
       *> The caller passes in the JSON string, plus fields to receive
       *> the parsed results, and a status code or similar indicator.
       *> Adjust for your own program’s calling conventions.
       *>-----------------------------------------------------------------
       01  LS-JSON-IN.
           05  LS-JSON-STRING       PIC X(8192).
       01  LS-PARSE-STATUS         PIC X(3).
       01  LS-ACCOUNT-OUT          PIC X(30).
       01  LS-TX-TYPE-OUT          PIC X(1).
       01  LS-AMOUNT-OUT           PIC 9(7)V99.
       PROCEDURE DIVISION USING
            LS-JSON-IN
            LS-PARSE-STATUS
            LS-ACCOUNT-OUT
            LS-TX-TYPE-OUT
            LS-AMOUNT-OUT.

       MAIN-PROCEDURE.
           MOVE LS-JSON-STRING TO WS-SEARCH-STRING
           PERFORM VARYING-PARSE

           *> Attempt to parse each field
           PERFORM PARSE-FIELD-ACCOUNT
           IF WS-PARSE-OK NOT = "200" THEN
              GO TO END-PARSE
           END-IF

           PERFORM PARSE-FIELD-TXTYPE
           IF WS-PARSE-OK NOT = "200" THEN
              GO TO END-PARSE
           END-IF

           PERFORM PARSE-FIELD-AMOUNT
           IF WS-PARSE-OK NOT = "200" THEN
              GO TO END-PARSE
           END-IF

       END-PARSE.
           MOVE WS-PARSE-OK TO LS-PARSE-STATUS
           GOBACK.

       *>-----------------------------------------------------------------
       *> PARSE-FIELD-ACCOUNT
       *>-----------------------------------------------------------------
       PARSE-FIELD-ACCOUNT.
           MOVE WS-ACCOUNT-KEY TO WS-FOUND-FIELD
           PERFORM FIND-JSON-VALUE
           IF WS-PARSE-OK = "200"
              MOVE WS-FOUND-VALUE TO LS-ACCOUNT-OUT
           END-IF
           .

       *>-----------------------------------------------------------------
       *> PARSE-FIELD-TXTYPE
       *>-----------------------------------------------------------------
       PARSE-FIELD-TXTYPE.
           MOVE WS-TX-TYPE-KEY TO WS-FOUND-FIELD
           PERFORM FIND-JSON-VALUE
           IF WS-PARSE-OK = "200"
              *> Suppose transaction_type is just 1 char
              MOVE WS-FOUND-VALUE(1:1) TO LS-TX-TYPE-OUT
           END-IF
           .

       *>-----------------------------------------------------------------
       *> PARSE-FIELD-AMOUNT
       *>-----------------------------------------------------------------
       PARSE-FIELD-AMOUNT.
           MOVE WS-AMOUNT-KEY TO WS-FOUND-FIELD
           PERFORM FIND-JSON-VALUE
           IF WS-PARSE-OK = "200"
              *> Convert the found string to numeric
              IF FUNCTION NUMVAL (WS-FOUND-VALUE) = 0 AND
                 WS-FOUND-VALUE NOT = "0"
                 MOVE "400" TO WS-PARSE-OK
              ELSE
                 COMPUTE LS-AMOUNT-OUT = FUNCTION NUMVAL (WS-FOUND-VALUE)
              END-IF
           END-IF
           .

       *>-----------------------------------------------------------------
       *> FIND-JSON-VALUE
       *>-----------------------------------------------------------------
       *> 1) Look for WS-FOUND-FIELD in WS-SEARCH-STRING.
       *> 2) Then look for a colon ':'.
       *> 3) Then look for either a double-quote or digits.
       *> 4) Extract everything up to the next quote or comma or brace.
       *>-----------------------------------------------------------------
       FIND-JSON-VALUE.
           MOVE SPACES TO WS-FOUND-VALUE
           INSPECT WS-SEARCH-STRING TALLYING WS-LEN FOR CHARACTERS

           INSPECT WS-SEARCH-STRING CONVERTING LOW-VALUE THRU HIGH-VALUE
                   TO WS-SEARCH-STRING   *> (No actual conversion, but some compilers need it)

           PERFORM VARYING WS-POS FROM 1 BY 1 UNTIL WS-POS > WS-LEN
               IF WS-SEARCH-STRING(WS-POS:15) = WS-FOUND-FIELD
                  *> Found the key
                  PERFORM EXTRACT-VALUE AFTER FOUND-FIELD
                  EXIT PERFORM
               END-IF
           END-PERFORM

           *> If we never found the key, set error
           IF WS-FOUND-VALUE = SPACES
              MOVE "400" TO WS-PARSE-OK
           END-IF
           .

       *>-----------------------------------------------------------------
       *> EXTRACT-VALUE
       *>-----------------------------------------------------------------
       *> Called right after we find something like "account"
       *> This tries to find the value after the ":" and quotes.
       *>-----------------------------------------------------------------
       EXTRACT-VALUE.
           *> We assume JSON looks like: "account":"1234567890"
           *> So from the current WS-POS, skip forward to colon, then
           *> skip optional spaces and quotes, then read until next quote
           *> or some delimiter.
           *>
           *> This is naive: no error-checking for missing quotes, etc.
           *>
           DECLARE-LOCAL-VARS.
               MOVE WS-POS TO WS-LEN   *> Reuse WS-LEN as a "local offset"

           *> 1) Advance until we find the colon:
           FIND-COLON.
               IF WS-LEN > 0 AND WS-LEN < FUNCTION LENGTH(WS-SEARCH-STRING)
                  ADD 1 TO WS-LEN
                  IF WS-SEARCH-STRING(WS-LEN:1) = ":"
                     GO TO SKIP-COLON
                  END-IF
                  GO TO FIND-COLON
               ELSE
                  MOVE "400" TO WS-PARSE-OK
                  EXIT PARAGRAPH
               END-IF

           SKIP-COLON.
               *> Move forward to skip colon
               ADD 1 TO WS-LEN

           *> 2) Skip spaces and possible quote
           SKIP-SPACES-AND-QUOTE.
               PERFORM SKIP-SPACES
               IF WS-SEARCH-STRING(WS-LEN:1) = '"' 
                  ADD 1 TO WS-LEN
               END-IF

           *> 3) Extract until next quote, comma, brace, or end of string
           EXTRACT-LOOP.
               IF WS-LEN > FUNCTION LENGTH(WS-SEARCH-STRING)
                  GO TO FAIL-END
               END-IF

               IF WS-SEARCH-STRING(WS-LEN:1) = '"' 
                  GO TO SUCCESS-END
               END-IF

               IF WS-SEARCH-STRING(WS-LEN:1) = ',' OR
                  WS-SEARCH-STRING(WS-LEN:1) = '}' 
                  GO TO SUCCESS-END
               END-IF

               STRING WS-SEARCH-STRING(WS-LEN:1) DELIMITED BY SIZE
                  INTO WS-FOUND-VALUE
                  WITH POINTER WS-TEMP
               END-STRING

               ADD 1 TO WS-LEN
               GO TO EXTRACT-LOOP

           SUCCESS-END.
               EXIT PARAGRAPH

           FAIL-END.
               MOVE "400" TO WS-PARSE-OK
               EXIT PARAGRAPH

           SKIP-SPACES.
               IF WS-SEARCH-STRING(WS-LEN:1) = SPACE
                  ADD 1 TO WS-LEN
                  IF WS-LEN <= FUNCTION LENGTH(WS-SEARCH-STRING)
                     GO TO SKIP-SPACES
                  END-IF
               END-IF
               EXIT PARAGRAPH
           .
       *>-----------------------------------------------------------------


       *>-----------------------------------------------------------------
       *> VARYING-PARSE
       *> Just resets any parse-specific statuses or placeholders each time.
       *>-----------------------------------------------------------------
       VARYING-PARSE.
           MOVE "200" TO WS-PARSE-OK
           MOVE SPACES TO WS-FOUND-VALUE
           MOVE SPACES TO WS-TEMP
           MOVE 0     TO WS-POS
           MOVE 0     TO WS-LEN
           .

       END PROGRAM JSON-PARSE-SUB.
