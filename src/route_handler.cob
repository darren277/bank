       IDENTIFICATION DIVISION.
       PROGRAM-ID. HANDLER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> ----------------------------------------------------------
       *> 1) Environment variables
       *> ----------------------------------------------------------
       01  WS-REQUEST-METHOD           PIC X(10).
       01  WS-REQUEST-PATH             PIC X(255).

       *> Content length and body buffer
       01  WS-CONTENT-LENGTH           PIC 9(7) VALUE ZERO.
       01  WS-RAW-BODY                 PIC X(8192) VALUE ALL SPACES. 
           *> Adjust size as needed.
       01  WS-BODY-LEN                 PIC 9(7)   VALUE ZERO.

       *> ----------------------------------------------------------
       *> For generating the response
       *> ----------------------------------------------------------
       01  WS-RESPONSE-OUT            PIC X(10000).
       01  WS-RESPONSE-BODY           PIC X(8000).
       01  WS-RESPONSE-BODY-LEN       PIC 9(7).

       01  CRLF                        PIC X(2)   VALUE x"0D0A".  *> Carriage Return + Line Feed

       *> ----------------------------------------------------------
       *> Temporary working
       *> ----------------------------------------------------------
       01  WS-STATUS-CODE             PIC X(3)   VALUE "200".
       01  WS-STATUS-TEXT             PIC X(32)  VALUE "OK".

       PROCEDURE DIVISION.

       *> ----------------------------------------------------------
       *> Main entry point
       *> ----------------------------------------------------------
       MAIN-LOGIC SECTION.
       MAIN-START.
           *> Read environment variables for method and path
           ACCEPT WS-REQUEST-METHOD FROM ENVIRONMENT "REQUEST_METHOD"
           ACCEPT WS-REQUEST-PATH    FROM ENVIRONMENT "PATH_INFO"
           *> Some servers might put the route in SCRIPT_NAME or
           *> REQUEST_URI—adjust as needed.

           *> Handle unknown path or method gracefully
           IF WS-REQUEST-METHOD = SPACE
              MOVE "405" TO WS-STATUS-CODE
              MOVE "Method Not Allowed" TO WS-STATUS-TEXT
              PERFORM SEND-ERROR-RESPONSE
              GOBACK
           END-IF

           IF WS-REQUEST-PATH = SPACE
              MOVE "404" TO WS-STATUS-CODE
              MOVE "Path Not Found" TO WS-STATUS-TEXT
              PERFORM SEND-ERROR-RESPONSE
              GOBACK
           END-IF

           *> Dispatch to the route handler
           PERFORM ROUTE-DISPATCH

           GOBACK
           .
       *> ----------------------------------------------------------

       *> ----------------------------------------------------------
       *> ROUTE-DISPATCH
       *> Decide based on path and method which subprogram to call
       *> ----------------------------------------------------------
       ROUTE-SECTION.
       ROUTE-DISPATCH.
           EVALUATE TRUE
             WHEN WS-REQUEST-PATH = "/api/insert_transaction"
                  EVALUATE WS-REQUEST-METHOD
                    WHEN "POST"
                      PERFORM POST-INSERT-TRANSACTION
                    WHEN OTHER
                      MOVE "405" TO WS-STATUS-CODE
                      MOVE "Method Not Allowed" TO WS-STATUS-TEXT
                      PERFORM SEND-ERROR-RESPONSE
                  END-EVALUATE

             WHEN "/api/list_transactions"
                  EVALUATE WS-REQUEST-METHOD
                    WHEN "GET"
                      PERFORM GET-LIST-TRANSACTIONS
                    WHEN OTHER
                      MOVE "405" TO WS-STATUS-CODE
                      MOVE "Method Not Allowed" TO WS-STATUS-TEXT
                      PERFORM SEND-ERROR-RESPONSE
                  END-EVALUATE

             WHEN OTHER
               MOVE "404" TO WS-STATUS-CODE
               MOVE "Path Not Found" TO WS-STATUS-TEXT
               PERFORM SEND-ERROR-RESPONSE
           END-EVALUATE
           .

       *> ----------------------------------------------------------
       *> POST-INSERT-TRANSACTION
       *> Reads JSON, calls subprogram to handle insertion
       *> ----------------------------------------------------------
       POST-INSERT-TRANSACTION.
           PERFORM READ-REQUEST-BODY
           IF WS-BODY-LEN > 0
              *> Here you would parse JSON from WS-RAW-BODY
              *> For example, call an external parser or subprogram
              CALL 'JSON-PARSE-SUB' USING WS-RAW-BODY
                                      *> Possibly more LINKAGE items
                                      *> to retrieve the parsed fields
                                   RETURNING WS-STATUS-CODE
              IF WS-STATUS-CODE NOT = "200"
                 MOVE "400" TO WS-STATUS-CODE
                 MOVE "Bad Request" TO WS-STATUS-TEXT
                 PERFORM SEND-ERROR-RESPONSE
                 EXIT SECTION
              END-IF

              *> If JSON parsing is fine, now call the COBOL routine
              *> that actually does the insertion logic:
              CALL 'INSERT-TRANSACTION-SUB' USING WS-RAW-BODY
                                            *> or actual fields extracted from JSON
                                         RETURNING WS-STATUS-CODE

              IF WS-STATUS-CODE = "200"
                 MOVE '{"status":"ok","message":"Transaction inserted"}'
                   TO WS-RESPONSE-BODY
                 PERFORM SEND-JSON-RESPONSE
              ELSE
                 MOVE "500" TO WS-STATUS-CODE
                 MOVE "Error in insertion" TO WS-STATUS-TEXT
                 PERFORM SEND-ERROR-RESPONSE
              END-IF

           ELSE
              MOVE "400" TO WS-STATUS-CODE
              MOVE "No JSON Body Found" TO WS-STATUS-TEXT
              PERFORM SEND-ERROR-RESPONSE
           END-IF
           .

       *> ----------------------------------------------------------
       *> GET-LIST-TRANSACTIONS
       *> Calls a subprogram that returns a JSON list
       *> ----------------------------------------------------------
       GET-LIST-TRANSACTIONS.
           *> Typically no request body for GET
           *> Call a subprogram to fetch a JSON array of transactions
           CALL 'LIST-TRANSACTIONS-SUB' RETURNING WS-STATUS-CODE

           IF WS-STATUS-CODE = "200"
              MOVE '{"transactions":["tx1","tx2","tx3"]}' TO WS-RESPONSE-BODY
              PERFORM SEND-JSON-RESPONSE
           ELSE
              MOVE "500" TO WS-STATUS-CODE
              MOVE "Failed to list transactions" TO WS-STATUS-TEXT
              PERFORM SEND-ERROR-RESPONSE
           END-IF
           .

       *> ----------------------------------------------------------
       *> READ-REQUEST-BODY
       *> Reads Content-Length bytes into WS-RAW-BODY
       *> ----------------------------------------------------------
       READ-REQUEST-BODY.
           ACCEPT WS-CONTENT-LENGTH FROM ENVIRONMENT "CONTENT_LENGTH"
           IF FUNCTION NUMVAL(WS-CONTENT-LENGTH) > 0
              COMPUTE WS-BODY-LEN = FUNCTION NUMVAL(WS-CONTENT-LENGTH)
              ACCEPT WS-RAW-BODY FROM CONSOLE
           ELSE
              MOVE 0 TO WS-BODY-LEN
           END-IF
           .

       *> ----------------------------------------------------------
       *> SEND-JSON-RESPONSE
       *> Returns a 200 OK with JSON body
       *> ----------------------------------------------------------
       SEND-JSON-RESPONSE.
           *> Typically we already set WS-RESPONSE-BODY
           COMPUTE WS-RESPONSE-BODY-LEN = FUNCTION LENGTH(WS-RESPONSE-BODY)

           STRING
             "Status: 200 OK"         DELIMITED BY SIZE
             CRLF
             "Content-Type: application/json" DELIMITED BY SIZE
             CRLF
             "Content-Length: "       DELIMITED BY SIZE
             FUNCTION NUMVAL-C (WS-RESPONSE-BODY-LEN)  DELIMITED BY SIZE
             CRLF
             CRLF
             WS-RESPONSE-BODY         DELIMITED BY SIZE
             INTO WS-RESPONSE-OUT
           END-STRING

           DISPLAY WS-RESPONSE-OUT
           .

       *> ----------------------------------------------------------
       *> SEND-ERROR-RESPONSE
       *> Returns an error JSON with given WS-STATUS-CODE and WS-STATUS-TEXT
       *> ----------------------------------------------------------
       SEND-ERROR-RESPONSE.
           *> Build a small JSON body with the error
           STRING
             "{\"status\":\"error\",\"code\":\""
             WS-STATUS-CODE
             "\",\"message\":\""
             WS-STATUS-TEXT
             "\"}"
             DELIMITED BY SIZE
             INTO WS-RESPONSE-BODY
           END-STRING

           COMPUTE WS-RESPONSE-BODY-LEN = FUNCTION LENGTH(WS-RESPONSE-BODY)

           *> Build full HTTP response
           STRING
             "Status: " WS-STATUS-CODE " " WS-STATUS-TEXT DELIMITED BY SIZE
             CRLF
             "Content-Type: application/json" DELIMITED BY SIZE
             CRLF
             "Content-Length: "
             FUNCTION NUMVAL-C (WS-RESPONSE-BODY-LEN)
             CRLF
             CRLF
             WS-RESPONSE-BODY
             DELIMITED BY SIZE
             INTO WS-RESPONSE-OUT
           END-STRING

           DISPLAY WS-RESPONSE-OUT
           .

       END PROGRAM HANDLER.
