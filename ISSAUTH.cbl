       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISSAUTH.
       AUTHOR. Gemini.
       DATE-WRITTEN. 2024-08-13.
       DATE-COMPILED.
      ******************************************************************
      * This program simulates an Issuing Financial Institution's
      * online authorization process.
      *
      * It is called by another program and receives an ISO 8583
      * financial request message (0200).
      *
      * It performs basic validation and returns an ISO 8583
      * financial response message (0210) with an approval or
      * decline code.
      *
      * LOGIC:
      * - Approves transactions for $500.00 or less.
      * - Declines transactions over $500.00 with a '51' (Not
      * Sufficient Funds) response code.
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. UNISYS.
       OBJECT-COMPUTER. UNISYS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROCESSING-FIELDS.
           05 WS-REQUEST-MTI          PIC X(4).
           05 WS-REQUEST-PAN          PIC X(19).
           05 WS-REQUEST-AMOUNT-NUM   PIC 9(10)V99.
           05 WS-AUTH-RESPONSE-CODE   PIC X(2).
           05 WS-AUTH-ID-RESPONSE     PIC X(6).

      *-- ISO 8583 Response Message Structure
       01  WS-ISO-RESPONSE.
           05 RESP-MTI                PIC X(4).
           05 RESP-PRIMARY-BITMAP     PIC X(8).
           05 RESP-DATA-ELEMENTS.
              10 RESP-DE002-PAN        PIC X(19).
              10 RESP-DE003-PROC-CODE  PIC X(6).
              10 RESP-DE004-TRAN-AMOUNT PIC X(12).
              10 RESP-DE011-STAN       PIC X(6).
              10 RESP-DE038-AUTH-ID    PIC X(6).
              10 RESP-DE039-RESP-CODE  PIC X(2).
              10 RESP-DE041-TERM-ID    PIC X(8).

       01  WS-RESPONSE-BUFFER        PIC X(2048).
       01  WS-MSG-POINTER            PIC 9(4) COMP VALUE 1.

      ******************************************************************
      * LINKAGE SECTION
      * Defines data passed from the calling program.
      ******************************************************************
       LINKAGE SECTION.
       01  LK-REQUEST-BUFFER         PIC X(2048).
       01  LK-RESPONSE-BUFFER        PIC X(2048).

      ******************************************************************
      * PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING LK-REQUEST-BUFFER, LK-RESPONSE-BUFFER.
       0000-MAIN-LOGIC.
           DISPLAY " ".
           DISPLAY "--- ISSAUTH PROGRAM STARTED (Issuer System) ---".
           DISPLAY "Received request: " LK-REQUEST-BUFFER(1:100).

           PERFORM 1000-PARSE-REQUEST.
           PERFORM 2000-APPLY-BUSINESS-RULES.
           PERFORM 3000-BUILD-RESPONSE.

           MOVE WS-RESPONSE-BUFFER TO LK-RESPONSE-BUFFER.
           DISPLAY "Sending response: " WS-RESPONSE-BUFFER(1:100).
           DISPLAY "--- ISSAUTH PROGRAM FINISHED ---".
           GOBACK.

      ******************************************************************
      * 1000-PARSE-REQUEST
      * Extracts key data from the incoming request message.
      * NOTE: This is a simplified parser assuming a fixed layout
      * from the known calling program. A real-world parser would
      * interpret the bitmap to find fields.
      ******************************************************************
       1000-PARSE-REQUEST.
           MOVE LK-REQUEST-BUFFER(1:4) TO WS-REQUEST-MTI.
           *-- Assuming MTI(4) + Bitmap(8) = 12 bytes offset
           *-- DE2 is LLVAR, so read 2-byte length first.
           *-- For this simulation, we assume a fixed length of 16.
           MOVE LK-REQUEST-BUFFER(15:19) TO WS-REQUEST-PAN.
           *-- DE4 Amount is at a fixed position for this example
           UNSTRING LK-REQUEST-BUFFER(40:12)
               INTO WS-REQUEST-AMOUNT-NUM.

      ******************************************************************
      * 2000-APPLY-BUSINESS-RULES
      * The core authorization logic.
      ******************************************************************
       2000-APPLY-BUSINESS-RULES.
           DISPLAY "Authorizing PAN " WS-REQUEST-PAN
               " for amount " WS-REQUEST-AMOUNT-NUM.

           IF WS-REQUEST-AMOUNT-NUM > 500.00
               MOVE "51" TO WS-AUTH-RESPONSE-CODE *> Not Sufficient Funds
               MOVE "DEC123" TO WS-AUTH-ID-RESPONSE
               DISPLAY "Result: DECLINED (Amount > 500.00)"
           ELSE
               MOVE "00" TO WS-AUTH-RESPONSE-CODE *> Approved
               MOVE "APP456" TO WS-AUTH-ID-RESPONSE
               DISPLAY "Result: APPROVED".

      ******************************************************************
      * 3000-BUILD-RESPONSE
      * Constructs the 0210 response message.
      ******************************************************************
       3000-BUILD-RESPONSE.
           INITIALIZE WS-RESPONSE-BUFFER, WS-ISO-RESPONSE.
           MOVE 1 TO WS-MSG-POINTER.

           MOVE "0210" TO RESP-MTI.
           *-- For simplicity, we will hardcode the bitmap
           MOVE X'F238000100800000' TO RESP-PRIMARY-BITMAP.

           *-- Copy original data from request
           MOVE LK-REQUEST-BUFFER(15:19) TO RESP-DE002-PAN.
           MOVE LK-REQUEST-BUFFER(34:6)  TO RESP-DE003-PROC-CODE.
           MOVE LK-REQUEST-BUFFER(40:12) TO RESP-DE004-TRAN-AMOUNT.
           MOVE LK-REQUEST-BUFFER(52:6)  TO RESP-DE011-STAN.
           MOVE LK-REQUEST-BUFFER(58:8)  TO RESP-DE041-TERM-ID.

           *-- Add response-specific data
           MOVE WS-AUTH-ID-RESPONSE TO RESP-DE038-AUTH-ID.
           MOVE WS-AUTH-RESPONSE-CODE TO RESP-DE039-RESP-CODE.

           *-- Assemble the message string (simplified)
           STRING
               RESP-MTI,
               RESP-PRIMARY-BITMAP,
               RESP-DE002-PAN,
               RESP-DE003-PROC-CODE,
               RESP-DE004-TRAN-AMOUNT,
               RESP-DE011-STAN,
               RESP-DE038-AUTH-ID,
               RESP-DE039-RESP-CODE,
               RESP-DE041-TERM-ID
               DELIMITED BY SIZE
               INTO WS-RESPONSE-BUFFER.
