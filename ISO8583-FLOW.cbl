       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISO8583-FLOW.
       AUTHOR. Gemini.
       DATE-WRITTEN. 2024-08-13.
       DATE-COMPILED.
      ******************************************************************
	  * This program demonstrates the REQUEST/ADVICE message flow
      * characteristic of the ISO 8583 [1993] standard.
      *
      * It simulates a full transaction lifecycle:
      * 1. Authorization Request (0100) -> Issuer Response (0110)
      * - A true request that requires a synchronous approval or
      * decline from the issuer.
      *
      * 2. Financial Advice (0220) -> Acquirer Acknowledgement (0230)
      * - An advice message that notifies the issuer of a completed
      * action. It only requires an acknowledgement of receipt.
      *
      * The program internally simulates the role of both the Acquirer
      * (sending the initial message) and the Issuer (replying).
      * This program demonstrates the REQUEST/ADVICE message flow.
      *
      * UPDATE: This version now calls an external ALGOL program
      * ('ALGOL_MAC_CALC') to compute a Message Authentication Code
      * (MAC) for the transaction, which is then appended to the
      * message (simulating DE64/DE128).
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. UNISYS.
       OBJECT-COMPUTER. UNISYS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * ISO 8583 MESSAGE BUFFERS
      ******************************************************************
       01  WS-REQUEST-BUFFER         PIC X(1024).
       01  WS-RESPONSE-BUFFER        PIC X(1024).
       01  WS-FINAL-MESSAGE          PIC X(1032).
       01  WS-MSG-POINTER            PIC 9(4) COMP VALUE 1.

      ******************************************************************
      * SHARED DATA ELEMENTS AND WORK FIELDS
      ******************************************************************
       01  WS-SHARED-TRAN-DATA.
           05 WS-PAN                  PIC X(19) VALUE "4915123456789012".
           05 WS-PROC-CODE            PIC X(6)  VALUE "001000".
           05 WS-AMOUNT-NUM           PIC 9(10)V99.
           05 WS-AMOUNT-ALPHA         PIC X(12).
           05 WS-STAN                 PIC X(6)  VALUE "001122".
           05 WS-TERM-ID              PIC X(8)  VALUE "TERM0001".
           05 WS-ACQUIRER-ID          PIC X(11) VALUE "12345678901".
           05 WS-RESPONSE-CODE        PIC X(2).
           05 WS-AUTH-ID              PIC X(6).

      *----------------------------------------------------------------*
      *--- NEW: CRYPTOGRAPHIC DATA SECTION                          ---*
      *----------------------------------------------------------------*
       01  WS-CRYPTO-DATA.
           05 WS-SECRET-KEY           PIC X(8) VALUE "SECRETKY".
           05 WS-MAC-RESULT           PIC X(8).
      *----------------------------------------------------------------*

       01  WS-USER-CHOICE            PIC X(1).
       01  WS-AMOUNT-INPUT           PIC Z(9)V99.

      ******************************************************************
      * PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           PERFORM 1000-DISPLAY-MENU.
           ACCEPT WS-USER-CHOICE.

           EVALUATE WS-USER-CHOICE
               WHEN '1'
                   PERFORM 2000-PROCESS-AUTH-REQUEST-FLOW
               WHEN '2'
                   PERFORM 3000-PROCESS-FINANCIAL-ADVICE-FLOW
               WHEN OTHER
                   DISPLAY "Invalid option. Program terminating."
           END-EVALUATE.

           STOP RUN.

      ******************************************************************
      * 1000-DISPLAY-MENU SECTION
      ******************************************************************
       1000-DISPLAY-MENU.
           DISPLAY "=================================================".
           DISPLAY "      ISO 8583 FLOW w/ ALGOL MAC".
           DISPLAY "=================================================".
           DISPLAY "  1. Authorization Request (0100 -> 0110)".
           DISPLAY "  2. Financial Advice (0220 -> 0230)".
           DISPLAY " ".
           DISPLAY "Please select a flow to execute: ".


      ******************************************************************
      * 2000-PROCESS-AUTH-REQUEST-FLOW
      ******************************************************************
       2000-PROCESS-AUTH-REQUEST-FLOW.
           DISPLAY " ".
           DISPLAY "--- Initiating Authorization Request Flow ---".
           DISPLAY "Enter transaction amount (e.g., 450.00): ".
           ACCEPT WS-AMOUNT-INPUT.
           MOVE WS-AMOUNT-INPUT TO WS-AMOUNT-NUM.

           PERFORM 9000-INITIALIZE-BUFFERS.

           *-- Step 1: Acquirer builds the 0100 Authorization Request
           STRING "0100" "F220000100800000" WS-PAN WS-PROC-CODE
                  WS-AMOUNT-ALPHA WS-STAN WS-ACQUIRER-ID WS-TERM-ID
               DELIMITED BY SIZE
               INTO WS-REQUEST-BUFFER.

           DISPLAY " ".
           DISPLAY "Step 1: Acquirer builds base message (0100)".
           DISPLAY "   Message: " WS-REQUEST-BUFFER(1:80).

           *-- Step 2: Call ALGOL program to generate the MAC
           PERFORM 9100-GENERATE-MAC.

           *-- Step 3: Append MAC and display final message
           STRING WS-REQUEST-BUFFER(1:80) WS-MAC-RESULT
               DELIMITED BY SIZE INTO WS-FINAL-MESSAGE.
           DISPLAY "Step 2: Acquirer sends message with MAC".
           DISPLAY "   Final Msg: " WS-FINAL-MESSAGE(1:88).

           *-- Step 4: Simulate sending to Issuer and getting a response
           PERFORM 8000-SIMULATE-ISSUER-RESPONSE.

           *-- Step 5: Acquirer receives and displays the 0110 Response
           DISPLAY " ".
           DISPLAY "Step 3: Acquirer receives ISSUER RESPONSE (0110)".
           DISPLAY "   Message: " WS-RESPONSE-BUFFER(1:80).
           DISPLAY "--- Authorization Flow Complete ---".


      ******************************************************************
      * 3000-PROCESS-FINANCIAL-ADVICE-FLOW
      ******************************************************************
       3000-PROCESS-FINANCIAL-ADVICE-FLOW.
           DISPLAY " ".
           DISPLAY "--- Initiating Financial Advice Flow ---".
           MOVE 150.75 TO WS-AMOUNT-NUM.

           PERFORM 9000-INITIALIZE-BUFFERS.

           *-- Step 1: Acquirer builds the 0220 Financial Advice
           STRING "0220" "F220000100800000" WS-PAN WS-PROC-CODE
                  WS-AMOUNT-ALPHA WS-STAN WS-ACQUIRER-ID WS-TERM-ID
               DELIMITED BY SIZE
               INTO WS-REQUEST-BUFFER.

           DISPLAY " ".
           DISPLAY "Step 1: Acquirer builds base message (0220)".
           DISPLAY "   Message: " WS-REQUEST-BUFFER(1:80).

           *-- Step 2: Call ALGOL program to generate the MAC
           PERFORM 9100-GENERATE-MAC.

           *-- Step 3: Append MAC and display final message
           STRING WS-REQUEST-BUFFER(1:80) WS-MAC-RESULT
               DELIMITED BY SIZE INTO WS-FINAL-MESSAGE.
           DISPLAY "Step 2: Acquirer sends message with MAC".
           DISPLAY "   Final Msg: " WS-FINAL-MESSAGE(1:88).

           *-- Step 4: Simulate sending and getting an acknowledgement
           PERFORM 8100-SIMULATE-ISSUER-ACKNOWLEDGEMENT.

           *-- Step 5: Acquirer receives and displays the 0230 Ack
           DISPLAY " ".
           DISPLAY "Step 3: Acquirer receives ISSUER ACK (0230)".
           DISPLAY "   Message: " WS-RESPONSE-BUFFER(1:80).
           DISPLAY "--- Advice Flow Complete ---".


      ******************************************************************
      * 8000-SIMULATE-ISSUER-RESPONSE (INTERNAL PARAGRAPH)
      ******************************************************************
       8000-SIMULATE-ISSUER-RESPONSE.
           DISPLAY "   ...Issuer System Processing 0100...".
           IF WS-AMOUNT-NUM > 1000.00
               MOVE "51" TO WS-RESPONSE-CODE
               MOVE "DEC987" TO WS-AUTH-ID
           ELSE
               MOVE "00" TO WS-RESPONSE-CODE
               MOVE "APP123" TO WS-AUTH-ID.
           STRING "0110" "F238800102800000" WS-PAN WS-PROC-CODE
                  WS-AMOUNT-ALPHA WS-STAN WS-ACQUIRER-ID WS-AUTH-ID
                  WS-RESPONSE-CODE WS-TERM-ID
               DELIMITED BY SIZE
               INTO WS-RESPONSE-BUFFER.


      ******************************************************************
      * 8100-SIMULATE-ISSUER-ACKNOWLEDGEMENT (INTERNAL PARAGRAPH)
      ******************************************************************
       8100-SIMULATE-ISSUER-ACKNOWLEDGEMENT.
           DISPLAY "   ...Issuer System Processing 0220...".
           MOVE "00" TO WS-RESPONSE-CODE.
           STRING "0230" "F238000100800000" WS-PAN WS-PROC-CODE
                  WS-AMOUNT-ALPHA WS-STAN WS-RESPONSE-CODE WS-TERM-ID
               DELIMITED BY SIZE
               INTO WS-RESPONSE-BUFFER.


      ******************************************************************
      * 9000-INITIALIZE-BUFFERS
      ******************************************************************
       9000-INITIALIZE-BUFFERS.
           INITIALIZE WS-REQUEST-BUFFER, WS-RESPONSE-BUFFER.
           INITIALIZE WS-FINAL-MESSAGE, WS-MAC-RESULT.
           MOVE 1 TO WS-MSG-POINTER.
           MOVE WS-AMOUNT-NUM TO WS-AMOUNT-ALPHA.

      *----------------------------------------------------------------*
      *--- NEW: PARAGRAPH TO CALL ALGOL PROGRAM                     ---*
      *----------------------------------------------------------------*
       9100-GENERATE-MAC.
           DISPLAY "   Calling ALGOL_MAC_CALC to generate MAC...".
           CALL "ALGOL_MAC_CALC" USING WS-REQUEST-BUFFER,
                                       WS-SECRET-KEY,
                                       WS-MAC-RESULT.
           DISPLAY "   ...Returned from ALGOL. MAC is: " WS-MAC-RESULT.
      *----------------------------------------------------------------*
