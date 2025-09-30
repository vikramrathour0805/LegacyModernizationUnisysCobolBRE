       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISO8583-GEN.
       AUTHOR. Gemini.
       DATE-WRITTEN. 2024-08-13.
       DATE-COMPILED.
      ******************************************************************
      * This program generates various ISO 8583 messages.
      *
      * UPDATE: For financial transactions (0200), this program now
      * CALLS the 'ISSAUTH' program to simulate sending the request
      * for online authorization and displays the received response.
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. UNISYS.
       OBJECT-COMPUTER. UNISYS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * ISO 8583 MESSAGE STRUCTURE AND DATA ELEMENTS
      ******************************************************************
       01  WS-ISO-MESSAGE.
           05 WS-MTI                  PIC X(4).
           05 WS-BITMAP.
              10 WS-PRIMARY-BITMAP   PIC X(8).
              10 WS-SECONDARY-BITMAP PIC X(8) VALUE LOW-VALUES.
           05 WS-DATA-ELEMENTS.
              10 DE002-PAN-LEN        PIC 9(2) COMP.
              10 DE002-PAN            PIC X(19).
              10 DE003-PROC-CODE      PIC X(6).
              10 DE004-TRAN-AMOUNT    PIC X(12).
              10 DE007-TRAN-DATETIME  PIC X(10).
              10 DE011-STAN           PIC X(6).
              10 DE012-LOCAL-TIME     PIC X(6).
              10 DE013-LOCAL-DATE     PIC X(4).
              10 DE024-FUNC-CODE      PIC X(3).
              10 DE032-ACQ-ID-LEN     PIC 9(2) COMP.
              10 DE032-ACQ-ID         PIC X(11).
              10 DE037-RRN            PIC X(12).
              10 DE039-RESP-CODE      PIC X(2).
              10 DE041-CARD-TERM-ID   PIC X(8).
              10 DE042-CARD-ACCP-ID   PIC X(15).
              10 DE048-ADD-DATA-LEN   PIC 9(3) COMP.
              10 DE048-ADD-DATA       PIC X(999).
              10 DE070-NET-MGMT-CODE  PIC X(3).
              10 DE090-ORIG-DATA-LEN  PIC 9(2) COMP.
              10 DE090-ORIG-DATA      PIC X(42).

       01  WS-MESSAGE-BUFFER         PIC X(2048).
       01  WS-RESPONSE-BUFFER        PIC X(2048).
       01  WS-MSG-POINTER            PIC 9(4) COMP VALUE 1.

      ******************************************************************
      * BITMAP MANIPULATION AREA
      ******************************************************************
       01  WS-BITMAP-WORK-AREA.
           05 WS-BITMAP-BYTES         PIC X(16) VALUE LOW-VALUES.
           05 FILLER REDEFINES WS-BITMAP-BYTES.
              10 WS-BITMAP-BIT OCCURS 128 TIMES PIC 1.

       01  WS-BIT-INDEX              PIC 9(3) COMP.

      ******************************************************************
      * USER INPUT AND CONTROL FLAGS
      ******************************************************************
       01  WS-USER-CHOICE            PIC X(1).
       01  WS-PAN-INPUT              PIC X(19).
       01  WS-AMOUNT-INPUT           PIC Z(9)V99.
       01  WS-AMOUNT-FORMATTED       PIC 9(10)V99.

      ******************************************************************
      * PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           PERFORM 1000-DISPLAY-MENU.
           ACCEPT WS-USER-CHOICE.

           EVALUATE WS-USER-CHOICE
               WHEN '1'
                   PERFORM 2000-PROCESS-NETWORK-SIGN-ON
               WHEN '2'
                   PERFORM 2100-PROCESS-NETWORK-SIGN-OFF
               WHEN '3'
                   PERFORM 2200-PROCESS-KEY-EXCHANGE
               WHEN '4'
                   PERFORM 3000-PROCESS-FINANCIAL-PURCHASE
               WHEN '5'
                   PERFORM 4000-PROCESS-REVERSAL
               WHEN OTHER
                   DISPLAY "Invalid option. Program terminating."
           END-EVALUATE.

           STOP RUN.

      ******************************************************************
      * 1000-DISPLAY-MENU SECTION
      ******************************************************************
       1000-DISPLAY-MENU.
           DISPLAY "=================================================".
           DISPLAY "      ISO 8583 MESSAGE GENERATOR".
           DISPLAY "=================================================".
           DISPLAY "  NETWORK MANAGEMENT (0800):".
           DISPLAY "    1. Network Sign-On".
           DISPLAY "    2. Network Sign-Off".
           DISPLAY "    3. Key Exchange".
           DISPLAY " ".
           DISPLAY "  FINANCIAL (0200):".
           DISPLAY "    4. Purchase Transaction".
           DISPLAY " ".
           DISPLAY "  REVERSAL (0420):".
           DISPLAY "    5. Reversal Advice".
           DISPLAY " ".
           DISPLAY "Please select an option: ".

      ******************************************************************
      * 2000-PROCESS-NETWORK-SIGN-ON
      ******************************************************************
       2000-PROCESS-NETWORK-SIGN-ON.
           PERFORM 9000-INITIALIZE-MESSAGE.
           MOVE "0800" TO WS-MTI.
           PERFORM 9100-SET-BIT-ON USING 7.
           MOVE "0813103000" TO DE007-TRAN-DATETIME.
           PERFORM 9100-SET-BIT-ON USING 11.
           MOVE "123456" TO DE011-STAN.
           PERFORM 9100-SET-BIT-ON USING 70.
           MOVE "001" TO DE070-NET-MGMT-CODE.
           PERFORM 9200-CONSTRUCT-AND-DISPLAY-MSG.

      ******************************************************************
      * 2100-PROCESS-NETWORK-SIGN-OFF
      ******************************************************************
       2100-PROCESS-NETWORK-SIGN-OFF.
           PERFORM 9000-INITIALIZE-MESSAGE.
           MOVE "0800" TO WS-MTI.
           PERFORM 9100-SET-BIT-ON USING 7.
           MOVE "0813103005" TO DE007-TRAN-DATETIME.
           PERFORM 9100-SET-BIT-ON USING 11.
           MOVE "123457" TO DE011-STAN.
           PERFORM 9100-SET-BIT-ON USING 70.
           MOVE "002" TO DE070-NET-MGMT-CODE.
           PERFORM 9200-CONSTRUCT-AND-DISPLAY-MSG.

      ******************************************************************
      * 2200-PROCESS-KEY-EXCHANGE
      ******************************************************************
       2200-PROCESS-KEY-EXCHANGE.
           PERFORM 9000-INITIALIZE-MESSAGE.
           MOVE "0800" TO WS-MTI.
           PERFORM 9100-SET-BIT-ON USING 7.
           MOVE "0813103100" TO DE007-TRAN-DATETIME.
           PERFORM 9100-SET-BIT-ON USING 11.
           MOVE "123458" TO DE011-STAN.
           PERFORM 9100-SET-BIT-ON USING 70.
           MOVE "161" TO DE070-NET-MGMT-CODE.
           PERFORM 9200-CONSTRUCT-AND-DISPLAY-MSG.

      ******************************************************************
      * 3000-PROCESS-FINANCIAL-PURCHASE
      ******************************************************************
       3000-PROCESS-FINANCIAL-PURCHASE.
           PERFORM 9000-INITIALIZE-MESSAGE.
           MOVE "0200" TO WS-MTI.

           DISPLAY "Enter PAN (Card Number): ".
           ACCEPT WS-PAN-INPUT.
           DISPLAY "Enter Amount (e.g., 123.45): ".
           ACCEPT WS-AMOUNT-INPUT.
           MOVE WS-AMOUNT-INPUT TO WS-AMOUNT-FORMATTED.

           PERFORM 9100-SET-BIT-ON USING 2.
           MOVE LENGTH OF WS-PAN-INPUT TO DE002-PAN-LEN.
           MOVE WS-PAN-INPUT TO DE002-PAN.
           PERFORM 9100-SET-BIT-ON USING 3.
           MOVE "000000" TO DE003-PROC-CODE.
           PERFORM 9100-SET-BIT-ON USING 4.
           STRING WS-AMOUNT-FORMATTED DELIMITED BY SIZE
               INTO DE004-TRAN-AMOUNT.
           PERFORM 9100-SET-BIT-ON USING 7.
           MOVE "0813103200" TO DE007-TRAN-DATETIME.
           PERFORM 9100-SET-BIT-ON USING 11.
           MOVE "123459" TO DE011-STAN.
           PERFORM 9100-SET-BIT-ON USING 41.
           MOVE "TERM1234" TO DE041-CARD-TERM-ID.

           PERFORM 9200-CONSTRUCT-AND-DISPLAY-MSG.
           PERFORM 3100-SEND-FOR-AUTHORIZATION.

      ******************************************************************
      * 3100-SEND-FOR-AUTHORIZATION  (*** NEW SECTION ***)
      * Calls the issuer program and displays the response.
      ******************************************************************
       3100-SEND-FOR-AUTHORIZATION.
           DISPLAY " ".
           DISPLAY ">>> Calling ISSAUTH program for authorization...".
           CALL "ISSAUTH" USING WS-MESSAGE-BUFFER, WS-RESPONSE-BUFFER.
           DISPLAY "<<< Returned from ISSAUTH program.".
           DISPLAY " ".
           DISPLAY "-------------------------------------------------".
           DISPLAY "      Issuer Response (0210) Received".
           DISPLAY "-------------------------------------------------".
           DISPLAY "Full Response Message: ".
           DISPLAY WS-RESPONSE-BUFFER(1:100).
           DISPLAY "-------------------------------------------------".

      ******************************************************************
      * 4000-PROCESS-REVERSAL
      ******************************************************************
       4000-PROCESS-REVERSAL.
           PERFORM 9000-INITIALIZE-MESSAGE.
           MOVE "0420" TO WS-MTI.
           PERFORM 9100-SET-BIT-ON USING 2.
           MOVE 16 TO DE002-PAN-LEN.
           MOVE "4111111111111111" TO DE002-PAN.
           PERFORM 9100-SET-BIT-ON USING 3.
           MOVE "000000" TO DE003-PROC-CODE.
           PERFORM 9100-SET-BIT-ON USING 4.
           MOVE "000000010000" TO DE004-TRAN-AMOUNT.
           PERFORM 9100-SET-BIT-ON USING 11.
           MOVE "987654" TO DE011-STAN.
           PERFORM 9100-SET-BIT-ON USING 39.
           MOVE "00" TO DE039-RESP-CODE.
           PERFORM 9100-SET-BIT-ON USING 90.
           MOVE 42 TO DE090-ORIG-DATA-LEN.
           MOVE "02009876540813103500..." TO DE090-ORIG-DATA.
           PERFORM 9200-CONSTRUCT-AND-DISPLAY-MSG.

      ******************************************************************
      * 9000-INITIALIZE-MESSAGE
      ******************************************************************
       9000-INITIALIZE-MESSAGE.
           INITIALIZE WS-ISO-MESSAGE.
           INITIALIZE WS-BITMAP-WORK-AREA.
           MOVE LOW-VALUES TO WS-MESSAGE-BUFFER, WS-RESPONSE-BUFFER.
           MOVE 1 TO WS-MSG-POINTER.

      ******************************************************************
      * 9100-SET-BIT-ON
      ******************************************************************
       9100-SET-BIT-ON USING IN-BIT-NUMBER.
           05 IN-BIT-NUMBER PIC 9(3).
           MOVE 1 TO WS-BITMAP-BIT(IN-BIT-NUMBER).
           IF IN-BIT-NUMBER > 64
               MOVE 1 TO WS-BITMAP-BIT(1).

      ******************************************************************
      * 9200-CONSTRUCT-AND-DISPLAY-MSG
      ******************************************************************
       9200-CONSTRUCT-AND-DISPLAY-MSG.
           MOVE WS-BITMAP-BYTES(1:8) TO WS-PRIMARY-BITMAP.
           IF WS-BITMAP-BIT(1) = 1
               MOVE WS-BITMAP-BYTES(9:8) TO WS-SECONDARY-BITMAP.
           MOVE WS-MTI TO WS-MESSAGE-BUFFER(WS-MSG-POINTER:4).
           ADD 4 TO WS-MSG-POINTER.
           MOVE WS-PRIMARY-BITMAP TO WS-MESSAGE-BUFFER(WS-MSG-POINTER:8).
           ADD 8 TO WS-MSG-POINTER.
           IF WS-BITMAP-BIT(1) = 1
               MOVE WS-SECONDARY-BITMAP
                   TO WS-MESSAGE-BUFFER(WS-MSG-POINTER:8)
               ADD 8 TO WS-MSG-POINTER.
           PERFORM 9300-APPEND-ELEMENT VARYING WS-BIT-INDEX
               FROM 2 BY 1 UNTIL WS-BIT-INDEX > 128.
           DISPLAY " ".
           DISPLAY "Constructed ISO 8583 Request (0200):".
           DISPLAY "Full Request Message: ".
           DISPLAY WS-MESSAGE-BUFFER(1:WS-MSG-POINTER - 1).

      ******************************************************************
      * 9300-APPEND-ELEMENT
      ******************************************************************
       9300-APPEND-ELEMENT.
           IF WS-BITMAP-BIT(WS-BIT-INDEX) = 1
               EVALUATE WS-BIT-INDEX
                   WHEN 2
                       MOVE DE002-PAN-LEN TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:2)
                       ADD 2 TO WS-MSG-POINTER
                       MOVE DE002-PAN TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:DE002-PAN-LEN)
                       ADD DE002-PAN-LEN TO WS-MSG-POINTER
                   WHEN 3
                       MOVE DE003-PROC-CODE TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:6)
                       ADD 6 TO WS-MSG-POINTER
                   WHEN 4
                       MOVE DE004-TRAN-AMOUNT TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:12)
                       ADD 12 TO WS-MSG-POINTER
                   WHEN 7
                       MOVE DE007-TRAN-DATETIME TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:10)
                       ADD 10 TO WS-MSG-POINTER
                   WHEN 11
                       MOVE DE011-STAN TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:6)
                       ADD 6 TO WS-MSG-POINTER
                   WHEN 41
                       MOVE DE041-CARD-TERM-ID TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:8)
                       ADD 8 TO WS-MSG-POINTER
                   WHEN 70
                       MOVE DE070-NET-MGMT-CODE TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:3)
                       ADD 3 TO WS-MSG-POINTER
                   WHEN 90
                       MOVE DE090-ORIG-DATA-LEN TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:2)
                       ADD 2 TO WS-MSG-POINTER
                       MOVE DE090-ORIG-DATA TO
                           WS-MESSAGE-BUFFER(WS-MSG-POINTER:42)
                       ADD 42 TO WS-MSG-POINTER
               END-EVALUATE.
