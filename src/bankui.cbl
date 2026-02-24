       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKUI.
      *================================================================*
      * BANKUI - Bank Ledger Terminal User Interface                    *
      * Green-screen TUI using COBOL SCREEN SECTION.                   *
      * Combines account management and transaction processing         *
      * into a single interactive terminal application.                *
      *                                                                *
      * Demonstrates: SCREEN SECTION, CRT STATUS, SPECIAL-NAMES,      *
      * FOREGROUND-COLOR/BACKGROUND-COLOR, HIGHLIGHT, REVERSE-VIDEO,   *
      * USING/FROM clauses, function key detection.                    *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE
               ASSIGN TO "data/ACCOUNTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-STATUS.

           SELECT TXN-FILE
               ASSIGN TO "data/TRANSACTIONS.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TXN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY ACCT-REC.

       FD  TXN-FILE.
       COPY TXNL-REC.

       WORKING-STORAGE SECTION.
      *--- CRT STATUS for function key detection ---
       01  WS-CRT-STATUS            PIC 9(4) VALUE 0.
           88  WS-KEY-ENTER             VALUE 0.
           88  WS-KEY-F3                VALUE 1003.
           88  WS-KEY-F7                VALUE 1007.
           88  WS-KEY-F8                VALUE 1008.
           88  WS-KEY-F12               VALUE 1012.

      *--- File status codes ---
       01  WS-ACCT-STATUS            PIC XX.
           88  WS-ACCT-OK               VALUE "00".
           88  WS-ACCT-EOF              VALUE "10".
           88  WS-ACCT-DUP-KEY          VALUE "22".
           88  WS-ACCT-NOT-FOUND        VALUE "23".
           88  WS-ACCT-FILE-MISSING     VALUE "35".

       01  WS-TXN-STATUS             PIC XX.
           88  WS-TXN-OK                VALUE "00".
           88  WS-TXN-EOF               VALUE "10".
           88  WS-TXN-FILE-MISSING      VALUE "35".

      *--- File open flags ---
       01  WS-ACCT-OPEN-FLAG         PIC 9 VALUE 0.
           88  WS-ACCT-IS-OPEN          VALUE 1.
       01  WS-TXN-OPEN-FLAG          PIC 9 VALUE 0.
           88  WS-TXN-IS-OPEN           VALUE 1.

      *--- Program flow control ---
       01  WS-PROGRAM-DONE           PIC 9 VALUE 0.
           88  WS-EXIT-PROGRAM          VALUE 1.
       01  WS-SCREEN-DONE            PIC 9 VALUE 0.
           88  WS-EXIT-SCREEN           VALUE 1.

      *--- Menu choice ---
       01  WS-MENU-CHOICE            PIC X(1) VALUE SPACES.

      *--- Create account input fields ---
       01  WS-INP-NAME               PIC X(30) VALUE SPACES.
       01  WS-INP-TYPE               PIC X(1) VALUE SPACES.
       01  WS-INP-DEPOSIT            PIC X(12) VALUE SPACES.
       01  WS-DEPOSIT-AMT            PIC 9(9)V99 VALUE ZEROS.

      *--- Lookup/transaction input fields ---
       01  WS-INP-ACCTNO             PIC X(8) VALUE SPACES.
       01  WS-INP-XFER-ACCTNO        PIC X(8) VALUE SPACES.
       01  WS-INP-AMOUNT             PIC X(12) VALUE SPACES.
       01  WS-INP-DESC               PIC X(20) VALUE SPACES.

      *--- Account number generation ---
       01  WS-NEXT-ACCT-NO           PIC 9(8) VALUE 10000001.

      *--- Transaction ID generation ---
       01  WS-NEXT-TXN-ID            PIC 9(10) VALUE 1.
       01  WS-TXN-EOF-FLAG           PIC 9 VALUE 0.
           88  WS-TXN-AT-EOF            VALUE 1.

      *--- Save fields for transfer ---
       01  WS-SAVE-ACCT-NO           PIC 9(8).
       01  WS-SAVE-ACCT-NAME         PIC X(30).
       01  WS-SAVE-ACCT-BAL          PIC S9(9)V99.
       01  WS-SAVE-ACCT-TYPE         PIC X(1).
       01  WS-SAVE-ACCT-STAT         PIC X(1).
       01  WS-SAVE-ACCT-DT           PIC 9(8).

      *--- Working fields ---
       01  WS-TXN-AMT                PIC 9(7)V99.
       01  WS-NEW-BAL                PIC S9(9)V99.
       01  WS-SIZE-ERR-FLAG          PIC 9 VALUE 0.
       01  WS-CONFIRM                PIC X(1) VALUE SPACES.
       01  WS-DUMMY                  PIC X(1) VALUE SPACES.

      *--- Display fields ---
       01  WS-DISP-BAL               PIC $$$,$$$,$$9.99-.
       01  WS-DISP-AMT               PIC $$$,$$$,$$9.99.
       01  WS-DISP-NO                PIC 9(8).
       01  WS-DISP-NAME              PIC X(30).
       01  WS-DISP-TYPE-FULL         PIC X(8).
       01  WS-DISP-STATUS-FULL       PIC X(6).
       01  WS-DISP-OPEN-DT           PIC 9(8).
       01  WS-BLANK-LINE             PIC X(80) VALUE SPACES.

      *--- Date/time fields ---
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURR-YEAR          PIC 9(4).
           05  WS-CURR-MONTH         PIC 9(2).
           05  WS-CURR-DAY           PIC 9(2).
           05  WS-CURR-HH            PIC 9(2).
           05  WS-CURR-MM            PIC 9(2).
           05  WS-CURR-SS            PIC 9(2).
           05  WS-CURR-REST          PIC X(7).
       01  WS-TODAY-DATE              PIC 9(8).
       01  WS-NOW-TIME                PIC 9(6).

      *--- Result messages (5 lines) ---
       01  WS-RESULT-TITLE           PIC X(40) VALUE SPACES.
       01  WS-RESULT-LINE1           PIC X(60) VALUE SPACES.
       01  WS-RESULT-LINE2           PIC X(60) VALUE SPACES.
       01  WS-RESULT-LINE3           PIC X(60) VALUE SPACES.
       01  WS-RESULT-LINE4           PIC X(60) VALUE SPACES.
       01  WS-RESULT-LINE5           PIC X(60) VALUE SPACES.

      *--- Account list table (15 rows per page) ---
       01  WS-LIST-ROWS.
           05  WS-LIST-ROW-01        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-02        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-03        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-04        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-05        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-06        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-07        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-08        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-09        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-10        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-11        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-12        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-13        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-14        PIC X(68) VALUE SPACES.
           05  WS-LIST-ROW-15        PIC X(68) VALUE SPACES.
       01  WS-LIST-TABLE REDEFINES WS-LIST-ROWS.
           05  WS-LIST-ROW           PIC X(68) OCCURS 15 TIMES.
       01  WS-LIST-COUNT              PIC 99 VALUE 0.
       01  WS-LIST-PAGE               PIC 999 VALUE 1.
       01  WS-LIST-HAS-MORE           PIC 9 VALUE 0.
       01  WS-LIST-SKIP               PIC 9(5) VALUE 0.
       01  WS-LIST-IDX                PIC 99 VALUE 0.
       01  WS-PAGE-INFO               PIC X(30) VALUE SPACES.

      *================================================================*
      * SCREEN SECTION - Declarative screen layouts                    *
      *================================================================*
       SCREEN SECTION.

      *--- Main Menu Screen ---
       01  MAIN-MENU-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 17
               VALUE "COBOL BANK LEDGER - MAIN MENU"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 3 COLUMN 17
               VALUE "+--------------------------------------------+"
               FOREGROUND-COLOR 2.
           05  LINE 4 COLUMN 17
               VALUE "|     ACCOUNT MANAGEMENT                     |"
               FOREGROUND-COLOR 2.
           05  LINE 5 COLUMN 17
               VALUE "|                                            |"
               FOREGROUND-COLOR 2.
           05  LINE 6 COLUMN 17
               VALUE "|  1. Create New Account                     |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 7 COLUMN 17
               VALUE "|  2. Lookup Account                         |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 8 COLUMN 17
               VALUE "|  3. List All Accounts                      |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 9 COLUMN 17
               VALUE "|  4. Close Account                          |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 10 COLUMN 17
               VALUE "|                                            |"
               FOREGROUND-COLOR 2.
           05  LINE 11 COLUMN 17
               VALUE "|     TRANSACTIONS                           |"
               FOREGROUND-COLOR 2.
           05  LINE 12 COLUMN 17
               VALUE "|                                            |"
               FOREGROUND-COLOR 2.
           05  LINE 13 COLUMN 17
               VALUE "|  5. Deposit                                |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 14 COLUMN 17
               VALUE "|  6. Withdraw                               |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 15 COLUMN 17
               VALUE "|  7. Transfer                               |"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 16 COLUMN 17
               VALUE "|                                            |"
               FOREGROUND-COLOR 2.
           05  LINE 17 COLUMN 17
               VALUE "+--------------------------------------------+"
               FOREGROUND-COLOR 2.
           05  LINE 19 COLUMN 17 VALUE "Selection: "
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 19 COLUMN 28 PIC X(1)
               USING WS-MENU-CHOICE
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Create Account Screen ---
       01  CREATE-ACCT-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 25
               VALUE "CREATE NEW ACCOUNT"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 4 COLUMN 5
               VALUE "Account holder name:"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 5 COLUMN 5 PIC X(30)
               USING WS-INP-NAME
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 7 COLUMN 5
               VALUE "Account type (C=Checking, S=Savings):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 8 COLUMN 5 PIC X(1)
               USING WS-INP-TYPE
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 10 COLUMN 5
               VALUE "Opening deposit (e.g. 1000.00):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 11 COLUMN 5 PIC X(12)
               USING WS-INP-DEPOSIT
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "Enter=Submit  F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Account Lookup Screen (input) ---
       01  LOOKUP-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 27
               VALUE "ACCOUNT LOOKUP"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 5 COLUMN 5
               VALUE "Enter account number (8 digits):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 7 COLUMN 5 PIC X(8)
               USING WS-INP-ACCTNO
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "Enter=Search  F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Account Detail Screen (display-only) ---
       01  DETAIL-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 27
               VALUE "ACCOUNT DETAIL"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 4 COLUMN 5 VALUE "Account No:"
               FOREGROUND-COLOR 2.
           05  LINE 4 COLUMN 20 PIC 9(8)
               FROM WS-DISP-NO
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 6 COLUMN 5 VALUE "Name:"
               FOREGROUND-COLOR 2.
           05  LINE 6 COLUMN 20 PIC X(30)
               FROM WS-DISP-NAME
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 8 COLUMN 5 VALUE "Balance:"
               FOREGROUND-COLOR 2.
           05  LINE 8 COLUMN 20 PIC $$$,$$$,$$9.99-
               FROM WS-DISP-BAL
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 10 COLUMN 5 VALUE "Type:"
               FOREGROUND-COLOR 2.
           05  LINE 10 COLUMN 20 PIC X(8)
               FROM WS-DISP-TYPE-FULL
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 12 COLUMN 5 VALUE "Status:"
               FOREGROUND-COLOR 2.
           05  LINE 12 COLUMN 20 PIC X(6)
               FROM WS-DISP-STATUS-FULL
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 14 COLUMN 5 VALUE "Opened:"
               FOREGROUND-COLOR 2.
           05  LINE 14 COLUMN 20 PIC 9(8)
               FROM WS-DISP-OPEN-DT
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 22 COLUMN 5 PIC X(1)
               USING WS-DUMMY
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 0.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Account List Screen ---
       01  LIST-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 28
               VALUE "ACCOUNT LIST"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 3 COLUMN 2
               VALUE "Acct No   Name                 "
               FOREGROUND-COLOR 2 HIGHLIGHT REVERSE-VIDEO.
           05  LINE 3 COLUMN 33
               VALUE "Balance          Type     Status"
               FOREGROUND-COLOR 2 HIGHLIGHT REVERSE-VIDEO.
           05  LINE 5  COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-01 FOREGROUND-COLOR 2.
           05  LINE 6  COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-02 FOREGROUND-COLOR 2.
           05  LINE 7  COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-03 FOREGROUND-COLOR 2.
           05  LINE 8  COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-04 FOREGROUND-COLOR 2.
           05  LINE 9  COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-05 FOREGROUND-COLOR 2.
           05  LINE 10 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-06 FOREGROUND-COLOR 2.
           05  LINE 11 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-07 FOREGROUND-COLOR 2.
           05  LINE 12 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-08 FOREGROUND-COLOR 2.
           05  LINE 13 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-09 FOREGROUND-COLOR 2.
           05  LINE 14 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-10 FOREGROUND-COLOR 2.
           05  LINE 15 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-11 FOREGROUND-COLOR 2.
           05  LINE 16 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-12 FOREGROUND-COLOR 2.
           05  LINE 17 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-13 FOREGROUND-COLOR 2.
           05  LINE 18 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-14 FOREGROUND-COLOR 2.
           05  LINE 19 COLUMN 2 PIC X(68)
               FROM WS-LIST-ROW-15 FOREGROUND-COLOR 2.
           05  LINE 21 COLUMN 5 PIC X(30)
               FROM WS-PAGE-INFO
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 22 COLUMN 2 PIC X(1)
               USING WS-DUMMY
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 0.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "F7=Prev  F8=Next  F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Transaction Entry Screen (Deposit/Withdraw) ---
       01  TXN-ENTRY-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 20 PIC X(40)
               FROM WS-RESULT-TITLE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 4 COLUMN 5
               VALUE "Account number (8 digits):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 5 COLUMN 5 PIC X(8)
               USING WS-INP-ACCTNO
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 7 COLUMN 5
               VALUE "Amount:"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 8 COLUMN 5 PIC X(12)
               USING WS-INP-AMOUNT
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 10 COLUMN 5
               VALUE "Description (up to 20 chars):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 11 COLUMN 5 PIC X(20)
               USING WS-INP-DESC
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "Enter=Submit  F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Transfer Entry Screen ---
       01  XFER-ENTRY-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 30
               VALUE "TRANSFER"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 4 COLUMN 5
               VALUE "From account (8 digits):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 5 COLUMN 5 PIC X(8)
               USING WS-INP-ACCTNO
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 7 COLUMN 5
               VALUE "To account (8 digits):"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 8 COLUMN 5 PIC X(8)
               USING WS-INP-XFER-ACCTNO
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 10 COLUMN 5
               VALUE "Amount:"
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 11 COLUMN 5 PIC X(12)
               USING WS-INP-AMOUNT
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "Enter=Submit  F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Confirm Screen ---
       01  CONFIRM-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 27
               VALUE "CONFIRM ACTION"
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 5 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE1
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 7 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE2
               FOREGROUND-COLOR 2.
           05  LINE 9 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE3
               FOREGROUND-COLOR 2.
           05  LINE 12 COLUMN 5
               VALUE "Confirm? (Y/N): "
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 12 COLUMN 22 PIC X(1)
               USING WS-CONFIRM
               FOREGROUND-COLOR 3 BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "Enter=Confirm  F3=Cancel"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *--- Result Screen ---
       01  RESULT-SCR.
           05  BLANK SCREEN
               FOREGROUND-COLOR 2 BACKGROUND-COLOR 0.
           05  LINE 1 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2.
           05  LINE 1 COLUMN 20 PIC X(40)
               FROM WS-RESULT-TITLE
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 2
               HIGHLIGHT.
           05  LINE 5 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE1
               FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 7 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE2
               FOREGROUND-COLOR 2.
           05  LINE 9 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE3
               FOREGROUND-COLOR 2.
           05  LINE 11 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE4
               FOREGROUND-COLOR 2.
           05  LINE 13 COLUMN 5 PIC X(60)
               FROM WS-RESULT-LINE5
               FOREGROUND-COLOR 2.
           05  LINE 22 COLUMN 5 PIC X(1)
               USING WS-DUMMY
               FOREGROUND-COLOR 0 BACKGROUND-COLOR 0.
           05  LINE 24 COLUMN 1 PIC X(80)
               FROM WS-BLANK-LINE
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
           05  LINE 24 COLUMN 3
               VALUE "F3=Back  F12=Quit"
               FOREGROUND-COLOR 7 BACKGROUND-COLOR 1
               HIGHLIGHT.

      *================================================================*
      * PROCEDURE DIVISION                                             *
      *================================================================*
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM OPEN-FILES
           IF NOT WS-ACCT-IS-OPEN
               DISPLAY "FATAL: Cannot open account file."
               STOP RUN
           END-IF
           PERFORM FIND-NEXT-ACCT-NO
           IF WS-TXN-IS-OPEN
               PERFORM FIND-NEXT-TXN-ID
           END-IF
           PERFORM SHOW-MAIN-MENU UNTIL WS-EXIT-PROGRAM
           PERFORM CLOSE-FILES
           STOP RUN.

      *--- File Operations ---
       OPEN-FILES.
           OPEN I-O ACCT-FILE
           IF WS-ACCT-OK
               SET WS-ACCT-IS-OPEN TO TRUE
           ELSE
               IF WS-ACCT-FILE-MISSING
                   OPEN OUTPUT ACCT-FILE
                   IF WS-ACCT-OK
                       CLOSE ACCT-FILE
                       OPEN I-O ACCT-FILE
                       IF WS-ACCT-OK
                           SET WS-ACCT-IS-OPEN TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF

           OPEN EXTEND TXN-FILE
           IF WS-TXN-OK
               SET WS-TXN-IS-OPEN TO TRUE
           ELSE
               IF WS-TXN-FILE-MISSING
                   OPEN OUTPUT TXN-FILE
                   IF WS-TXN-OK
                       SET WS-TXN-IS-OPEN TO TRUE
                   END-IF
               END-IF
           END-IF.

       CLOSE-FILES.
           IF WS-ACCT-IS-OPEN
               CLOSE ACCT-FILE
               MOVE 0 TO WS-ACCT-OPEN-FLAG
           END-IF
           IF WS-TXN-IS-OPEN
               CLOSE TXN-FILE
               MOVE 0 TO WS-TXN-OPEN-FLAG
           END-IF.

       FIND-NEXT-ACCT-NO.
           MOVE HIGH-VALUES TO ACCT-NO
           START ACCT-FILE KEY IS LESS THAN ACCT-NO
               INVALID KEY
                   MOVE 10000001 TO WS-NEXT-ACCT-NO
                   GO TO FIND-NEXT-ACCT-NO-EXIT
           END-START
           READ ACCT-FILE PREVIOUS
               AT END
                   MOVE 10000001 TO WS-NEXT-ACCT-NO
                   GO TO FIND-NEXT-ACCT-NO-EXIT
           END-READ
           ADD 1 TO ACCT-NO GIVING WS-NEXT-ACCT-NO.
       FIND-NEXT-ACCT-NO-EXIT.
           EXIT.

       FIND-NEXT-TXN-ID.
           IF WS-TXN-IS-OPEN
               CLOSE TXN-FILE
               MOVE 0 TO WS-TXN-OPEN-FLAG
           END-IF
           OPEN INPUT TXN-FILE
           IF WS-TXN-OK
               MOVE 0 TO WS-NEXT-TXN-ID
               MOVE 0 TO WS-TXN-EOF-FLAG
               PERFORM READ-SINGLE-TXN UNTIL WS-TXN-AT-EOF
               ADD 1 TO WS-NEXT-TXN-ID
               CLOSE TXN-FILE
           ELSE
               MOVE 1 TO WS-NEXT-TXN-ID
           END-IF
           OPEN EXTEND TXN-FILE
           IF WS-TXN-OK
               SET WS-TXN-IS-OPEN TO TRUE
           ELSE
               MOVE 0 TO WS-TXN-OPEN-FLAG
           END-IF.

       READ-SINGLE-TXN.
           READ TXN-FILE
               AT END
                   SET WS-TXN-AT-EOF TO TRUE
               NOT AT END
                   IF TXN-ID > WS-NEXT-TXN-ID
                       MOVE TXN-ID TO WS-NEXT-TXN-ID
                   END-IF
           END-READ.

       GET-CURRENT-DATETIME.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-TODAY-DATE
           END-STRING
           STRING WS-CURR-HH WS-CURR-MM WS-CURR-SS
               DELIMITED BY SIZE INTO WS-NOW-TIME
           END-STRING.

      *--- Main Menu ---
       SHOW-MAIN-MENU.
           MOVE SPACES TO WS-MENU-CHOICE
           DISPLAY MAIN-MENU-SCR
           ACCEPT MAIN-MENU-SCR
           EVALUATE TRUE
               WHEN WS-KEY-F12
                   MOVE 1 TO WS-PROGRAM-DONE
               WHEN OTHER
                   EVALUATE WS-MENU-CHOICE
                       WHEN "1" PERFORM DO-CREATE-ACCOUNT
                       WHEN "2" PERFORM DO-LOOKUP-ACCOUNT
                       WHEN "3" PERFORM DO-LIST-ACCOUNTS
                       WHEN "4" PERFORM DO-CLOSE-ACCOUNT
                       WHEN "5" PERFORM DO-DEPOSIT
                       WHEN "6" PERFORM DO-WITHDRAWAL
                       WHEN "7" PERFORM DO-TRANSFER
                   END-EVALUATE
           END-EVALUATE.

      *--- Populate detail display fields from ACCT-REC ---
       POPULATE-DETAIL-FIELDS.
           MOVE ACCT-NO      TO WS-DISP-NO
           MOVE ACCT-NAME    TO WS-DISP-NAME
           MOVE ACCT-BAL     TO WS-DISP-BAL
           MOVE ACCT-OPEN-DT TO WS-DISP-OPEN-DT
           EVALUATE ACCT-TYPE
               WHEN "C" MOVE "Checking" TO WS-DISP-TYPE-FULL
               WHEN "S" MOVE "Savings " TO WS-DISP-TYPE-FULL
               WHEN OTHER MOVE "Unknown " TO WS-DISP-TYPE-FULL
           END-EVALUATE
           EVALUATE ACCT-STATUS
               WHEN "A" MOVE "Active" TO WS-DISP-STATUS-FULL
               WHEN "X" MOVE "Closed" TO WS-DISP-STATUS-FULL
               WHEN OTHER MOVE "  ?   " TO WS-DISP-STATUS-FULL
           END-EVALUATE.

      *--- Show Result Screen (utility) ---
       SHOW-RESULT.
           DISPLAY RESULT-SCR
           ACCEPT RESULT-SCR.

      *================================================================*
      * CREATE ACCOUNT                                                 *
      *================================================================*
       DO-CREATE-ACCOUNT.
           MOVE SPACES TO WS-INP-NAME WS-INP-TYPE WS-INP-DEPOSIT
           DISPLAY CREATE-ACCT-SCR
           ACCEPT CREATE-ACCT-SCR
           IF WS-KEY-F3
               GO TO DO-CREATE-ACCOUNT-EXIT
           END-IF
           IF WS-KEY-F12
               MOVE 1 TO WS-PROGRAM-DONE
               GO TO DO-CREATE-ACCOUNT-EXIT
           END-IF

      *    Validate name
           IF WS-INP-NAME = SPACES
               MOVE "CREATE ACCOUNT - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Name cannot be empty."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-CREATE-ACCOUNT-EXIT
           END-IF

      *    Validate type
           MOVE FUNCTION UPPER-CASE(WS-INP-TYPE)
               TO WS-INP-TYPE
           IF WS-INP-TYPE NOT = "C" AND "S"
               MOVE "CREATE ACCOUNT - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Invalid type. Must be C or S."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-CREATE-ACCOUNT-EXIT
           END-IF

      *    Validate deposit
           MOVE FUNCTION NUMVAL(WS-INP-DEPOSIT)
               TO WS-DEPOSIT-AMT
           IF WS-DEPOSIT-AMT < 0.01
               MOVE "CREATE ACCOUNT - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Deposit must be at least $0.01."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-CREATE-ACCOUNT-EXIT
           END-IF

      *    Create the account record
           MOVE WS-NEXT-ACCT-NO   TO ACCT-NO
           MOVE WS-INP-NAME       TO ACCT-NAME
           MOVE WS-DEPOSIT-AMT    TO ACCT-BAL
           MOVE WS-INP-TYPE       TO ACCT-TYPE
           MOVE "A"               TO ACCT-STATUS
           PERFORM GET-CURRENT-DATETIME
           MOVE WS-TODAY-DATE     TO ACCT-OPEN-DT

           WRITE ACCT-REC
           IF WS-ACCT-OK
               MOVE "ACCOUNT CREATED" TO WS-RESULT-TITLE
               MOVE SPACES TO WS-RESULT-LINE1
                   WS-RESULT-LINE2 WS-RESULT-LINE3
                   WS-RESULT-LINE4 WS-RESULT-LINE5
               STRING "Account Number: " ACCT-NO
                   DELIMITED BY SIZE
                   INTO WS-RESULT-LINE1
               END-STRING
               STRING "Name:           " ACCT-NAME
                   DELIMITED BY SIZE
                   INTO WS-RESULT-LINE2
               END-STRING
               MOVE ACCT-BAL TO WS-DISP-BAL
               STRING "Balance:        " WS-DISP-BAL
                   DELIMITED BY SIZE
                   INTO WS-RESULT-LINE3
               END-STRING
               EVALUATE ACCT-TYPE
                   WHEN "C"
                       MOVE "Type:           Checking"
                           TO WS-RESULT-LINE4
                   WHEN "S"
                       MOVE "Type:           Savings"
                           TO WS-RESULT-LINE4
               END-EVALUATE
               MOVE "Account is now active."
                   TO WS-RESULT-LINE5
               ADD 1 TO WS-NEXT-ACCT-NO
           ELSE
               MOVE "CREATE ACCOUNT - ERROR"
                   TO WS-RESULT-TITLE
               MOVE SPACES TO WS-RESULT-LINE1
                   WS-RESULT-LINE2 WS-RESULT-LINE3
                   WS-RESULT-LINE4 WS-RESULT-LINE5
               STRING "Could not create account. Status: "
                   WS-ACCT-STATUS
                   DELIMITED BY SIZE
                   INTO WS-RESULT-LINE1
               END-STRING
           END-IF
           PERFORM SHOW-RESULT.
       DO-CREATE-ACCOUNT-EXIT.
           EXIT.

      *================================================================*
      * LOOKUP ACCOUNT                                                 *
      *================================================================*
       DO-LOOKUP-ACCOUNT.
           MOVE SPACES TO WS-INP-ACCTNO
           DISPLAY LOOKUP-SCR
           ACCEPT LOOKUP-SCR
           IF WS-KEY-F3
               GO TO DO-LOOKUP-ACCOUNT-EXIT
           END-IF
           IF WS-KEY-F12
               MOVE 1 TO WS-PROGRAM-DONE
               GO TO DO-LOOKUP-ACCOUNT-EXIT
           END-IF

           MOVE WS-INP-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "ACCOUNT LOOKUP - NOT FOUND"
                       TO WS-RESULT-TITLE
                   MOVE SPACES TO WS-RESULT-LINE1
                       WS-RESULT-LINE2 WS-RESULT-LINE3
                       WS-RESULT-LINE4 WS-RESULT-LINE5
                   STRING "Account " WS-INP-ACCTNO
                       " was not found."
                       DELIMITED BY SIZE
                       INTO WS-RESULT-LINE1
                   END-STRING
                   PERFORM SHOW-RESULT
                   GO TO DO-LOOKUP-ACCOUNT-EXIT
           END-READ

           PERFORM POPULATE-DETAIL-FIELDS
           DISPLAY DETAIL-SCR
           ACCEPT DETAIL-SCR.
       DO-LOOKUP-ACCOUNT-EXIT.
           EXIT.

      *================================================================*
      * LIST ACCOUNTS (Paged)                                          *
      *================================================================*
       DO-LIST-ACCOUNTS.
           MOVE 1 TO WS-LIST-PAGE
           PERFORM LOAD-LIST-PAGE
           MOVE 0 TO WS-SCREEN-DONE
           PERFORM SHOW-LIST-SCREEN
               UNTIL WS-EXIT-SCREEN OR WS-EXIT-PROGRAM.

       SHOW-LIST-SCREEN.
           MOVE SPACES TO WS-DUMMY
           DISPLAY LIST-SCR
           ACCEPT LIST-SCR
           EVALUATE TRUE
               WHEN WS-KEY-F3
                   MOVE 1 TO WS-SCREEN-DONE
               WHEN WS-KEY-F12
                   MOVE 1 TO WS-PROGRAM-DONE
               WHEN WS-KEY-F7
                   IF WS-LIST-PAGE > 1
                       SUBTRACT 1 FROM WS-LIST-PAGE
                       PERFORM LOAD-LIST-PAGE
                   END-IF
               WHEN WS-KEY-F8
                   IF WS-LIST-HAS-MORE = 1
                       ADD 1 TO WS-LIST-PAGE
                       PERFORM LOAD-LIST-PAGE
                   END-IF
           END-EVALUATE.

       LOAD-LIST-PAGE.
           MOVE SPACES TO WS-LIST-ROWS
           MOVE 0 TO WS-LIST-COUNT
           MOVE 0 TO WS-LIST-HAS-MORE

           MOVE LOW-VALUES TO ACCT-NO
           START ACCT-FILE KEY IS GREATER THAN ACCT-NO
               INVALID KEY
                   MOVE "No accounts found."
                       TO WS-PAGE-INFO
                   GO TO LOAD-LIST-PAGE-EXIT
           END-START

      *    Skip to current page
           COMPUTE WS-LIST-SKIP =
               (WS-LIST-PAGE - 1) * 15
           PERFORM SKIP-LIST-RECORD
               WS-LIST-SKIP TIMES

      *    Load up to 15 records
           PERFORM LOAD-ONE-LIST-RECORD
               UNTIL WS-LIST-COUNT >= 15

      *    Check if more records exist
           IF WS-ACCT-OK
               READ ACCT-FILE NEXT
                   AT END CONTINUE
                   NOT AT END
                       MOVE 1 TO WS-LIST-HAS-MORE
               END-READ
           END-IF

      *    Format page info
           MOVE SPACES TO WS-PAGE-INFO
           STRING "Page " WS-LIST-PAGE
               DELIMITED BY SIZE INTO WS-PAGE-INFO
           END-STRING.
       LOAD-LIST-PAGE-EXIT.
           EXIT.

       SKIP-LIST-RECORD.
           READ ACCT-FILE NEXT
               AT END CONTINUE
           END-READ.

       LOAD-ONE-LIST-RECORD.
           READ ACCT-FILE NEXT
               AT END
                   MOVE 15 TO WS-LIST-COUNT
                   GO TO LOAD-ONE-RECORD-EXIT
               NOT AT END
                   ADD 1 TO WS-LIST-COUNT
                   MOVE ACCT-BAL TO WS-DISP-BAL
                   EVALUATE ACCT-TYPE
                       WHEN "C"
                           MOVE "Checking" TO WS-DISP-TYPE-FULL
                       WHEN "S"
                           MOVE "Savings " TO WS-DISP-TYPE-FULL
                       WHEN OTHER
                           MOVE "Unknown " TO WS-DISP-TYPE-FULL
                   END-EVALUATE
                   EVALUATE ACCT-STATUS
                       WHEN "A"
                           MOVE "Active" TO WS-DISP-STATUS-FULL
                       WHEN "X"
                           MOVE "Closed" TO WS-DISP-STATUS-FULL
                       WHEN OTHER
                           MOVE "  ?   " TO WS-DISP-STATUS-FULL
                   END-EVALUATE
                   MOVE SPACES TO
                       WS-LIST-ROW(WS-LIST-COUNT)
                   STRING ACCT-NO "  "
                       ACCT-NAME(1:20) " "
                       WS-DISP-BAL " "
                       WS-DISP-TYPE-FULL " "
                       WS-DISP-STATUS-FULL
                       DELIMITED BY SIZE
                       INTO WS-LIST-ROW(WS-LIST-COUNT)
                   END-STRING
           END-READ.
       LOAD-ONE-RECORD-EXIT.
           EXIT.

      *================================================================*
      * CLOSE ACCOUNT                                                  *
      *================================================================*
       DO-CLOSE-ACCOUNT.
           MOVE SPACES TO WS-INP-ACCTNO
           DISPLAY LOOKUP-SCR
           ACCEPT LOOKUP-SCR
           IF WS-KEY-F3
               GO TO DO-CLOSE-ACCOUNT-EXIT
           END-IF
           IF WS-KEY-F12
               MOVE 1 TO WS-PROGRAM-DONE
               GO TO DO-CLOSE-ACCOUNT-EXIT
           END-IF

           MOVE WS-INP-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "CLOSE ACCOUNT - NOT FOUND"
                       TO WS-RESULT-TITLE
                   MOVE SPACES TO WS-RESULT-LINE1
                       WS-RESULT-LINE2 WS-RESULT-LINE3
                       WS-RESULT-LINE4 WS-RESULT-LINE5
                   STRING "Account " WS-INP-ACCTNO
                       " was not found."
                       DELIMITED BY SIZE
                       INTO WS-RESULT-LINE1
                   END-STRING
                   PERFORM SHOW-RESULT
                   GO TO DO-CLOSE-ACCOUNT-EXIT
           END-READ

           IF ACCT-IS-CLOSED
               MOVE "CLOSE ACCOUNT - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Account is already closed."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-CLOSE-ACCOUNT-EXIT
           END-IF

      *    Show confirm screen
           MOVE SPACES TO WS-RESULT-LINE1 WS-RESULT-LINE2
               WS-RESULT-LINE3
           STRING "Close account " ACCT-NO "?"
               DELIMITED BY SIZE INTO WS-RESULT-LINE1
           END-STRING
           STRING "Name: " ACCT-NAME
               DELIMITED BY SIZE INTO WS-RESULT-LINE2
           END-STRING
           MOVE ACCT-BAL TO WS-DISP-BAL
           STRING "Balance: " WS-DISP-BAL
               DELIMITED BY SIZE INTO WS-RESULT-LINE3
           END-STRING
           MOVE SPACES TO WS-CONFIRM
           DISPLAY CONFIRM-SCR
           ACCEPT CONFIRM-SCR
           IF WS-KEY-F3
               GO TO DO-CLOSE-ACCOUNT-EXIT
           END-IF

           MOVE FUNCTION UPPER-CASE(WS-CONFIRM)
               TO WS-CONFIRM
           IF WS-CONFIRM = "Y"
               MOVE "X" TO ACCT-STATUS
               REWRITE ACCT-REC
               IF WS-ACCT-OK
                   MOVE "ACCOUNT CLOSED"
                       TO WS-RESULT-TITLE
                   MOVE SPACES TO WS-RESULT-LINE1
                       WS-RESULT-LINE2 WS-RESULT-LINE3
                       WS-RESULT-LINE4 WS-RESULT-LINE5
                   STRING "Account " ACCT-NO
                       " has been closed."
                       DELIMITED BY SIZE
                       INTO WS-RESULT-LINE1
                   END-STRING
               ELSE
                   MOVE "CLOSE ACCOUNT - ERROR"
                       TO WS-RESULT-TITLE
                   MOVE SPACES TO WS-RESULT-LINE1
                       WS-RESULT-LINE2 WS-RESULT-LINE3
                       WS-RESULT-LINE4 WS-RESULT-LINE5
                   STRING "Could not close account. Status: "
                       WS-ACCT-STATUS
                       DELIMITED BY SIZE
                       INTO WS-RESULT-LINE1
                   END-STRING
               END-IF
               PERFORM SHOW-RESULT
           END-IF.
       DO-CLOSE-ACCOUNT-EXIT.
           EXIT.

      *================================================================*
      * DEPOSIT                                                        *
      *================================================================*
       DO-DEPOSIT.
           MOVE "DEPOSIT" TO WS-RESULT-TITLE
           MOVE SPACES TO WS-INP-ACCTNO WS-INP-AMOUNT
               WS-INP-DESC
           DISPLAY TXN-ENTRY-SCR
           ACCEPT TXN-ENTRY-SCR
           IF WS-KEY-F3
               GO TO DO-DEPOSIT-EXIT
           END-IF
           IF WS-KEY-F12
               MOVE 1 TO WS-PROGRAM-DONE
               GO TO DO-DEPOSIT-EXIT
           END-IF

           MOVE WS-INP-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "DEPOSIT - ERROR" TO WS-RESULT-TITLE
                   MOVE "Account not found."
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-DEPOSIT-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               MOVE "DEPOSIT - ERROR" TO WS-RESULT-TITLE
               MOVE "Account is not active."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-DEPOSIT-EXIT
           END-IF

           MOVE FUNCTION NUMVAL(WS-INP-AMOUNT)
               TO WS-TXN-AMT
           IF WS-TXN-AMT < 0.01
               MOVE "DEPOSIT - ERROR" TO WS-RESULT-TITLE
               MOVE "Amount must be at least $0.01."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-DEPOSIT-EXIT
           END-IF

      *    Update account balance
           MOVE 0 TO WS-SIZE-ERR-FLAG
           ADD WS-TXN-AMT TO ACCT-BAL
               ON SIZE ERROR
                   MOVE 1 TO WS-SIZE-ERR-FLAG
           END-ADD

           IF WS-SIZE-ERR-FLAG = 1
               MOVE "DEPOSIT - ERROR" TO WS-RESULT-TITLE
               MOVE "Balance overflow. Deposit too large."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-DEPOSIT-EXIT
           END-IF

           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               MOVE "DEPOSIT - ERROR" TO WS-RESULT-TITLE
               MOVE "Could not update account."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-DEPOSIT-EXIT
           END-IF

      *    Log the transaction
           PERFORM GET-CURRENT-DATETIME
           MOVE WS-NEXT-TXN-ID    TO TXN-ID
           MOVE ACCT-NO            TO TXN-ACCT-NO
           MOVE "D"                TO TXN-TYPE
           MOVE WS-TXN-AMT        TO TXN-AMOUNT
           MOVE WS-TODAY-DATE      TO TXN-DATE
           MOVE WS-NOW-TIME        TO TXN-TIME
           MOVE WS-INP-DESC        TO TXN-DESC
           MOVE "C"                TO TXN-STATUS
           MOVE ZEROS              TO TXN-XFER-ACCT

           WRITE TXN-REC
           IF WS-TXN-OK
               ADD 1 TO WS-NEXT-TXN-ID
           END-IF

           MOVE "DEPOSIT SUCCESSFUL" TO WS-RESULT-TITLE
           MOVE SPACES TO WS-RESULT-LINE1
               WS-RESULT-LINE2 WS-RESULT-LINE3
               WS-RESULT-LINE4 WS-RESULT-LINE5
           MOVE WS-TXN-AMT TO WS-DISP-AMT
           STRING "Amount:      " WS-DISP-AMT
               DELIMITED BY SIZE INTO WS-RESULT-LINE1
           END-STRING
           MOVE ACCT-BAL TO WS-DISP-BAL
           STRING "New Balance: " WS-DISP-BAL
               DELIMITED BY SIZE INTO WS-RESULT-LINE2
           END-STRING
           MOVE "Transaction logged."
               TO WS-RESULT-LINE3
           PERFORM SHOW-RESULT.
       DO-DEPOSIT-EXIT.
           EXIT.

      *================================================================*
      * WITHDRAWAL                                                     *
      *================================================================*
       DO-WITHDRAWAL.
           MOVE "WITHDRAWAL" TO WS-RESULT-TITLE
           MOVE SPACES TO WS-INP-ACCTNO WS-INP-AMOUNT
               WS-INP-DESC
           DISPLAY TXN-ENTRY-SCR
           ACCEPT TXN-ENTRY-SCR
           IF WS-KEY-F3
               GO TO DO-WITHDRAWAL-EXIT
           END-IF
           IF WS-KEY-F12
               MOVE 1 TO WS-PROGRAM-DONE
               GO TO DO-WITHDRAWAL-EXIT
           END-IF

           MOVE WS-INP-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "WITHDRAWAL - ERROR"
                       TO WS-RESULT-TITLE
                   MOVE "Account not found."
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-WITHDRAWAL-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               MOVE "WITHDRAWAL - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Account is not active."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-WITHDRAWAL-EXIT
           END-IF

           MOVE FUNCTION NUMVAL(WS-INP-AMOUNT)
               TO WS-TXN-AMT
           IF WS-TXN-AMT < 0.01
               MOVE "WITHDRAWAL - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Amount must be at least $0.01."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-WITHDRAWAL-EXIT
           END-IF

      *    Check sufficient funds
           IF WS-TXN-AMT > ACCT-BAL
               MOVE "WITHDRAWAL - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Insufficient funds. No overdraft."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-WITHDRAWAL-EXIT
           END-IF

      *    Update account balance
           SUBTRACT WS-TXN-AMT FROM ACCT-BAL
               ON SIZE ERROR
                   MOVE "WITHDRAWAL - ERROR"
                       TO WS-RESULT-TITLE
                   MOVE "Balance underflow."
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-WITHDRAWAL-EXIT
           END-SUBTRACT

           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               MOVE "WITHDRAWAL - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Could not update account."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-WITHDRAWAL-EXIT
           END-IF

      *    Log the transaction
           PERFORM GET-CURRENT-DATETIME
           MOVE WS-NEXT-TXN-ID    TO TXN-ID
           MOVE ACCT-NO            TO TXN-ACCT-NO
           MOVE "W"                TO TXN-TYPE
           MOVE WS-TXN-AMT        TO TXN-AMOUNT
           MOVE WS-TODAY-DATE      TO TXN-DATE
           MOVE WS-NOW-TIME        TO TXN-TIME
           MOVE WS-INP-DESC        TO TXN-DESC
           MOVE "C"                TO TXN-STATUS
           MOVE ZEROS              TO TXN-XFER-ACCT

           WRITE TXN-REC
           IF WS-TXN-OK
               ADD 1 TO WS-NEXT-TXN-ID
           END-IF

           MOVE "WITHDRAWAL SUCCESSFUL"
               TO WS-RESULT-TITLE
           MOVE SPACES TO WS-RESULT-LINE1
               WS-RESULT-LINE2 WS-RESULT-LINE3
               WS-RESULT-LINE4 WS-RESULT-LINE5
           MOVE WS-TXN-AMT TO WS-DISP-AMT
           STRING "Amount:      " WS-DISP-AMT
               DELIMITED BY SIZE INTO WS-RESULT-LINE1
           END-STRING
           MOVE ACCT-BAL TO WS-DISP-BAL
           STRING "New Balance: " WS-DISP-BAL
               DELIMITED BY SIZE INTO WS-RESULT-LINE2
           END-STRING
           MOVE "Transaction logged."
               TO WS-RESULT-LINE3
           PERFORM SHOW-RESULT.
       DO-WITHDRAWAL-EXIT.
           EXIT.

      *================================================================*
      * TRANSFER                                                       *
      *================================================================*
       DO-TRANSFER.
           MOVE SPACES TO WS-INP-ACCTNO WS-INP-XFER-ACCTNO
               WS-INP-AMOUNT
           DISPLAY XFER-ENTRY-SCR
           ACCEPT XFER-ENTRY-SCR
           IF WS-KEY-F3
               GO TO DO-TRANSFER-EXIT
           END-IF
           IF WS-KEY-F12
               MOVE 1 TO WS-PROGRAM-DONE
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Read source account
           MOVE WS-INP-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "TRANSFER - ERROR"
                       TO WS-RESULT-TITLE
                   MOVE "Source account not found."
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-TRANSFER-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Source account is not active."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Save source account data
           MOVE ACCT-NO     TO WS-SAVE-ACCT-NO
           MOVE ACCT-NAME   TO WS-SAVE-ACCT-NAME
           MOVE ACCT-BAL    TO WS-SAVE-ACCT-BAL
           MOVE ACCT-TYPE   TO WS-SAVE-ACCT-TYPE
           MOVE ACCT-STATUS TO WS-SAVE-ACCT-STAT
           MOVE ACCT-OPEN-DT TO WS-SAVE-ACCT-DT

      *    Validate same-account transfer
           IF WS-INP-XFER-ACCTNO = WS-INP-ACCTNO
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Cannot transfer to the same account."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Read target account
           MOVE WS-INP-XFER-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "TRANSFER - ERROR"
                       TO WS-RESULT-TITLE
                   MOVE "Target account not found."
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-TRANSFER-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Target account is not active."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Validate amount
           MOVE FUNCTION NUMVAL(WS-INP-AMOUNT)
               TO WS-TXN-AMT
           IF WS-TXN-AMT < 0.01
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Amount must be at least $0.01."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Check sufficient funds in source
           IF WS-TXN-AMT > WS-SAVE-ACCT-BAL
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Insufficient funds in source."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Update source account (withdraw)
           MOVE WS-SAVE-ACCT-NO   TO ACCT-NO
           MOVE WS-SAVE-ACCT-NAME TO ACCT-NAME
           SUBTRACT WS-TXN-AMT FROM WS-SAVE-ACCT-BAL
               GIVING ACCT-BAL
           MOVE WS-SAVE-ACCT-TYPE TO ACCT-TYPE
           MOVE WS-SAVE-ACCT-STAT TO ACCT-STATUS
           MOVE WS-SAVE-ACCT-DT   TO ACCT-OPEN-DT
           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Could not update source account."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Update target account (deposit)
           MOVE WS-INP-XFER-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE "TRANSFER - CRITICAL"
                       TO WS-RESULT-TITLE
                   MOVE "Target account vanished!"
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-TRANSFER-EXIT
           END-READ
           ADD WS-TXN-AMT TO ACCT-BAL
               ON SIZE ERROR
                   MOVE "TRANSFER - ERROR"
                       TO WS-RESULT-TITLE
                   MOVE "Target balance overflow."
                       TO WS-RESULT-LINE1
                   MOVE SPACES TO WS-RESULT-LINE2
                       WS-RESULT-LINE3 WS-RESULT-LINE4
                       WS-RESULT-LINE5
                   PERFORM SHOW-RESULT
                   GO TO DO-TRANSFER-EXIT
           END-ADD
           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               MOVE "TRANSFER - ERROR"
                   TO WS-RESULT-TITLE
               MOVE "Could not update target account."
                   TO WS-RESULT-LINE1
               MOVE SPACES TO WS-RESULT-LINE2
                   WS-RESULT-LINE3 WS-RESULT-LINE4
                   WS-RESULT-LINE5
               PERFORM SHOW-RESULT
               GO TO DO-TRANSFER-EXIT
           END-IF

      *    Log the transfer transaction
           PERFORM GET-CURRENT-DATETIME
           MOVE WS-NEXT-TXN-ID         TO TXN-ID
           MOVE WS-SAVE-ACCT-NO        TO TXN-ACCT-NO
           MOVE "T"                     TO TXN-TYPE
           MOVE WS-TXN-AMT             TO TXN-AMOUNT
           MOVE WS-TODAY-DATE           TO TXN-DATE
           MOVE WS-NOW-TIME             TO TXN-TIME
           MOVE "TRANSFER"              TO TXN-DESC
           MOVE "C"                     TO TXN-STATUS
           MOVE WS-INP-XFER-ACCTNO     TO TXN-XFER-ACCT

           WRITE TXN-REC
           IF WS-TXN-OK
               ADD 1 TO WS-NEXT-TXN-ID
           END-IF

      *    Show success with final balances
           MOVE "TRANSFER SUCCESSFUL"
               TO WS-RESULT-TITLE
           MOVE SPACES TO WS-RESULT-LINE1
               WS-RESULT-LINE2 WS-RESULT-LINE3
               WS-RESULT-LINE4 WS-RESULT-LINE5
           MOVE WS-TXN-AMT TO WS-DISP-AMT
           STRING "Amount: " WS-DISP-AMT
               DELIMITED BY SIZE INTO WS-RESULT-LINE1
           END-STRING

      *    Re-read source for final balance
           MOVE WS-SAVE-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY CONTINUE
           END-READ
           MOVE ACCT-BAL TO WS-DISP-BAL
           STRING "From " WS-SAVE-ACCT-NO
               " balance: " WS-DISP-BAL
               DELIMITED BY SIZE INTO WS-RESULT-LINE2
           END-STRING

      *    Re-read target for final balance
           MOVE WS-INP-XFER-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY CONTINUE
           END-READ
           MOVE ACCT-BAL TO WS-DISP-BAL
           STRING "To   " WS-INP-XFER-ACCTNO
               " balance: " WS-DISP-BAL
               DELIMITED BY SIZE INTO WS-RESULT-LINE3
           END-STRING
           MOVE "Transaction logged."
               TO WS-RESULT-LINE4
           PERFORM SHOW-RESULT.
       DO-TRANSFER-EXIT.
           EXIT.
