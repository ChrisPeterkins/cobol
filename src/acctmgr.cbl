       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGR.
      *================================================================*
      * ACCTMGR - Account Manager                                      *
      * Interactive menu-driven program for managing bank accounts.     *
      * Operations: CREATE, LOOKUP, LIST, CLOSE                        *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE
               ASSIGN TO "data/ACCOUNTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY ACCT-REC.

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS        PIC XX.
           88  WS-ACCT-OK            VALUE "00".
           88  WS-ACCT-DUP-KEY       VALUE "22".
           88  WS-ACCT-NOT-FOUND     VALUE "23".
           88  WS-ACCT-EOF           VALUE "10".
           88  WS-ACCT-FILE-MISSING  VALUE "35".

       01  WS-FILE-OPEN-FLAG    PIC 9 VALUE 0.
           88  WS-FILE-IS-OPEN      VALUE 1.

       01  WS-MENU-CHOICE       PIC X(1).
           88  WS-CREATE            VALUE "1".
           88  WS-LOOKUP            VALUE "2".
           88  WS-LIST              VALUE "3".
           88  WS-CLOSE-ACCT        VALUE "4".
           88  WS-QUIT              VALUE "Q" "q".

       01  WS-INPUT-NAME         PIC X(30).
       01  WS-INPUT-TYPE         PIC X(1).
       01  WS-INPUT-DEPOSIT      PIC X(12).
       01  WS-INPUT-ACCTNO       PIC X(8).
       01  WS-DEPOSIT-AMT        PIC 9(9)V99.
       01  WS-CONFIRM            PIC X(1).

       01  WS-NEXT-ACCT-NO       PIC 9(8) VALUE 10000001.
       01  WS-ACCT-COUNT         PIC 9(5) VALUE 0.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURR-YEAR      PIC 9(4).
           05  WS-CURR-MONTH     PIC 9(2).
           05  WS-CURR-DAY       PIC 9(2).
           05  WS-CURR-TIME      PIC X(8).

       01  WS-TODAY-YYYYMMDD     PIC 9(8).

       01  WS-DISP-BAL           PIC $$$,$$$,$$9.99-.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM OPEN-ACCT-FILE
           IF NOT WS-FILE-IS-OPEN
               DISPLAY "FATAL: Cannot open account file."
               STOP RUN
           END-IF
           PERFORM FIND-NEXT-ACCT-NO
           PERFORM MAIN-MENU UNTIL WS-QUIT
           PERFORM CLOSE-ACCT-FILE
           STOP RUN.

       OPEN-ACCT-FILE.
           OPEN I-O ACCT-FILE
           IF WS-ACCT-OK
               SET WS-FILE-IS-OPEN TO TRUE
           ELSE
               IF WS-ACCT-FILE-MISSING
                   OPEN OUTPUT ACCT-FILE
                   IF WS-ACCT-OK
                       CLOSE ACCT-FILE
                       OPEN I-O ACCT-FILE
                       IF WS-ACCT-OK
                           SET WS-FILE-IS-OPEN TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF.

       CLOSE-ACCT-FILE.
           IF WS-FILE-IS-OPEN
               CLOSE ACCT-FILE
               MOVE 0 TO WS-FILE-OPEN-FLAG
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

       MAIN-MENU.
           DISPLAY SPACES
           DISPLAY "========================================="
           DISPLAY "    COBOL BANK LEDGER - ACCOUNT MANAGER"
           DISPLAY "========================================="
           DISPLAY "  1. Create New Account"
           DISPLAY "  2. Lookup Account"
           DISPLAY "  3. List All Accounts"
           DISPLAY "  4. Close Account"
           DISPLAY "  Q. Quit"
           DISPLAY "========================================="
           DISPLAY "Enter choice: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           EVALUATE TRUE
               WHEN WS-CREATE
                   PERFORM CREATE-ACCOUNT
               WHEN WS-LOOKUP
                   PERFORM LOOKUP-ACCOUNT
               WHEN WS-LIST
                   PERFORM LIST-ACCOUNTS
               WHEN WS-CLOSE-ACCT
                   PERFORM CLOSE-ACCOUNT
               WHEN WS-QUIT
                   DISPLAY "Goodbye."
               WHEN OTHER
                   DISPLAY "Invalid choice. Try again."
           END-EVALUATE.

       GET-TODAY-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-TODAY-YYYYMMDD
           END-STRING.

       CREATE-ACCOUNT.
           DISPLAY SPACES
           DISPLAY "--- Create New Account ---"
           DISPLAY "Account holder name (up to 30 chars):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-NAME
           IF WS-INPUT-NAME = SPACES
               DISPLAY "Name cannot be empty."
               GO TO CREATE-ACCOUNT-EXIT
           END-IF

           DISPLAY "Account type (C=Checking, S=Savings):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-TYPE
           MOVE FUNCTION UPPER-CASE(WS-INPUT-TYPE)
               TO WS-INPUT-TYPE
           IF WS-INPUT-TYPE NOT = "C" AND "S"
               DISPLAY "Invalid type. Must be C or S."
               GO TO CREATE-ACCOUNT-EXIT
           END-IF

           DISPLAY "Opening deposit amount (e.g. 1000.00):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-DEPOSIT
           MOVE FUNCTION NUMVAL(WS-INPUT-DEPOSIT)
               TO WS-DEPOSIT-AMT
           IF WS-DEPOSIT-AMT < 0.01
               DISPLAY "Deposit must be at least $0.01."
               GO TO CREATE-ACCOUNT-EXIT
           END-IF

           MOVE WS-NEXT-ACCT-NO   TO ACCT-NO
           MOVE WS-INPUT-NAME     TO ACCT-NAME
           MOVE WS-DEPOSIT-AMT    TO ACCT-BAL
           MOVE WS-INPUT-TYPE     TO ACCT-TYPE
           MOVE "A"               TO ACCT-STATUS
           PERFORM GET-TODAY-DATE
           MOVE WS-TODAY-YYYYMMDD TO ACCT-OPEN-DT

           WRITE ACCT-REC
           IF WS-ACCT-OK
               DISPLAY SPACES
               DISPLAY "Account created successfully!"
               DISPLAY "  Account Number: " ACCT-NO
               DISPLAY "  Name:           " ACCT-NAME
               MOVE ACCT-BAL TO WS-DISP-BAL
               DISPLAY "  Balance:        " WS-DISP-BAL
               DISPLAY "  Type:           " ACCT-TYPE
               ADD 1 TO WS-NEXT-ACCT-NO
           ELSE
               DISPLAY "ERROR: Could not create account."
               DISPLAY "  File status: " WS-ACCT-STATUS
           END-IF.
       CREATE-ACCOUNT-EXIT.
           EXIT.

       LOOKUP-ACCOUNT.
           DISPLAY SPACES
           DISPLAY "--- Account Lookup ---"
           DISPLAY "Enter account number (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCTNO
           MOVE WS-INPUT-ACCTNO TO ACCT-NO

           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Account " ACCT-NO " not found."
                   GO TO LOOKUP-ACCOUNT-EXIT
           END-READ

           PERFORM DISPLAY-ACCOUNT-DETAIL.
       LOOKUP-ACCOUNT-EXIT.
           EXIT.

       DISPLAY-ACCOUNT-DETAIL.
           DISPLAY SPACES
           DISPLAY "  Account No:   " ACCT-NO
           DISPLAY "  Name:         " ACCT-NAME
           MOVE ACCT-BAL TO WS-DISP-BAL
           DISPLAY "  Balance:      " WS-DISP-BAL
           EVALUATE ACCT-TYPE
               WHEN "C"
                   DISPLAY "  Type:         Checking"
               WHEN "S"
                   DISPLAY "  Type:         Savings"
               WHEN OTHER
                   DISPLAY "  Type:         Unknown (" ACCT-TYPE
                       ")"
           END-EVALUATE
           EVALUATE ACCT-STATUS
               WHEN "A"
                   DISPLAY "  Status:       Active"
               WHEN "X"
                   DISPLAY "  Status:       Closed"
               WHEN OTHER
                   DISPLAY "  Status:       Unknown ("
                       ACCT-STATUS ")"
           END-EVALUATE
           DISPLAY "  Opened:       " ACCT-OPEN-DT.

       LIST-ACCOUNTS.
           DISPLAY SPACES
           DISPLAY "--- All Accounts ---"
           DISPLAY "Acct No   Name"
               "                       Balance     Type  Status"
           DISPLAY "--------  --------"
               "---------------------------------  ----  ------"
           MOVE 0 TO WS-ACCT-COUNT

           MOVE LOW-VALUES TO ACCT-NO
           START ACCT-FILE KEY IS GREATER THAN ACCT-NO
               INVALID KEY
                   DISPLAY "(No accounts found)"
                   GO TO LIST-ACCOUNTS-EXIT
           END-START

           PERFORM READ-NEXT-ACCOUNT
               UNTIL NOT WS-ACCT-OK

           DISPLAY "--------  --------"
               "---------------------------------  ----  ------"
           DISPLAY "Total accounts: " WS-ACCT-COUNT.
       LIST-ACCOUNTS-EXIT.
           EXIT.

       READ-NEXT-ACCOUNT.
           READ ACCT-FILE NEXT
               AT END
                   CONTINUE
               NOT AT END
                   ADD 1 TO WS-ACCT-COUNT
                   MOVE ACCT-BAL TO WS-DISP-BAL
                   DISPLAY ACCT-NO "  " ACCT-NAME "  "
                       WS-DISP-BAL "  " ACCT-TYPE "     "
                       ACCT-STATUS
           END-READ.

       CLOSE-ACCOUNT.
           DISPLAY SPACES
           DISPLAY "--- Close Account ---"
           DISPLAY "Enter account number to close (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCTNO
           MOVE WS-INPUT-ACCTNO TO ACCT-NO

           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Account " ACCT-NO " not found."
                   GO TO CLOSE-ACCOUNT-EXIT
           END-READ

           IF ACCT-IS-CLOSED
               DISPLAY "Account is already closed."
               GO TO CLOSE-ACCOUNT-EXIT
           END-IF

           PERFORM DISPLAY-ACCOUNT-DETAIL
           DISPLAY SPACES
           DISPLAY "Close this account? (Y/N): "
               WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           MOVE FUNCTION UPPER-CASE(WS-CONFIRM) TO WS-CONFIRM
           IF WS-CONFIRM = "Y"
               MOVE "X" TO ACCT-STATUS
               REWRITE ACCT-REC
               IF WS-ACCT-OK
                   DISPLAY "Account " ACCT-NO " has been closed."
               ELSE
                   DISPLAY "ERROR: Could not close account."
                   DISPLAY "  File status: " WS-ACCT-STATUS
               END-IF
           ELSE
               DISPLAY "Close cancelled."
           END-IF.
       CLOSE-ACCOUNT-EXIT.
           EXIT.
