      *================================================================*
      * ACCT-REC.cpy - Account Master Record Layout                    *
      * Record Length: 59 bytes                                        *
      *================================================================*
       01  ACCT-REC.
           05  ACCT-NO            PIC 9(8).
           05  ACCT-NAME          PIC X(30).
           05  ACCT-BAL           PIC S9(9)V99.
           05  ACCT-TYPE          PIC X(1).
               88  ACCT-IS-CHECKING   VALUE "C".
               88  ACCT-IS-SAVINGS    VALUE "S".
           05  ACCT-STATUS        PIC X(1).
               88  ACCT-IS-ACTIVE     VALUE "A".
               88  ACCT-IS-CLOSED     VALUE "X".
           05  ACCT-OPEN-DT       PIC 9(8).
