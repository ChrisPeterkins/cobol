      *================================================================*
      * TXNL-REC.cpy - Transaction Log Record Layout                   *
      * Record Length: 71 bytes                                        *
      *================================================================*
       01  TXN-REC.
           05  TXN-ID             PIC 9(10).
           05  TXN-ACCT-NO        PIC 9(8).
           05  TXN-TYPE           PIC X(1).
               88  TXN-IS-DEPOSIT     VALUE "D".
               88  TXN-IS-WITHDRAWAL  VALUE "W".
               88  TXN-IS-TRANSFER    VALUE "T".
           05  TXN-AMOUNT         PIC 9(7)V99.
           05  TXN-DATE           PIC 9(8).
           05  TXN-TIME           PIC 9(6).
           05  TXN-DESC           PIC X(20).
           05  TXN-STATUS         PIC X(1).
               88  TXN-IS-PENDING     VALUE "P".
               88  TXN-IS-COMPLETE    VALUE "C".
               88  TXN-IS-FAILED      VALUE "F".
           05  TXN-XFER-ACCT     PIC 9(8).
