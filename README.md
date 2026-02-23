# COBOL Bank Ledger

A batch-processing bank ledger system written in COBOL — built as a learning project for experienced developers exploring mainframe-era patterns.

## Prerequisites

- **GnuCOBOL 3.x**: `brew install gnucobol` (macOS)
- Verify: `cobc --version`

## Quick Start

```bash
# Build everything
make all

# Load seed test accounts (5 accounts)
./bin/seedload

# Manage accounts (create, lookup, list, close)
./bin/acctmgr

# Process transactions (deposit, withdraw, transfer)
./bin/txnproc

# Run end-of-day reconciliation
./bin/dayend

# Generate an account statement
./bin/rptgen
```

## Project Structure

```
Cobol/
├── Makefile              # Build system
├── copybooks/            # Shared record layouts (like C headers)
│   ├── ACCT-REC.cpy      # Account record: 59 bytes
│   └── TXNL-REC.cpy      # Transaction record: 71 bytes
├── src/                  # COBOL source programs
│   ├── acctmgr.cbl       # Account manager (interactive)
│   ├── txnproc.cbl       # Transaction processor (interactive)
│   ├── dayend.cbl         # End-of-day reconciliation (batch)
│   ├── rptgen.cbl         # Report generator (batch)
│   └── seedload.cbl       # Seed data loader
├── data/                 # Runtime data files (generated)
│   ├── ACCOUNTS.dat       # Indexed account master file
│   └── TRANSACTIONS.dat   # Sequential transaction log
└── bin/                  # Compiled executables (generated)
```

## Programs

### ACCTMGR — Account Manager
Interactive menu-driven program for managing accounts:
- **Create**: Enter name, type (C/S), and opening deposit. Auto-assigns account number.
- **Lookup**: Search by 8-digit account number.
- **List**: Display all accounts with balances.
- **Close**: Mark an account as closed.

### TXNPROC — Transaction Processor
Interactive program for financial operations:
- **Deposit**: Add funds to an active account.
- **Withdraw**: Remove funds (no overdraft allowed).
- **Transfer**: Move funds between two active accounts.

All transactions are logged to `TRANSACTIONS.dat` and account balances are updated in real time.

### DAYEND — End-of-Day Reconciliation
Batch program that:
1. Reads all completed transactions for a given date
2. Sorts them by account number (using COBOL SORT)
3. Summarizes deposits, withdrawals, and transfers per account
4. Reports any discrepancies
5. Prints a daily summary

### RPTGEN — Report Generator
Generates formatted account statements:
- Accepts account number and date range
- Shows opening balance, each transaction with running balance, and closing balance
- Formatted with PIC editing for currency display

## Demo Workflow

```bash
# 1. Build
make all

# 2. Seed accounts
./bin/seedload

# 3. Process some transactions
./bin/txnproc
#   → Deposit $1,000 to account 10000001
#   → Withdraw $200 from account 10000003
#   → Transfer $500 from 10000002 to 10000001

# 4. Run reconciliation
./bin/dayend
#   → Press Enter for today's date

# 5. Generate statement
./bin/rptgen
#   → Enter account 10000001
#   → Press Enter for all dates
```

## COBOL Concepts Demonstrated

| Concept | Where |
|---|---|
| Indexed file I/O (ISAM) | ACCTMGR, TXNPROC |
| Sequential file I/O | TXNPROC (transaction log) |
| COPY statement (copybooks) | All programs |
| Level-88 conditions | Copybooks, all programs |
| Fixed-point decimal math | TXNPROC |
| ON SIZE ERROR | TXNPROC |
| SORT verb with INPUT/OUTPUT PROCEDURE | DAYEND |
| Control break processing | DAYEND |
| PIC editing (Z, $, -, etc.) | RPTGEN |
| ACCEPT/DISPLAY | ACCTMGR, TXNPROC |
| EVALUATE (case/switch) | All programs |
| STRING/UNSTRING | Date formatting |
| FILE STATUS codes | All programs |
| PERFORM UNTIL | All programs |

## Makefile Targets

```bash
make all        # Build all programs
make clean      # Remove compiled binaries
make cleandata  # Remove data files
make reset      # Remove both binaries and data
```

## Data Files

- **ACCOUNTS.dat**: Indexed file (ORGANIZATION IS INDEXED). 59-byte fixed-width records keyed on 8-digit account number.
- **TRANSACTIONS.dat**: Sequential file. 71-byte fixed-width records appended chronologically.

Both files are created automatically when first needed.
