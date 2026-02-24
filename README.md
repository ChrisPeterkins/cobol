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
│   ├── bankui.cbl        # TUI frontend (SCREEN SECTION)
│   ├── dayend.cbl         # End-of-day reconciliation (batch)
│   ├── rptgen.cbl         # Report generator (batch)
│   └── seedload.cbl       # Seed data loader
├── tests/                # Automated tests
│   └── test-suite.sh     # Integration test suite (~53 tests)
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

### BANKUI — Terminal User Interface
Green-screen TUI that combines account management and transaction processing into a single application using COBOL's `SCREEN SECTION`:
- **Main Menu**: 7 operations in a box-drawn layout (create/lookup/list/close + deposit/withdraw/transfer)
- **Function Keys**: F3=Back, F7/F8=Page, F12=Quit
- **Color Theme**: Green-on-black with cyan input fields, blue status bar

```bash
./bin/bankui
```

Requires a terminal with ncurses support (any modern terminal emulator works).

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
| SCREEN SECTION | BANKUI |
| SPECIAL-NAMES / CRT STATUS | BANKUI |
| FOREGROUND-COLOR / BACKGROUND-COLOR | BANKUI |
| USING / FROM (screen binding) | BANKUI |
| EVALUATE (case/switch) | All programs |
| STRING/UNSTRING | Date formatting |
| FILE STATUS codes | All programs |
| PERFORM UNTIL | All programs |

## Makefile Targets

```bash
make all        # Build all programs
make test       # Build and run integration tests
make clean      # Remove compiled binaries
make cleandata  # Remove data files
make reset      # Remove both binaries and data
```

## Data Files

- **ACCOUNTS.dat**: Indexed file (ORGANIZATION IS INDEXED). 59-byte fixed-width records keyed on 8-digit account number.
- **TRANSACTIONS.dat**: Sequential file. 71-byte fixed-width records appended chronologically.

Both files are created automatically when first needed.

## Testing

The project includes an automated integration test suite that validates the full workflow:

```bash
# Run all tests
make test

# Or run directly
./tests/test-suite.sh
```

The test suite covers 6 phases with ~53 test cases:

| Phase | What's Verified |
|---|---|
| 0. Build | `make clean && make all`, all 6 binaries exist |
| 1. Seed Data | seedload creates 5 accounts, acctmgr list confirms |
| 2. Account Mgmt | Create, lookup, lookup nonexistent, list count |
| 3. Transactions | Deposit, withdraw, overdraft rejection, transfer, closed account rejection |
| 4. Day-End | Reconciliation passes, daily summary, no discrepancies |
| 5. Report Gen | Statement header/footer, account name, transaction descriptions |
| 6. Closure | Close account, verify closed, re-close rejected |

Tests use the `printf | ./bin/program` pattern to drive interactive programs with piped input. Output is color-coded (green=pass, red=fail).
