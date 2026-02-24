#!/usr/bin/env bash
#================================================================
# COBOL Bank Ledger — Integration Test Suite
# Self-contained bash tests using printf | ./bin/program pattern.
# Color-coded output: GREEN=pass, RED=fail.
#================================================================

set -euo pipefail

# --- Configuration ---
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BIN="$PROJECT_DIR/bin"
DATA="$PROJECT_DIR/data"

cd "$PROJECT_DIR"

# --- Colors ---
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# --- Counters ---
PASS=0
FAIL=0
TOTAL=0

# --- Test Helpers ---
assert_contains() {
    local output="$1"
    local expected="$2"
    local name="$3"
    TOTAL=$((TOTAL + 1))
    if echo "$output" | grep -q "$expected"; then
        echo -e "  ${GREEN}PASS${NC} $name"
        PASS=$((PASS + 1))
    else
        echo -e "  ${RED}FAIL${NC} $name"
        echo -e "       Expected to find: ${YELLOW}$expected${NC}"
        FAIL=$((FAIL + 1))
    fi
}

assert_not_contains() {
    local output="$1"
    local unexpected="$2"
    local name="$3"
    TOTAL=$((TOTAL + 1))
    if echo "$output" | grep -q "$unexpected"; then
        echo -e "  ${RED}FAIL${NC} $name"
        echo -e "       Did not expect: ${YELLOW}$unexpected${NC}"
        FAIL=$((FAIL + 1))
    else
        echo -e "  ${GREEN}PASS${NC} $name"
        PASS=$((PASS + 1))
    fi
}

assert_exit_zero() {
    local exit_code="$1"
    local name="$2"
    TOTAL=$((TOTAL + 1))
    if [ "$exit_code" -eq 0 ]; then
        echo -e "  ${GREEN}PASS${NC} $name"
        PASS=$((PASS + 1))
    else
        echo -e "  ${RED}FAIL${NC} $name (exit code: $exit_code)"
        FAIL=$((FAIL + 1))
    fi
}

assert_file_exists() {
    local filepath="$1"
    local name="$2"
    TOTAL=$((TOTAL + 1))
    if [ -f "$filepath" ]; then
        echo -e "  ${GREEN}PASS${NC} $name"
        PASS=$((PASS + 1))
    else
        echo -e "  ${RED}FAIL${NC} $name (file not found: $filepath)"
        FAIL=$((FAIL + 1))
    fi
}

phase_header() {
    echo ""
    echo -e "${CYAN}${BOLD}━━━ Phase $1: $2 ━━━${NC}"
}

#================================================================
# Phase 0: Build
#================================================================
phase_header 0 "Build"

# Clean everything first
make clean 2>/dev/null || true
rm -f "$DATA"/*.dat

# Build all programs
BUILD_OUTPUT=$(make all 2>&1)
BUILD_RC=$?
assert_exit_zero "$BUILD_RC" "make all succeeds"

# Check all binaries exist
assert_file_exists "$BIN/acctmgr"  "acctmgr binary exists"
assert_file_exists "$BIN/txnproc"  "txnproc binary exists"
assert_file_exists "$BIN/dayend"   "dayend binary exists"
assert_file_exists "$BIN/rptgen"   "rptgen binary exists"
assert_file_exists "$BIN/seedload" "seedload binary exists"
assert_file_exists "$BIN/bankui"   "bankui binary exists"

#================================================================
# Phase 1: Seed Data
#================================================================
phase_header 1 "Seed Data"

# Run seedload
SEED_OUTPUT=$("$BIN/seedload" 2>&1)
SEED_RC=$?
assert_exit_zero "$SEED_RC" "seedload runs successfully"
assert_contains "$SEED_OUTPUT" "Seed data loaded successfully" "seedload reports success"
assert_contains "$SEED_OUTPUT" "Alice Johnson" "seedload shows Alice"
assert_contains "$SEED_OUTPUT" "Bob Smith" "seedload shows Bob"
assert_contains "$SEED_OUTPUT" "Carol Williams" "seedload shows Carol"
assert_contains "$SEED_OUTPUT" "David Chen" "seedload shows David"
assert_contains "$SEED_OUTPUT" "Eve Martinez" "seedload shows Eve"

# Verify via acctmgr list
LIST_OUTPUT=$(printf "3\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$LIST_OUTPUT" "Total accounts: 00005" "acctmgr confirms 5 accounts"
assert_contains "$LIST_OUTPUT" "Alice Johnson" "acctmgr list shows Alice"

#================================================================
# Phase 2: Account Management
#================================================================
phase_header 2 "Account Management"

# Create a new account
CREATE_OUTPUT=$(printf "1\nTest User\nC\n2500.00\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$CREATE_OUTPUT" "Account created successfully" "create account succeeds"
assert_contains "$CREATE_OUTPUT" "Test User" "create shows account name"
assert_contains "$CREATE_OUTPUT" "10000006" "create assigns next account number"

# Lookup the new account
LOOKUP_OUTPUT=$(printf "2\n10000006\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$LOOKUP_OUTPUT" "Test User" "lookup finds new account"
assert_contains "$LOOKUP_OUTPUT" "Active" "lookup shows active status"

# Lookup nonexistent account
NOFIND_OUTPUT=$(printf "2\n99999999\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$NOFIND_OUTPUT" "not found" "lookup reports not found"

# List shows 6 accounts now
LIST2_OUTPUT=$(printf "3\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$LIST2_OUTPUT" "Total accounts: 00006" "list shows 6 accounts"

# Lookup seed account by number
SEED_LOOKUP=$(printf "2\n10000001\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$SEED_LOOKUP" "Alice Johnson" "lookup finds Alice by number"
assert_contains "$SEED_LOOKUP" "Checking" "Alice has Checking type"

# Lookup savings account
SAV_LOOKUP=$(printf "2\n10000002\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$SAV_LOOKUP" "Bob Smith" "lookup finds Bob"
assert_contains "$SAV_LOOKUP" "Savings" "Bob has Savings type"

#================================================================
# Phase 3: Transactions
#================================================================
phase_header 3 "Transactions"

# Deposit to Alice
DEP_OUTPUT=$(printf "1\n10000001\n1000.00\nPayroll deposit\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$DEP_OUTPUT" "Deposit successful" "deposit succeeds"
assert_contains "$DEP_OUTPUT" "1,000.00" "deposit shows amount"

# Verify new balance via acctmgr lookup
BAL_OUTPUT=$(printf "2\n10000001\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$BAL_OUTPUT" "6,000.00" "Alice balance is 6000 after deposit"

# Withdraw from Carol
WD_OUTPUT=$(printf "2\n10000003\n200.00\nATM withdrawal\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$WD_OUTPUT" "Withdrawal successful" "withdrawal succeeds"

# Verify Carol's balance
CAROL_BAL=$(printf "2\n10000003\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$CAROL_BAL" "650.00" "Carol balance is 650 after withdrawal"

# Overdraft rejection - Carol has 650, try to withdraw 1000
OD_OUTPUT=$(printf "2\n10000003\n1000.00\nBig purchase\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$OD_OUTPUT" "Insufficient funds" "overdraft rejected"

# Verify Carol's balance unchanged
CAROL_BAL2=$(printf "2\n10000003\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$CAROL_BAL2" "650.00" "Carol balance unchanged after overdraft"

# Transfer from Bob to Alice
XFER_OUTPUT=$(printf "3\n10000002\n10000001\n500.00\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$XFER_OUTPUT" "Transfer successful" "transfer succeeds"

# Verify Bob's new balance (12500 - 500 = 12000)
BOB_BAL=$(printf "2\n10000002\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$BOB_BAL" "12,000.00" "Bob balance after transfer out"

# Verify Alice's new balance (6000 + 500 = 6500)
ALICE_BAL=$(printf "2\n10000001\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$ALICE_BAL" "6,500.00" "Alice balance after transfer in"

# Deposit to closed account (Eve, 10000005)
CLOSED_DEP=$(printf "1\n10000005\n100.00\nTest\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$CLOSED_DEP" "not active" "deposit to closed account rejected"

# Withdraw from closed account
CLOSED_WD=$(printf "2\n10000005\n100.00\nTest\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$CLOSED_WD" "not active" "withdrawal from closed account rejected"

# Transfer from nonexistent account
BAD_XFER=$(printf "3\n99999999\n10000001\n100.00\nQ\n" | "$BIN/txnproc" 2>&1)
assert_contains "$BAD_XFER" "not found" "transfer from nonexistent rejected"

#================================================================
# Phase 4: Day-End Reconciliation
#================================================================
phase_header 4 "Day-End Reconciliation"

# Get today's date in YYYYMMDD format
TODAY=$(date +%Y%m%d)

# Run day-end for today
DAYEND_OUTPUT=$(printf "\n" | "$BIN/dayend" 2>&1)
DAYEND_RC=$?
assert_exit_zero "$DAYEND_RC" "dayend runs successfully"
assert_contains "$DAYEND_OUTPUT" "DAILY SUMMARY" "dayend produces daily summary"
assert_contains "$DAYEND_OUTPUT" "Reconciliation: PASS" "reconciliation passes"
assert_not_contains "$DAYEND_OUTPUT" "RECONCILIATION ISSUES" "no discrepancies found"

#================================================================
# Phase 5: Report Generation
#================================================================
phase_header 5 "Report Generation"

# Generate statement for Alice (all dates)
RPT_OUTPUT=$(printf "10000001\n\n\n" | "$BIN/rptgen" 2>&1)
RPT_RC=$?
assert_exit_zero "$RPT_RC" "rptgen runs successfully"
assert_contains "$RPT_OUTPUT" "ACCOUNT STATEMENT" "report has statement header"
assert_contains "$RPT_OUTPUT" "Alice Johnson" "report shows Alice's name"
assert_contains "$RPT_OUTPUT" "End of Statement" "report has footer"
assert_contains "$RPT_OUTPUT" "Payroll deposit" "report shows deposit description"
assert_contains "$RPT_OUTPUT" "Deposit" "report shows transaction type"

#================================================================
# Phase 6: Account Closure
#================================================================
phase_header 6 "Account Closure"

# Close the test account (10000006)
CLOSE_OUTPUT=$(printf "4\n10000006\nY\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$CLOSE_OUTPUT" "has been closed" "close account succeeds"

# Verify closed status
CLOSED_LOOKUP=$(printf "2\n10000006\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$CLOSED_LOOKUP" "Closed" "account shows closed status"

# Try to re-close
RECLOSE_OUTPUT=$(printf "4\n10000006\nY\nQ\n" | "$BIN/acctmgr" 2>&1)
assert_contains "$RECLOSE_OUTPUT" "already closed" "re-close rejected"

#================================================================
# Summary
#================================================================
echo ""
echo -e "${CYAN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BOLD}  Test Results: ${PASS} passed, ${FAIL} failed, ${TOTAL} total${NC}"
echo -e "${CYAN}${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

if [ "$FAIL" -eq 0 ]; then
    echo -e "  ${GREEN}${BOLD}ALL TESTS PASSED${NC}"
    echo ""
    exit 0
else
    echo -e "  ${RED}${BOLD}SOME TESTS FAILED${NC}"
    echo ""
    exit 1
fi
