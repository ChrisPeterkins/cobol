const { parseCurrency, cleanOutput } = require('./acctmgr');

function parseStatement(raw) {
  const text = cleanOutput(raw);

  if (/not found/i.test(text)) {
    return { success: false, message: 'Account not found' };
  }

  // Parse account header
  const account = {
    accountNo: (text.match(/Account:\s+(\d+)/) || [])[1] || '',
    name: (text.match(/Name:\s+(.+)/) || [])[1]?.trim() || '',
    type: (text.match(/Type:\s+(\S+)/) || [])[1]?.trim() || '',
    status: (text.match(/Status:\s+(\S+)/) || [])[1]?.trim() || '',
    period: (text.match(/Period:\s+(.+)/) || [])[1]?.trim() || '',
    curBalance: parseCurrency((text.match(/Cur Balance:([\$\d,.\-\s]+)/) || [])[1])
  };

  // Parse opening balance
  const openBalMatch = text.match(/Opening Balance:[\s]*([\$\d,.\-]+)/);
  const openingBalance = openBalMatch ? parseCurrency(openBalMatch[1]) : 0;

  // Parse transaction lines
  // Format: "  YYYY-MM-DD HH:MM:SS Deposit  $1,000.00       $6,000.00 Payroll deposit"
  const transactions = [];
  const lines = text.split('\n');
  for (const line of lines) {
    const m = line.match(/^\s{2}(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2}:\d{2})\s+(Deposit|Withdraw|Transfer)\s+([\$\d,.\-\s]+?)\s+([\$\d,.\-\s]+?)\s+(.+)$/);
    if (m) {
      transactions.push({
        date: m[1],
        time: m[2],
        type: m[3],
        amount: parseCurrency(m[4]),
        balance: parseCurrency(m[5]),
        description: m[6].trim()
      });
    }
  }

  // Parse summary
  const summary = {
    transactions: parseInt((text.match(/Transactions:\s+(\d+)/) || [])[1] || '0', 10),
    totalDeposits: parseCurrency((text.match(/Total Deposits:\s+([\$\d,.\-]+)/) || [])[1]),
    totalWithdrawals: parseCurrency((text.match(/Total Withdrawals:([\$\d,.\-\s]+)/) || [])[1]),
    totalTransfers: parseCurrency((text.match(/Total Transfers:\s+([\$\d,.\-]+)/) || [])[1]),
    closingBalance: parseCurrency((text.match(/Closing Balance:\s+([\$\d,.\-]+)/) || [])[1])
  };

  return { success: true, account, openingBalance, transactions, summary };
}

module.exports = { parseStatement };
