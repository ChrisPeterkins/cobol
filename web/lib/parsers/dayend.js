const { parseCurrency, cleanOutput } = require('./acctmgr');

function parseDayend(raw) {
  const text = cleanOutput(raw);
  const lines = text.split('\n');

  // Parse per-account details
  const accountDetails = [];
  let i = 0;
  while (i < lines.length) {
    const acctMatch = lines[i].match(/Account:\s*(\d{8})\s+(.+)/);
    if (acctMatch) {
      const detail = {
        accountNo: acctMatch[1],
        name: acctMatch[2].trim(),
        deposits: 0,
        withdrawals: 0,
        transfers: 0,
        netChange: 0,
        curBalance: 0,
        txnCount: ''
      };
      // Read the next few lines for this account's details
      for (let j = i + 1; j < Math.min(i + 6, lines.length); j++) {
        const depMatch = lines[j].match(/Deposits:\s+([\$\d,.\-\s]+?)(?:\s+\(count:\s*(\d+)\))?$/);
        if (depMatch) {
          detail.deposits = parseCurrency(depMatch[1]);
          if (depMatch[2]) detail.txnCount = depMatch[2];
        }
        const wdMatch = lines[j].match(/Withdrawals:\s+([\$\d,.\-\s]+)/);
        if (wdMatch) detail.withdrawals = parseCurrency(wdMatch[1]);
        const xferMatch = lines[j].match(/Transfers:\s+([\$\d,.\-\s]+)/);
        if (xferMatch) detail.transfers = parseCurrency(xferMatch[1]);
        const netMatch = lines[j].match(/Net Change:\s+([\$\d,.\-\s]+)/);
        if (netMatch) detail.netChange = parseCurrency(netMatch[1]);
        const balMatch = lines[j].match(/Cur Balance:\s+([\$\d,.\-\s]+)/);
        if (balMatch) detail.curBalance = parseCurrency(balMatch[1]);
      }
      accountDetails.push(detail);
    }
    i++;
  }

  // Parse daily summary
  const summary = {
    date: (text.match(/Date:\s+(\d{8})/) || [])[1] || '',
    accountsActive: parseInt((text.match(/Accounts Active:\s*(\d+)/) || [])[1] || '0', 10),
    transactions: parseInt((text.match(/Transactions:\s+(\d+)/) || [])[1] || '0', 10),
    totalDeposits: parseCurrency((text.match(/Total Deposits:\s+([\$\d,.\-\s]+)/) || [])[1]),
    totalWithdrawals: parseCurrency((text.match(/Total Withdraws:\s+([\$\d,.\-\s]+)/) || [])[1]),
    totalTransfers: parseCurrency((text.match(/Total Transfers:\s+([\$\d,.\-\s]+)/) || [])[1]),
    discrepancies: parseInt((text.match(/Discrepancies:\s+(\d+)/) || [])[1] || '0', 10),
    reconciliation: /Reconciliation: PASS/i.test(text) ? 'PASS' : 'FAIL'
  };

  const txnCount = (text.match(/Transactions found for this date:\s*(\d+)/) || [])[1];
  if (txnCount !== undefined) summary.txnFoundForDate = parseInt(txnCount, 10);

  return { success: true, summary, accountDetails };
}

module.exports = { parseDayend };
