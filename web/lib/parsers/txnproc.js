const { parseCurrency, cleanOutput } = require('./acctmgr');

function parseDeposit(raw) {
  const text = cleanOutput(raw);
  if (/Deposit successful/i.test(text)) {
    const amount = parseCurrency((text.match(/Amount:\s+([\$\d,.\-]+)/) || [])[1]);
    const newBalance = parseCurrency((text.match(/New Balance:\s+([\$\d,.\-]+)/) || [])[1]);
    return { success: true, message: 'Deposit successful', amount, newBalance };
  }
  if (/not found/i.test(text)) return { success: false, message: 'Account not found' };
  if (/not active/i.test(text)) return { success: false, message: 'Account is not active' };
  if (/at least/i.test(text)) return { success: false, message: 'Amount must be at least $0.01' };
  if (/overflow/i.test(text)) return { success: false, message: 'Balance overflow' };
  return { success: false, message: 'Deposit failed' };
}

function parseWithdraw(raw) {
  const text = cleanOutput(raw);
  if (/Withdrawal successful/i.test(text)) {
    const amount = parseCurrency((text.match(/Amount:\s+([\$\d,.\-]+)/) || [])[1]);
    const newBalance = parseCurrency((text.match(/New Balance:\s+([\$\d,.\-]+)/) || [])[1]);
    return { success: true, message: 'Withdrawal successful', amount, newBalance };
  }
  if (/not found/i.test(text)) return { success: false, message: 'Account not found' };
  if (/not active/i.test(text)) return { success: false, message: 'Account is not active' };
  if (/Insufficient funds/i.test(text)) return { success: false, message: 'Insufficient funds' };
  if (/at least/i.test(text)) return { success: false, message: 'Amount must be at least $0.01' };
  return { success: false, message: 'Withdrawal failed' };
}

function parseTransfer(raw) {
  const text = cleanOutput(raw);
  if (/Transfer successful/i.test(text)) {
    const amount = parseCurrency((text.match(/Amount:\s+([\$\d,.\-]+)/) || [])[1]);
    const fromMatch = text.match(/From (\d+) balance:\s+([\$\d,.\-]+)/);
    const toMatch = text.match(/To\s+(\d+) balance:\s+([\$\d,.\-]+)/);
    return {
      success: true,
      message: 'Transfer successful',
      amount,
      fromAccount: fromMatch ? fromMatch[1] : '',
      fromBalance: fromMatch ? parseCurrency(fromMatch[2]) : 0,
      toAccount: toMatch ? toMatch[1] : '',
      toBalance: toMatch ? parseCurrency(toMatch[2]) : 0
    };
  }
  if (/Source account not found/i.test(text)) return { success: false, message: 'Source account not found' };
  if (/Target account not found/i.test(text)) return { success: false, message: 'Target account not found' };
  if (/not active/i.test(text)) return { success: false, message: 'Account is not active' };
  if (/same account/i.test(text)) return { success: false, message: 'Cannot transfer to the same account' };
  if (/Insufficient funds/i.test(text)) return { success: false, message: 'Insufficient funds in source account' };
  if (/not found/i.test(text)) return { success: false, message: 'Account not found' };
  return { success: false, message: 'Transfer failed' };
}

module.exports = { parseDeposit, parseWithdraw, parseTransfer };
