function parseCurrency(str) {
  if (!str) return 0;
  return parseFloat(String(str).replace(/[$,]/g, '').trim()) || 0;
}

function cleanOutput(raw) {
  return raw.split('\n')
    .filter(l => !l.startsWith('libcob:'))
    .join('\n');
}

function parseCreate(raw) {
  const text = cleanOutput(raw);
  if (/Account created successfully/i.test(text)) {
    const acctNo = (text.match(/Account Number:\s*(\d+)/) || [])[1] || '';
    const name = (text.match(/Name:\s+(.+)/) || [])[1]?.trim() || '';
    const balance = parseCurrency((text.match(/Balance:\s+([\$\d,.\-]+)/) || [])[1]);
    const type = (text.match(/Type:\s+(\S)/) || [])[1] || '';
    return {
      success: true,
      message: 'Account created successfully',
      account: { accountNo: acctNo, name, balance, type }
    };
  }
  const errMatch = text.match(/(Name cannot be empty|Invalid type|Deposit must be|ERROR:.+)/);
  return { success: false, message: errMatch ? errMatch[1].trim() : 'Account creation failed' };
}

function parseLookup(raw) {
  const text = cleanOutput(raw);
  if (/not found/i.test(text)) {
    return { success: false, message: 'Account not found' };
  }
  const acctNo = (text.match(/Account No:\s*(\d+)/) || [])[1] || '';
  const name = (text.match(/Name:\s+(.+)/) || [])[1]?.trim() || '';
  const balance = parseCurrency((text.match(/Balance:\s+([\$\d,.\-]+)/) || [])[1]);
  const typeMatch = text.match(/Type:\s+(Checking|Savings|Unknown|\S)/);
  const type = typeMatch ? typeMatch[1].trim() : '';
  const statusMatch = text.match(/Status:\s+(Active|Closed|Unknown)/);
  const status = statusMatch ? statusMatch[1].trim() : '';
  const opened = (text.match(/Opened:\s+(\d+)/) || [])[1] || '';

  return {
    success: true,
    account: { accountNo: acctNo, name, balance, type, status, opened }
  };
}

function parseList(raw) {
  const text = cleanOutput(raw);
  const accounts = [];
  const lines = text.split('\n');

  for (const line of lines) {
    // Match account rows: "10000001  Alice Johnson                    $5,000.00  C     A"
    const m = line.match(/^(\d{8})\s{2}(.{30})\s{2}([\$\d,.\-\s]+)\s{2}(\S)\s{5}(\S)/);
    if (m) {
      accounts.push({
        accountNo: m[1],
        name: m[2].trim(),
        balance: parseCurrency(m[3]),
        type: m[4] === 'C' ? 'Checking' : m[4] === 'S' ? 'Savings' : m[4],
        status: m[5] === 'A' ? 'Active' : m[5] === 'X' ? 'Closed' : m[5]
      });
    }
  }

  const totalMatch = text.match(/Total accounts:\s*(\d+)/);
  const totalCount = totalMatch ? parseInt(totalMatch[1], 10) : accounts.length;

  return { success: true, accounts, totalCount };
}

function parseClose(raw) {
  const text = cleanOutput(raw);
  if (/has been closed/i.test(text)) {
    const acctNo = (text.match(/Account (\d+) has been closed/) || [])[1] || '';
    return { success: true, message: `Account ${acctNo} has been closed` };
  }
  if (/already closed/i.test(text)) {
    return { success: false, message: 'Account is already closed' };
  }
  if (/not found/i.test(text)) {
    return { success: false, message: 'Account not found' };
  }
  return { success: false, message: 'Close operation failed' };
}

module.exports = { parseCreate, parseLookup, parseList, parseClose, parseCurrency, cleanOutput };
