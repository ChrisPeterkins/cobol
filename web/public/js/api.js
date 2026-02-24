// API client for COBOL Bank Ledger
const API = {
  async _fetch(url, options = {}) {
    const res = await fetch(url, {
      headers: { 'Content-Type': 'application/json' },
      ...options
    });
    const data = await res.json();
    // Push raw output to terminal if present
    if (data.raw) {
      App.pushTerminal(data.raw);
    }
    if (!res.ok && !data.success) {
      throw { status: res.status, ...data };
    }
    return data;
  },

  // Accounts
  listAccounts() {
    return this._fetch('/api/accounts');
  },
  getAccount(id) {
    return this._fetch(`/api/accounts/${id}`);
  },
  createAccount(name, type, deposit) {
    return this._fetch('/api/accounts', {
      method: 'POST',
      body: JSON.stringify({ name, type, deposit })
    });
  },
  closeAccount(id) {
    return this._fetch(`/api/accounts/${id}`, { method: 'DELETE' });
  },

  // Transactions
  deposit(account, amount, description) {
    return this._fetch('/api/transactions/deposit', {
      method: 'POST',
      body: JSON.stringify({ account, amount, description })
    });
  },
  withdraw(account, amount, description) {
    return this._fetch('/api/transactions/withdraw', {
      method: 'POST',
      body: JSON.stringify({ account, amount, description })
    });
  },
  transfer(from, to, amount) {
    return this._fetch('/api/transactions/transfer', {
      method: 'POST',
      body: JSON.stringify({ from, to, amount })
    });
  },

  // Reports
  dayend(date) {
    return this._fetch('/api/reports/dayend', {
      method: 'POST',
      body: JSON.stringify({ date })
    });
  },
  statement(account, startDate, endDate) {
    return this._fetch('/api/reports/statement', {
      method: 'POST',
      body: JSON.stringify({ account, startDate, endDate })
    });
  },

  // System
  seed() {
    return this._fetch('/api/system/seed', { method: 'POST' });
  },
  health() {
    return this._fetch('/api/system/health');
  }
};
