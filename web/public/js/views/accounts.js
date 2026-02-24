const AccountsView = {
  render() {
    return `
      <div class="section-title">ACCOUNT MANAGEMENT</div>
      <div class="tab-bar">
        <button class="tab-btn active" data-tab="list">LIST</button>
        <button class="tab-btn" data-tab="create">CREATE</button>
        <button class="tab-btn" data-tab="detail">DETAIL</button>
      </div>
      <div id="acct-tab-content"></div>
    `;
  },

  init() {
    document.querySelectorAll('.tab-bar .tab-btn').forEach(btn => {
      btn.addEventListener('click', () => {
        document.querySelectorAll('.tab-bar .tab-btn').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        this.showTab(btn.dataset.tab);
      });
    });
    this.showTab('list');
  },

  showTab(tab) {
    const el = document.getElementById('acct-tab-content');
    if (tab === 'list') this.renderList(el);
    else if (tab === 'create') this.renderCreate(el);
    else if (tab === 'detail') this.renderDetail(el);
  },

  async renderList(el) {
    el.innerHTML = '<div class="loading">Loading accounts..._</div>';
    try {
      const data = await API.listAccounts();
      if (!data.accounts || data.accounts.length === 0) {
        el.innerHTML = '<div class="empty-state">No accounts found. Seed data or create an account.</div>';
        return;
      }
      el.innerHTML = `
        <table>
          <thead>
            <tr>
              <th>ACCT NO</th>
              <th>NAME</th>
              <th>BALANCE</th>
              <th>TYPE</th>
              <th>STATUS</th>
            </tr>
          </thead>
          <tbody>
            ${data.accounts.map(a => `
              <tr class="clickable" data-acct="${a.accountNo}">
                <td>${a.accountNo}</td>
                <td>${this.esc(a.name)}</td>
                <td class="num">$${a.balance.toLocaleString('en-US', {minimumFractionDigits: 2})}</td>
                <td>${this.esc(a.type)}</td>
                <td class="${a.status === 'Active' ? 'status-active' : 'status-closed'}">${this.esc(a.status)}</td>
              </tr>
            `).join('')}
          </tbody>
        </table>
        <div style="color: var(--text-dim); font-size: 12px; margin-top: 8px;">
          Total: ${data.totalCount} accounts
        </div>
      `;
      el.querySelectorAll('tr.clickable').forEach(row => {
        row.addEventListener('click', () => {
          this.selectedAccount = row.dataset.acct;
          document.querySelectorAll('.tab-bar .tab-btn').forEach(b => b.classList.remove('active'));
          document.querySelector('.tab-btn[data-tab="detail"]').classList.add('active');
          this.renderDetail(document.getElementById('acct-tab-content'));
        });
      });
    } catch (err) {
      el.innerHTML = `<div class="result-box error">Error: ${err.message || 'Failed to load accounts'}</div>`;
    }
  },

  renderCreate(el) {
    el.innerHTML = `
      <div class="panel">
        <div class="panel-title">CREATE NEW ACCOUNT</div>
        <form id="create-form">
          <div class="form-group">
            <label>ACCOUNT HOLDER NAME</label>
            <input type="text" id="create-name" maxlength="30" placeholder="e.g. John Doe" required>
          </div>
          <div class="form-row">
            <div class="form-group">
              <label>ACCOUNT TYPE</label>
              <select id="create-type">
                <option value="C">Checking</option>
                <option value="S">Savings</option>
              </select>
            </div>
            <div class="form-group">
              <label>OPENING DEPOSIT ($)</label>
              <input type="text" id="create-deposit" placeholder="1000.00" required>
            </div>
          </div>
          <button type="submit" class="btn">CREATE ACCOUNT</button>
        </form>
        <div id="create-result"></div>
      </div>
    `;
    document.getElementById('create-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const btn = e.target.querySelector('button');
      btn.disabled = true;
      btn.textContent = 'CREATING...';
      const resultEl = document.getElementById('create-result');
      try {
        const data = await API.createAccount(
          document.getElementById('create-name').value,
          document.getElementById('create-type').value,
          document.getElementById('create-deposit').value
        );
        resultEl.innerHTML = `
          <div class="result-box success">
            Account created successfully!
            Account No: ${data.account.accountNo}
            Name:       ${this.esc(data.account.name)}
            Balance:    $${data.account.balance.toLocaleString('en-US', {minimumFractionDigits: 2})}
            Type:       ${data.account.type === 'C' ? 'Checking' : 'Savings'}
          </div>
        `;
        App.toast('Account created: ' + data.account.accountNo, 'success');
        e.target.reset();
      } catch (err) {
        resultEl.innerHTML = `<div class="result-box error">${err.message || 'Creation failed'}</div>`;
        App.toast(err.message || 'Creation failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'CREATE ACCOUNT';
    });
  },

  async renderDetail(el) {
    if (!this.selectedAccount) {
      el.innerHTML = `
        <div class="panel">
          <div class="panel-title">ACCOUNT DETAIL</div>
          <div class="form-group">
            <label>ENTER ACCOUNT NUMBER</label>
            <input type="text" id="detail-acctno" maxlength="8" placeholder="10000001">
          </div>
          <button class="btn" id="btn-lookup">LOOKUP</button>
          <div id="detail-result"></div>
        </div>
      `;
      document.getElementById('btn-lookup').addEventListener('click', () => {
        this.selectedAccount = document.getElementById('detail-acctno').value;
        this.renderDetail(el);
      });
      return;
    }

    el.innerHTML = '<div class="loading">Looking up account..._</div>';
    try {
      const data = await API.getAccount(this.selectedAccount);
      if (!data.success) {
        el.innerHTML = `<div class="result-box error">${data.message}</div>`;
        this.selectedAccount = null;
        return;
      }
      const a = data.account;
      el.innerHTML = `
        <div class="panel">
          <div class="panel-title">ACCOUNT DETAIL</div>
          <div class="detail-row"><span class="detail-label">ACCOUNT NO:</span><span class="detail-value">${a.accountNo}</span></div>
          <div class="detail-row"><span class="detail-label">NAME:</span><span class="detail-value">${this.esc(a.name)}</span></div>
          <div class="detail-row"><span class="detail-label">BALANCE:</span><span class="detail-value">$${a.balance.toLocaleString('en-US', {minimumFractionDigits: 2})}</span></div>
          <div class="detail-row"><span class="detail-label">TYPE:</span><span class="detail-value">${this.esc(a.type)}</span></div>
          <div class="detail-row"><span class="detail-label">STATUS:</span><span class="detail-value ${a.status === 'Active' ? 'status-active' : 'status-closed'}">${this.esc(a.status)}</span></div>
          <div class="detail-row"><span class="detail-label">OPENED:</span><span class="detail-value">${this.formatDate(a.opened)}</span></div>
          <div style="margin-top: 16px; display: flex; gap: 10px;">
            ${a.status === 'Active' ? '<button class="btn btn-danger" id="btn-close-acct">CLOSE ACCOUNT</button>' : ''}
            <button class="btn btn-dim" id="btn-back-list">BACK TO LIST</button>
          </div>
          <div id="detail-action-result"></div>
        </div>
      `;
      const closeBtn = document.getElementById('btn-close-acct');
      if (closeBtn) {
        closeBtn.addEventListener('click', () => {
          App.confirm(
            'CLOSE ACCOUNT',
            `Are you sure you want to close account ${a.accountNo} (${this.esc(a.name)})?`,
            async () => {
              try {
                const res = await API.closeAccount(a.accountNo);
                if (res.success) {
                  App.toast(res.message, 'success');
                  this.renderDetail(el);
                } else {
                  App.toast(res.message, 'error');
                }
              } catch (err) {
                App.toast(err.message || 'Close failed', 'error');
              }
            }
          );
        });
      }
      document.getElementById('btn-back-list').addEventListener('click', () => {
        this.selectedAccount = null;
        document.querySelectorAll('.tab-bar .tab-btn').forEach(b => b.classList.remove('active'));
        document.querySelector('.tab-btn[data-tab="list"]').classList.add('active');
        this.renderList(el);
      });
    } catch (err) {
      el.innerHTML = `<div class="result-box error">${err.message || 'Lookup failed'}</div>`;
      this.selectedAccount = null;
    }
  },

  formatDate(yyyymmdd) {
    if (!yyyymmdd || yyyymmdd.length !== 8) return yyyymmdd || 'N/A';
    return `${yyyymmdd.slice(0,4)}-${yyyymmdd.slice(4,6)}-${yyyymmdd.slice(6,8)}`;
  },

  esc(str) {
    const d = document.createElement('div');
    d.textContent = str || '';
    return d.innerHTML;
  },

  selectedAccount: null
};
