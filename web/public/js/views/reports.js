const ReportsView = {
  render() {
    return `
      <div class="section-title">REPORTS</div>
      <div class="tab-bar">
        <button class="tab-btn active" data-tab="dayend">DAY-END RECONCILIATION</button>
        <button class="tab-btn" data-tab="statement">ACCOUNT STATEMENT</button>
      </div>
      <div id="rpt-tab-content"></div>
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
    this.showTab('dayend');
  },

  showTab(tab) {
    const el = document.getElementById('rpt-tab-content');
    if (tab === 'dayend') this.renderDayend(el);
    else if (tab === 'statement') this.renderStatement(el);
  },

  renderDayend(el) {
    const today = new Date().toISOString().slice(0, 10).replace(/-/g, '');
    el.innerHTML = `
      <div class="panel">
        <div class="panel-title">DAY-END BATCH RECONCILIATION</div>
        <form id="dayend-form">
          <div class="form-group">
            <label>PROCESSING DATE (YYYYMMDD, blank for today)</label>
            <input type="text" id="dayend-date" maxlength="8" placeholder="${today}">
          </div>
          <button type="submit" class="btn">RUN RECONCILIATION</button>
        </form>
        <div id="dayend-result"></div>
      </div>
    `;
    document.getElementById('dayend-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const btn = e.target.querySelector('button');
      btn.disabled = true;
      btn.textContent = 'PROCESSING...';
      const resultEl = document.getElementById('dayend-result');
      resultEl.innerHTML = '<div class="loading">Running reconciliation..._</div>';
      try {
        const data = await API.dayend(document.getElementById('dayend-date').value);
        const s = data.summary;
        let html = `<div class="report-output">`;
        html += `=========================================\n`;
        html += `  DAILY SUMMARY\n`;
        html += `=========================================\n`;
        html += `  Date:            ${s.date}\n`;
        html += `  Accounts Active: ${s.accountsActive}\n`;
        html += `  Transactions:    ${s.transactions}\n`;
        html += `  Total Deposits:  $${s.totalDeposits.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  Total Withdraws: $${s.totalWithdrawals.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  Total Transfers: $${s.totalTransfers.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  Discrepancies:   ${s.discrepancies}\n`;
        html += `  Reconciliation:  <span style="color: ${s.reconciliation === 'PASS' ? 'var(--green)' : 'var(--red)'}">${s.reconciliation}</span>\n`;
        html += `=========================================\n\n`;

        if (data.accountDetails && data.accountDetails.length > 0) {
          html += `  ACCOUNT DETAILS\n`;
          html += `  ---------------\n`;
          for (const a of data.accountDetails) {
            html += `\n  Account: ${a.accountNo}  ${this.esc(a.name)}\n`;
            html += `    Deposits:    $${a.deposits.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
            html += `    Withdrawals: $${a.withdrawals.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
            html += `    Transfers:   $${a.transfers.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
            html += `    Net Change:  $${a.netChange.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
            html += `    Cur Balance: $${a.curBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
          }
        }

        html += `</div>`;
        resultEl.innerHTML = html;
        App.toast(`Reconciliation: ${s.reconciliation}`, s.reconciliation === 'PASS' ? 'success' : 'error');
      } catch (err) {
        resultEl.innerHTML = `<div class="result-box error">${err.message || 'Reconciliation failed'}</div>`;
        App.toast(err.message || 'Reconciliation failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'RUN RECONCILIATION';
    });
  },

  renderStatement(el) {
    el.innerHTML = `
      <div class="panel">
        <div class="panel-title">ACCOUNT STATEMENT GENERATOR</div>
        <form id="stmt-form">
          <div class="form-group">
            <label>ACCOUNT NUMBER</label>
            <input type="text" id="stmt-acct" maxlength="8" placeholder="10000001" required>
          </div>
          <div class="form-row">
            <div class="form-group">
              <label>START DATE (YYYYMMDD, blank for all)</label>
              <input type="text" id="stmt-start" maxlength="8" placeholder="20250101">
            </div>
            <div class="form-group">
              <label>END DATE (YYYYMMDD, blank for today)</label>
              <input type="text" id="stmt-end" maxlength="8" placeholder="20251231">
            </div>
          </div>
          <button type="submit" class="btn">GENERATE STATEMENT</button>
        </form>
        <div id="stmt-result"></div>
      </div>
    `;
    document.getElementById('stmt-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const btn = e.target.querySelector('button');
      btn.disabled = true;
      btn.textContent = 'GENERATING...';
      const resultEl = document.getElementById('stmt-result');
      resultEl.innerHTML = '<div class="loading">Generating statement..._</div>';
      try {
        const data = await API.statement(
          document.getElementById('stmt-acct').value,
          document.getElementById('stmt-start').value,
          document.getElementById('stmt-end').value
        );
        const a = data.account;
        const sum = data.summary;
        let html = '<div class="report-output">';
        html += `${'='.repeat(78)}\n`;
        html += `  COBOL BANK LEDGER                    ACCOUNT STATEMENT\n`;
        html += `${'='.repeat(78)}\n\n`;
        html += `  Account:    ${a.accountNo}\n`;
        html += `  Name:       ${this.esc(a.name)}\n`;
        html += `  Type:       ${this.esc(a.type)}\n`;
        html += `  Status:     ${this.esc(a.status)}\n`;
        html += `  Period:     ${this.esc(a.period)}\n`;
        html += `  Cur Balance:$${a.curBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}\n\n`;
        html += `  Date       Time     Type        Amount          Balance         Description\n`;
        html += `  ${'-'.repeat(76)}\n`;
        html += `  Opening Balance:                              $${data.openingBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  ${'-'.repeat(76)}\n`;

        if (data.transactions && data.transactions.length > 0) {
          for (const t of data.transactions) {
            const typeStr = t.type.padEnd(9);
            html += `  ${t.date} ${t.time} ${typeStr} $${t.amount.toLocaleString('en-US', {minimumFractionDigits: 2}).padStart(12)} $${t.balance.toLocaleString('en-US', {minimumFractionDigits: 2}).padStart(14)} ${this.esc(t.description)}\n`;
          }
        } else {
          html += `  (No transactions in this period)\n`;
        }

        html += `  ${'-'.repeat(76)}\n\n`;
        html += `  STATEMENT SUMMARY\n`;
        html += `  -----------------\n`;
        html += `  Transactions:     ${sum.transactions}\n`;
        html += `  Total Deposits:   $${sum.totalDeposits.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  Total Withdrawals:$${sum.totalWithdrawals.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  Total Transfers:  $${sum.totalTransfers.toLocaleString('en-US', {minimumFractionDigits: 2})}\n`;
        html += `  Closing Balance:  $${sum.closingBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}\n\n`;
        html += `${'='.repeat(78)}\n`;
        html += `  End of Statement\n`;
        html += `${'='.repeat(78)}\n`;
        html += '</div>';
        resultEl.innerHTML = html;
        App.toast('Statement generated', 'success');
      } catch (err) {
        resultEl.innerHTML = `<div class="result-box error">${err.message || 'Statement generation failed'}</div>`;
        App.toast(err.message || 'Statement failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'GENERATE STATEMENT';
    });
  },

  esc(str) {
    const d = document.createElement('div');
    d.textContent = str || '';
    return d.innerHTML;
  }
};
