const TransactionsView = {
  render() {
    return `
      <div class="section-title">TRANSACTION PROCESSING</div>
      <div class="tab-bar">
        <button class="tab-btn active" data-tab="deposit">DEPOSIT</button>
        <button class="tab-btn" data-tab="withdraw">WITHDRAW</button>
        <button class="tab-btn" data-tab="transfer">TRANSFER</button>
      </div>
      <div id="txn-tab-content"></div>
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
    this.showTab('deposit');
  },

  showTab(tab) {
    const el = document.getElementById('txn-tab-content');
    if (tab === 'deposit') this.renderDeposit(el);
    else if (tab === 'withdraw') this.renderWithdraw(el);
    else if (tab === 'transfer') this.renderTransfer(el);
  },

  renderDeposit(el) {
    el.innerHTML = `
      <div class="panel">
        <div class="panel-title">DEPOSIT FUNDS</div>
        <form id="deposit-form">
          <div class="form-row">
            <div class="form-group">
              <label>ACCOUNT NUMBER</label>
              <input type="text" id="dep-acct" maxlength="8" placeholder="10000001" required>
            </div>
            <div class="form-group">
              <label>AMOUNT ($)</label>
              <input type="text" id="dep-amount" placeholder="1000.00" required>
            </div>
          </div>
          <div class="form-group">
            <label>DESCRIPTION</label>
            <input type="text" id="dep-desc" maxlength="20" placeholder="Payroll deposit">
          </div>
          <button type="submit" class="btn">PROCESS DEPOSIT</button>
        </form>
        <div id="dep-result"></div>
      </div>
    `;
    document.getElementById('deposit-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const btn = e.target.querySelector('button');
      btn.disabled = true;
      btn.textContent = 'PROCESSING...';
      try {
        const data = await API.deposit(
          document.getElementById('dep-acct').value,
          document.getElementById('dep-amount').value,
          document.getElementById('dep-desc').value || 'Web deposit'
        );
        document.getElementById('dep-result').innerHTML = `
          <div class="result-box success">
Deposit successful!
  Amount:      $${data.amount.toLocaleString('en-US', {minimumFractionDigits: 2})}
  New Balance: $${data.newBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}
          </div>
        `;
        App.toast('Deposit successful', 'success');
      } catch (err) {
        document.getElementById('dep-result').innerHTML = `<div class="result-box error">${err.message || 'Deposit failed'}</div>`;
        App.toast(err.message || 'Deposit failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'PROCESS DEPOSIT';
    });
  },

  renderWithdraw(el) {
    el.innerHTML = `
      <div class="panel">
        <div class="panel-title">WITHDRAW FUNDS</div>
        <form id="withdraw-form">
          <div class="form-row">
            <div class="form-group">
              <label>ACCOUNT NUMBER</label>
              <input type="text" id="wd-acct" maxlength="8" placeholder="10000001" required>
            </div>
            <div class="form-group">
              <label>AMOUNT ($)</label>
              <input type="text" id="wd-amount" placeholder="200.00" required>
            </div>
          </div>
          <div class="form-group">
            <label>DESCRIPTION</label>
            <input type="text" id="wd-desc" maxlength="20" placeholder="ATM withdrawal">
          </div>
          <button type="submit" class="btn">PROCESS WITHDRAWAL</button>
        </form>
        <div id="wd-result"></div>
      </div>
    `;
    document.getElementById('withdraw-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const btn = e.target.querySelector('button');
      btn.disabled = true;
      btn.textContent = 'PROCESSING...';
      try {
        const data = await API.withdraw(
          document.getElementById('wd-acct').value,
          document.getElementById('wd-amount').value,
          document.getElementById('wd-desc').value || 'Web withdrawal'
        );
        document.getElementById('wd-result').innerHTML = `
          <div class="result-box success">
Withdrawal successful!
  Amount:      $${data.amount.toLocaleString('en-US', {minimumFractionDigits: 2})}
  New Balance: $${data.newBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}
          </div>
        `;
        App.toast('Withdrawal successful', 'success');
      } catch (err) {
        document.getElementById('wd-result').innerHTML = `<div class="result-box error">${err.message || 'Withdrawal failed'}</div>`;
        App.toast(err.message || 'Withdrawal failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'PROCESS WITHDRAWAL';
    });
  },

  renderTransfer(el) {
    el.innerHTML = `
      <div class="panel">
        <div class="panel-title">TRANSFER FUNDS</div>
        <form id="transfer-form">
          <div class="form-row">
            <div class="form-group">
              <label>FROM ACCOUNT</label>
              <input type="text" id="xfer-from" maxlength="8" placeholder="10000002" required>
            </div>
            <div class="form-group">
              <label>TO ACCOUNT</label>
              <input type="text" id="xfer-to" maxlength="8" placeholder="10000001" required>
            </div>
          </div>
          <div class="form-group">
            <label>AMOUNT ($)</label>
            <input type="text" id="xfer-amount" placeholder="500.00" required>
          </div>
          <button type="submit" class="btn">PROCESS TRANSFER</button>
        </form>
        <div id="xfer-result"></div>
      </div>
    `;
    document.getElementById('transfer-form').addEventListener('submit', async (e) => {
      e.preventDefault();
      const btn = e.target.querySelector('button');
      btn.disabled = true;
      btn.textContent = 'PROCESSING...';
      try {
        const data = await API.transfer(
          document.getElementById('xfer-from').value,
          document.getElementById('xfer-to').value,
          document.getElementById('xfer-amount').value
        );
        document.getElementById('xfer-result').innerHTML = `
          <div class="result-box success">
Transfer successful!
  Amount: $${data.amount.toLocaleString('en-US', {minimumFractionDigits: 2})}
  From ${data.fromAccount} balance: $${data.fromBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}
  To   ${data.toAccount} balance: $${data.toBalance.toLocaleString('en-US', {minimumFractionDigits: 2})}
          </div>
        `;
        App.toast('Transfer successful', 'success');
      } catch (err) {
        document.getElementById('xfer-result').innerHTML = `<div class="result-box error">${err.message || 'Transfer failed'}</div>`;
        App.toast(err.message || 'Transfer failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'PROCESS TRANSFER';
    });
  }
};
