// Main application — hash router + global utilities
const App = {
  routes: {
    accounts: AccountsView,
    transactions: TransactionsView,
    reports: ReportsView
  },

  init() {
    window.addEventListener('hashchange', () => this.route());

    // Seed button
    document.getElementById('btn-seed').addEventListener('click', async () => {
      const btn = document.getElementById('btn-seed');
      btn.disabled = true;
      btn.textContent = 'LOADING...';
      try {
        const data = await API.seed();
        this.toast(`Seed data loaded: ${data.accountCount} accounts`, 'success');
        // Refresh current view
        this.route();
      } catch (err) {
        this.toast(err.message || 'Seed failed', 'error');
      }
      btn.disabled = false;
      btn.textContent = 'SEED DATA';
    });

    // Terminal toggle
    document.getElementById('btn-terminal').addEventListener('click', () => this.toggleTerminal());
    document.getElementById('btn-terminal-close').addEventListener('click', () => this.toggleTerminal(false));

    // Modal
    document.getElementById('modal-cancel').addEventListener('click', () => this.hideModal());

    // Initial route
    if (!location.hash || location.hash === '#/') {
      location.hash = '#/accounts';
    } else {
      this.route();
    }
  },

  route() {
    const hash = location.hash.replace('#/', '') || 'accounts';
    const view = this.routes[hash];
    if (!view) {
      location.hash = '#/accounts';
      return;
    }
    // Update nav tabs
    document.querySelectorAll('.nav-tab').forEach(tab => {
      tab.classList.toggle('active', tab.dataset.route === hash);
    });
    // Render view
    const app = document.getElementById('app');
    app.innerHTML = view.render();
    if (view.init) view.init();
  },

  // Terminal panel
  toggleTerminal(force) {
    const panel = document.getElementById('terminal-panel');
    const show = force !== undefined ? force : panel.classList.contains('hidden');
    panel.classList.toggle('hidden', !show);
    document.body.classList.toggle('terminal-open', show);
  },

  pushTerminal(text) {
    const output = document.getElementById('terminal-output');
    const timestamp = new Date().toLocaleTimeString();
    output.textContent += `\n--- [${timestamp}] ---\n${text}\n`;
    output.scrollTop = output.scrollHeight;
  },

  // Toast notifications
  toast(message, type = 'info') {
    const container = document.getElementById('toast-container');
    const el = document.createElement('div');
    el.className = `toast ${type}`;
    el.textContent = message;
    container.appendChild(el);
    setTimeout(() => {
      el.style.opacity = '0';
      el.style.transition = 'opacity 0.3s';
      setTimeout(() => el.remove(), 300);
    }, 3000);
  },

  // Confirmation modal
  confirm(title, body, onConfirm) {
    document.getElementById('modal-title').textContent = title;
    document.getElementById('modal-body').textContent = body;
    document.getElementById('modal-overlay').classList.remove('hidden');
    const confirmBtn = document.getElementById('modal-confirm');
    const newBtn = confirmBtn.cloneNode(true);
    confirmBtn.parentNode.replaceChild(newBtn, confirmBtn);
    newBtn.addEventListener('click', () => {
      this.hideModal();
      onConfirm();
    });
  },

  hideModal() {
    document.getElementById('modal-overlay').classList.add('hidden');
  }
};

// Boot
document.addEventListener('DOMContentLoaded', () => App.init());
