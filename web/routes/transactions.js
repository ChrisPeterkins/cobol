const express = require('express');
const router = express.Router();
const { runCobol, sanitize } = require('../lib/cobol-runner');
const queue = require('../lib/queue');
const { parseDeposit, parseWithdraw, parseTransfer } = require('../lib/parsers/txnproc');

// POST /api/transactions/deposit
router.post('/deposit', async (req, res) => {
  try {
    const acct = sanitize(req.body.account, 8);
    const amount = sanitize(req.body.amount, 12);
    const desc = sanitize(req.body.description || 'Web deposit', 20);
    if (!acct) return res.status(400).json({ error: 'Account number is required' });
    if (!amount) return res.status(400).json({ error: 'Amount is required' });

    const stdin = `1\n${acct}\n${amount}\n${desc}\nQ\n`;
    const result = await queue.enqueue(() => runCobol('txnproc', stdin));
    const parsed = parseDeposit(result.raw);
    const status = parsed.success ? 200 : 400;
    res.status(status).json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// POST /api/transactions/withdraw
router.post('/withdraw', async (req, res) => {
  try {
    const acct = sanitize(req.body.account, 8);
    const amount = sanitize(req.body.amount, 12);
    const desc = sanitize(req.body.description || 'Web withdrawal', 20);
    if (!acct) return res.status(400).json({ error: 'Account number is required' });
    if (!amount) return res.status(400).json({ error: 'Amount is required' });

    const stdin = `2\n${acct}\n${amount}\n${desc}\nQ\n`;
    const result = await queue.enqueue(() => runCobol('txnproc', stdin));
    const parsed = parseWithdraw(result.raw);
    const status = parsed.success ? 200 : 400;
    res.status(status).json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// POST /api/transactions/transfer
router.post('/transfer', async (req, res) => {
  try {
    const from = sanitize(req.body.from, 8);
    const to = sanitize(req.body.to, 8);
    const amount = sanitize(req.body.amount, 12);
    if (!from) return res.status(400).json({ error: 'Source account is required' });
    if (!to) return res.status(400).json({ error: 'Target account is required' });
    if (!amount) return res.status(400).json({ error: 'Amount is required' });

    const stdin = `3\n${from}\n${to}\n${amount}\nQ\n`;
    const result = await queue.enqueue(() => runCobol('txnproc', stdin));
    const parsed = parseTransfer(result.raw);
    const status = parsed.success ? 200 : 400;
    res.status(status).json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

module.exports = router;
