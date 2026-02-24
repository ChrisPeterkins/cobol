const express = require('express');
const router = express.Router();
const { runCobol, sanitize } = require('../lib/cobol-runner');
const queue = require('../lib/queue');
const { parseDayend } = require('../lib/parsers/dayend');
const { parseStatement } = require('../lib/parsers/rptgen');

// POST /api/reports/dayend
router.post('/dayend', async (req, res) => {
  try {
    const date = sanitize(req.body.date || '', 8);
    // Empty string = press Enter = use today's date
    const stdin = `${date}\n`;
    const result = await queue.enqueue(() => runCobol('dayend', stdin));
    const parsed = parseDayend(result.raw);
    res.json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// POST /api/reports/statement
router.post('/statement', async (req, res) => {
  try {
    const acct = sanitize(req.body.account, 8);
    const startDate = sanitize(req.body.startDate || '', 8);
    const endDate = sanitize(req.body.endDate || '', 8);
    if (!acct) return res.status(400).json({ error: 'Account number is required' });

    const stdin = `${acct}\n${startDate}\n${endDate}\n`;
    const result = await queue.enqueue(() => runCobol('rptgen', stdin));
    const parsed = parseStatement(result.raw);
    const status = parsed.success ? 200 : 400;
    res.status(status).json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

module.exports = router;
