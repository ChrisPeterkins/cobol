const express = require('express');
const router = express.Router();
const { runCobol, sanitize } = require('../lib/cobol-runner');
const queue = require('../lib/queue');
const { parseCreate, parseLookup, parseList, parseClose } = require('../lib/parsers/acctmgr');

// GET /api/accounts — list all accounts
router.get('/', async (req, res) => {
  try {
    const result = await queue.enqueue(() => runCobol('acctmgr', '3\nQ\n'));
    const parsed = parseList(result.raw);
    res.json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// GET /api/accounts/:id — lookup single account
router.get('/:id', async (req, res) => {
  try {
    const id = sanitize(req.params.id, 8);
    const result = await queue.enqueue(() => runCobol('acctmgr', `2\n${id}\nQ\n`));
    const parsed = parseLookup(result.raw);
    res.json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// POST /api/accounts — create new account
router.post('/', async (req, res) => {
  try {
    const name = sanitize(req.body.name, 30);
    const type = sanitize(req.body.type, 1);
    const deposit = sanitize(req.body.deposit, 12);
    if (!name) return res.status(400).json({ error: 'Name is required' });
    if (!type || !['C', 'S'].includes(type.toUpperCase())) return res.status(400).json({ error: 'Type must be C or S' });
    if (!deposit) return res.status(400).json({ error: 'Deposit amount is required' });

    const stdin = `1\n${name}\n${type.toUpperCase()}\n${deposit}\nQ\n`;
    const result = await queue.enqueue(() => runCobol('acctmgr', stdin));
    const parsed = parseCreate(result.raw);
    const status = parsed.success ? 201 : 400;
    res.status(status).json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// DELETE /api/accounts/:id — close account
router.delete('/:id', async (req, res) => {
  try {
    const id = sanitize(req.params.id, 8);
    const stdin = `4\n${id}\nY\nQ\n`;
    const result = await queue.enqueue(() => runCobol('acctmgr', stdin));
    const parsed = parseClose(result.raw);
    res.json({ ...parsed, raw: result.raw });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

module.exports = router;
