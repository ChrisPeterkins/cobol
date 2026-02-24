const express = require('express');
const router = express.Router();
const { runCobol } = require('../lib/cobol-runner');
const queue = require('../lib/queue');

// POST /api/system/seed
router.post('/seed', async (req, res) => {
  try {
    const result = await queue.enqueue(() => runCobol('seedload'));
    const success = /Seed data loaded successfully/i.test(result.raw);
    const countMatch = result.raw.match(/Loaded\s+(\d+)\s+seed accounts/);
    const accountCount = countMatch ? parseInt(countMatch[1], 10) : 0;
    res.json({
      success,
      message: success ? 'Seed data loaded successfully' : 'Seed load failed',
      accountCount,
      raw: result.raw
    });
  } catch (err) {
    if (err.message.includes('Queue full')) return res.status(503).json({ error: err.message });
    res.status(500).json({ error: err.message });
  }
});

// GET /api/system/health
router.get('/health', (req, res) => {
  res.json({ status: 'ok', queue: queue.pending });
});

module.exports = router;
