const { execFile } = require('child_process');
const path = require('path');

const PROJECT_ROOT = path.resolve(__dirname, '..', '..');
const BIN_DIR = path.join(PROJECT_ROOT, 'bin');

function sanitize(str, maxLen = 100) {
  return String(str)
    .replace(/[\n\r]/g, '')
    .replace(/[^\x20-\x7E]/g, '')
    .substring(0, maxLen)
    .trim();
}

function runCobol(program, stdin = '', timeoutMs = 10000) {
  const binPath = path.join(BIN_DIR, program);

  return new Promise((resolve, reject) => {
    const child = execFile(binPath, [], {
      cwd: PROJECT_ROOT,
      timeout: timeoutMs,
      maxBuffer: 1024 * 1024,
      env: { ...process.env }
    }, (error, stdout, stderr) => {
      const raw = (stdout || '') + (stderr || '');
      if (error && error.killed) {
        return reject(new Error(`COBOL program '${program}' timed out after ${timeoutMs}ms`));
      }
      // COBOL programs may return non-zero on some paths, but we still want the output
      resolve({ raw, stdout: stdout || '', stderr: stderr || '', exitCode: error ? error.code : 0 });
    });

    if (stdin) {
      child.stdin.write(stdin);
      child.stdin.end();
    } else {
      child.stdin.end();
    }
  });
}

module.exports = { runCobol, sanitize, BIN_DIR, PROJECT_ROOT };
