'use strict';
// Run with: node .github/scripts/dependabot-auto-merge.test.js

const assert = require('assert');
const { matchesTitle, allChecksPassed } = require('./dependabot-auto-merge.js');

let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`✓ ${name}`);
    passed++;
  } catch (e) {
    console.log(`✗ ${name} — ${e.message}`);
    failed++;
  }
}

const asyncTests = [];
function asyncTest(name, fn) {
  asyncTests.push({ name, fn });
}

// Minimal recording mock for the github.rest/paginate surface allChecksPassed touches.
function makeGithubMock({ checkRuns = [], statuses = [] }) {
  return {
    paginate: async (fn) => fn(),
    rest: {
      checks: {
        listForRef: async () => checkRuns,
      },
      repos: {
        getCombinedStatusForRef: async () => ({ data: { statuses } }),
      },
    },
  };
}

// ----------------------------------------------------------------
// matchesTitle
// ----------------------------------------------------------------

test('matches plain "Bump the github-actions group with N updates"', () => {
  assert.strictEqual(matchesTitle('Bump the github-actions group with 12 updates'), true);
});

test('matches "build(deps): bump the github-actions group with N updates"', () => {
  assert.strictEqual(matchesTitle('build(deps): bump the github-actions group with 17 updates'), true);
});

test('matches "across 1 directory" variant', () => {
  assert.strictEqual(matchesTitle('Bump the github-actions group across 1 directory with 11 updates'), true);
});

test('is case-insensitive', () => {
  assert.strictEqual(matchesTitle('BUMP THE GITHUB-ACTIONS GROUP WITH 3 UPDATES'), true);
});

test('does not match an unrelated dependabot title', () => {
  assert.strictEqual(matchesTitle('Bump lycheeverse/lychee-action from 1.9.3 to 2.0.2 in /.github/workflows'), false);
});

test('does not match a human PR title that happens to mention bumping', () => {
  assert.strictEqual(matchesTitle('Bump minimum CMake version'), false);
});

// ----------------------------------------------------------------
// allChecksPassed
// ----------------------------------------------------------------

asyncTest('not ready when a check run is still in progress', async () => {
  const github = makeGithubMock({
    checkRuns: [{ name: 'build', status: 'in_progress', conclusion: null }],
  });
  const result = await allChecksPassed({ github, owner: 'o', repo: 'r', ref: 'sha' });
  assert.strictEqual(result.ready, false);
  assert.match(result.reason, /still in_progress/);
});

asyncTest('not ready when a completed check run failed', async () => {
  const github = makeGithubMock({
    checkRuns: [{ name: 'build', status: 'completed', conclusion: 'failure' }],
  });
  const result = await allChecksPassed({ github, owner: 'o', repo: 'r', ref: 'sha' });
  assert.strictEqual(result.ready, false);
  assert.match(result.reason, /concluded failure/);
});

asyncTest('ready when all check runs are success/neutral/skipped', async () => {
  const github = makeGithubMock({
    checkRuns: [
      { name: 'build', status: 'completed', conclusion: 'success' },
      { name: 'lint', status: 'completed', conclusion: 'neutral' },
      { name: 'matrix-unused-leg', status: 'completed', conclusion: 'skipped' },
    ],
  });
  const result = await allChecksPassed({ github, owner: 'o', repo: 'r', ref: 'sha' });
  assert.strictEqual(result.ready, true);
});

asyncTest('not ready when a legacy commit status is pending', async () => {
  const github = makeGithubMock({
    checkRuns: [{ name: 'build', status: 'completed', conclusion: 'success' }],
    statuses: [{ context: 'ci/legacy', state: 'pending' }],
  });
  const result = await allChecksPassed({ github, owner: 'o', repo: 'r', ref: 'sha' });
  assert.strictEqual(result.ready, false);
  assert.match(result.reason, /ci\/legacy/);
});

asyncTest('not ready when there are no checks or statuses at all yet', async () => {
  const github = makeGithubMock({});
  const result = await allChecksPassed({ github, owner: 'o', repo: 'r', ref: 'sha' });
  assert.strictEqual(result.ready, false);
  assert.match(result.reason, /no checks or statuses/);
});

// ----------------------------------------------------------------
// Summary
// ----------------------------------------------------------------

(async () => {
  for (const { name, fn } of asyncTests) {
    try {
      await fn();
      console.log(`✓ ${name}`);
      passed++;
    } catch (e) {
      console.log(`✗ ${name} — ${e.message}`);
      failed++;
    }
  }
  console.log('');
  console.log(`${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
})();
