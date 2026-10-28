// Tests for mark-stale.js and draft-pr-policy.js. Run: node .github/scripts/stale-policy.test.js
'use strict';

const assert = require('assert');
const { runMarkStale }   = require('./mark-stale.js');
const { runDraftPolicy } = require('./draft-pr-policy.js');

const DAY = 24 * 60 * 60 * 1000;
const daysAgo = (n) => new Date(Date.now() - n * DAY).toISOString();

const HUMAN = { login: 'jhendersonHDF', type: 'User' };
const BOT   = { login: 'github-actions[bot]', type: 'Bot' };

// Recording mock of the github.rest surface the two scripts use. `pr` is the
// single open PR; the per-PR lists come from `data`.
function makeGithub(pr, data = {}) {
  const calls = { addLabels: [], removeLabel: [], createComment: [], listEvents: 0 };
  const list = (items) => async () => items || [];
  return {
    calls,
    paginate: async (fn, opts) => fn(opts),
    rest: {
      pulls: {
        list:               list([pr]),
        get:                async () => ({ data: pr }),
        listCommits:        list(data.commits),
        listReviews:        list(data.reviews),
        listReviewComments: list(data.reviewComments),
      },
      issues: {
        getLabel:      async () => ({}),
        listComments:  list(data.comments),
        listEvents:    async () => { calls.listEvents++; return data.events || []; },
        addLabels:     async (o) => { calls.addLabels.push(o.labels[0]); },
        removeLabel:   async (o) => { calls.removeLabel.push(o.name); },
        createComment: async (o) => { calls.createComment.push(o.body); },
      },
    },
  };
}

const core = { info: () => {}, warning: () => {} };
const scheduleContext = { eventName: 'schedule', repo: { owner: 'HDFGroup', repo: 'hdf5' } };

function makePr({ draft, labels = [], createdDaysAgo = 200 }) {
  return {
    number: 1, draft, created_at: daysAgo(createdDaysAgo),
    labels: labels.map((name) => ({ name })), user: { login: 'jhendersonHDF' },
  };
}
const commit = (n) => ({ commit: { committer: { date: daysAgo(n) } } });
const unlabeled = (label, actor, n) => ({ event: 'unlabeled', label: { name: label }, actor, created_at: daysAgo(n) });

const tests = [];
const test = (name, fn) => tests.push({ name, fn });

// ---------------------------------------------------------------- draft-pr-policy

test('draft: inactive draft with no label removal is flagged', async () => {
  const github = makeGithub(makePr({ draft: true }), { commits: [commit(90)] });
  await runDraftPolicy({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, ['draft-stale']);
  assert.strictEqual(github.calls.createComment.length, 1);
});

test('draft: a person removing draft-stale counts as activity (PR #6513)', async () => {
  const github = makeGithub(makePr({ draft: true }), {
    commits: [commit(90)],
    events: [unlabeled('draft-stale', HUMAN, 1)],
  });
  await runDraftPolicy({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, [], 'Label is not put back the next day');
  assert.deepStrictEqual(github.calls.createComment, [], 'No second keepalive comment');
});

test('draft: a label removal older than the window does not keep it fresh', async () => {
  const github = makeGithub(makePr({ draft: true }), {
    commits: [commit(120)],
    events: [unlabeled('draft-stale', HUMAN, 70)],
  });
  await runDraftPolicy({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, ['draft-stale']);
});

test('draft: a bot removing draft-stale does not count', async () => {
  const github = makeGithub(makePr({ draft: true }), {
    commits: [commit(90)],
    events: [unlabeled('draft-stale', BOT, 1)],
  });
  await runDraftPolicy({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, ['draft-stale']);
});

test('draft: removing some other label does not count', async () => {
  const github = makeGithub(makePr({ draft: true }), {
    commits: [commit(90)],
    events: [unlabeled('needs-decision', HUMAN, 1)],
  });
  await runDraftPolicy({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, ['draft-stale']);
});

test('draft: recently active draft is not flagged and label events are not fetched', async () => {
  const github = makeGithub(makePr({ draft: true }), { commits: [commit(10)] });
  await runDraftPolicy({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, []);
  assert.strictEqual(github.calls.listEvents, 0);
});

test('draft: checking the keepalive box un-stales on the comment event', async () => {
  const pr = { ...makePr({ draft: true, labels: ['draft-stale'] }) };
  const github = makeGithub(pr, {
    comments: [{
      user: BOT, created_at: daysAgo(5), updated_at: daysAgo(0),
      body: '<!-- draft-stale-keepalive -->\n- [x] Still working on this -- check this box to keep the draft open',
    }],
  });
  const context = {
    eventName: 'issue_comment', repo: scheduleContext.repo,
    payload: { action: 'edited', issue: { number: 1, pull_request: {} } },
  };
  await runDraftPolicy({ github, context, core });
  assert.deepStrictEqual(github.calls.removeLabel, ['draft-stale']);
});

// ---------------------------------------------------------------- mark-stale

test('ready PR: inactive PR with no label removal is marked stale', async () => {
  const github = makeGithub(makePr({ draft: false }), { commits: [commit(40)] });
  await runMarkStale({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, ['stale']);
});

test('ready PR: a person removing stale counts as activity', async () => {
  const github = makeGithub(makePr({ draft: false }), {
    commits: [commit(40)],
    events: [unlabeled('stale', HUMAN, 1)],
  });
  await runMarkStale({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, []);
  assert.deepStrictEqual(github.calls.createComment, []);
});

test('ready PR: a bot removing stale does not count', async () => {
  const github = makeGithub(makePr({ draft: false }), {
    commits: [commit(40)],
    events: [unlabeled('stale', BOT, 1)],
  });
  await runMarkStale({ github, context: scheduleContext, core });
  assert.deepStrictEqual(github.calls.addLabels, ['stale']);
});

// ---------------------------------------------------------------- run

(async () => {
  let passed = 0, failed = 0;
  for (const { name, fn } of tests) {
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
