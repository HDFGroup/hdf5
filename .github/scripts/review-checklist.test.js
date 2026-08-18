'use strict';
// Run with: node .github/scripts/review-checklist.test.js

const assert = require('assert');
const {
  MARKER,
  matchesPattern,
  labelFromPattern,
  attributeFiles,
  computeApprovals,
  computeChangesRequested,
  buildChangeRequestFileMap,
  chooseReviewers,
  resolveAreaPicks,
  buildBody,
  parseExcluded,
  serializeExcluded,
  withExcluded,
  parseManuallyAdded,
  serializeManuallyAdded,
  parseAssigned,
  serializeAssigned,
  planSynchronizeSwaps,
  coordinateReviewers,
} = require('./review-checklist.js');

// Minimal recording mock for the github.rest surface coordinateReviewers
// touches. Each call resolves successfully and is appended to its call log.
function makeGithubMock() {
  const calls = { removeRequestedReviewers: [], requestReviewers: [], addAssignees: [] };
  return {
    calls,
    rest: {
      pulls: {
        removeRequestedReviewers: async (opts) => { calls.removeRequestedReviewers.push(opts.reviewers[0]); },
        requestReviewers:         async (opts) => { calls.requestReviewers.push(opts.reviewers[0]); },
      },
      issues: {
        addAssignees: async (opts) => { calls.addAssignees.push(opts.assignees[0]); },
      },
    },
  };
}

function makeCore() {
  return { info: () => {}, warning: () => {}, setFailed: () => {} };
}

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

// coordinateReviewers exercises real async API calls (mocked). test() doesn't
// await, so an async fn's assertions would run after the pass/fail tally is
// already printed — queue these separately and await them before the summary.
const asyncTests = [];
function asyncTest(name, fn) {
  asyncTests.push({ name, fn });
}

// ----------------------------------------------------------------
// matchesPattern — anchored directory patterns
// ----------------------------------------------------------------

test('anchored dir: fortran/src/H5f.F90 matches /fortran/', () => {
  assert.strictEqual(matchesPattern('fortran/src/H5f.F90', '/fortran/'), true);
});

test('anchored dir: src/H5public.h does not match /fortran/', () => {
  assert.strictEqual(matchesPattern('src/H5public.h', '/fortran/'), false);
});

test('anchored dir: src/H5FDsubfiling/foo.c matches /src/H5FDsubfiling/', () => {
  assert.strictEqual(matchesPattern('src/H5FDsubfiling/foo.c', '/src/H5FDsubfiling/'), true);
});

test('anchored dir: src/H5FDsubfiling/foo.c matches less-specific /src/', () => {
  assert.strictEqual(matchesPattern('src/H5FDsubfiling/foo.c', '/src/'), true);
});

// ----------------------------------------------------------------
// matchesPattern — anchored plain-file patterns
// ----------------------------------------------------------------

test('anchored file: CMakeLists.txt matches /CMakeLists.txt', () => {
  assert.strictEqual(matchesPattern('CMakeLists.txt', '/CMakeLists.txt'), true);
});

test('anchored file: src/CMakeLists.txt does not match /CMakeLists.txt', () => {
  assert.strictEqual(matchesPattern('src/CMakeLists.txt', '/CMakeLists.txt'), false);
});

// ----------------------------------------------------------------
// matchesPattern — unanchored glob (*.cmake)
// ----------------------------------------------------------------

test('unanchored glob: config/foo.cmake matches *.cmake', () => {
  assert.strictEqual(matchesPattern('config/foo.cmake', '*.cmake'), true);
});

test('unanchored glob: a/b/deep/x.cmake matches *.cmake', () => {
  assert.strictEqual(matchesPattern('a/b/deep/x.cmake', '*.cmake'), true);
});

test('unanchored glob: src/H5public.h does not match *.cmake', () => {
  assert.strictEqual(matchesPattern('src/H5public.h', '*.cmake'), false);
});

test('unanchored glob: config/foo.cmake with pattern /config/*.cmake is anchored, not a bare wildcard', () => {
  // The spec lists this under the unanchored-glob section to contrast *.cmake (matches anywhere)
  // with /config/*.cmake (anchored prefix glob, a distinct category). The definitive expected
  // value from the "Anchored glob with path prefix" section (and the implementation) is true.
  assert.strictEqual(matchesPattern('config/foo.cmake', '/config/*.cmake'), true);
});

// ----------------------------------------------------------------
// matchesPattern — anchored glob with path prefix
// ----------------------------------------------------------------

test('anchored prefix glob: config/cmake/foo.cmake does not match /config/*.cmake (subdir, * no cross /)', () => {
  assert.strictEqual(matchesPattern('config/cmake/foo.cmake', '/config/*.cmake'), false);
});

test('anchored prefix glob: config/foo.cmake matches /config/*.cmake', () => {
  assert.strictEqual(matchesPattern('config/foo.cmake', '/config/*.cmake'), true);
});

// ----------------------------------------------------------------
// matchesPattern — double-star glob
// ----------------------------------------------------------------

test('double-star glob: src/deep/nested/file.h matches /src/**/*.h', () => {
  assert.strictEqual(matchesPattern('src/deep/nested/file.h', '/src/**/*.h'), true);
});

test('double-star glob: docs/file.h does not match /src/**/*.h', () => {
  assert.strictEqual(matchesPattern('docs/file.h', '/src/**/*.h'), false);
});

test('double-star glob: src/file.h matches /src/**/*.h (zero-depth directory)', () => {
  assert.strictEqual(matchesPattern('src/file.h', '/src/**/*.h'), true);
});

test('unanchored directory: tools/src/foo.c matches src/', () => {
  assert.strictEqual(matchesPattern('tools/src/foo.c', 'src/'), true);
});

// ----------------------------------------------------------------
// labelFromPattern
// ----------------------------------------------------------------

test('labelFromPattern: /fortran/ => "fortran"', () => {
  assert.strictEqual(labelFromPattern('/fortran/'), 'fortran');
});

test('labelFromPattern: *.cmake => "*.cmake"', () => {
  assert.strictEqual(labelFromPattern('*.cmake'), '*.cmake');
});

test('labelFromPattern: /CMakeLists.txt => "CMakeLists.txt"', () => {
  assert.strictEqual(labelFromPattern('/CMakeLists.txt'), 'CMakeLists.txt');
});

// ----------------------------------------------------------------
// attributeFiles
// ----------------------------------------------------------------

test('attributeFiles: file goes to most-precedent (last) matching area', () => {
  const areas = [
    { pattern: '/src/',             label: 'src',            owners: ['alice'] },
    { pattern: '/src/H5FDsubfiling/', label: 'src/H5FDsubfiling', owners: ['bob'] },
  ];
  const files = [{ filename: 'src/H5FDsubfiling/foo.c', changes: 10 }];
  const byArea = attributeFiles(files, areas);
  assert.strictEqual(byArea.get('/src/').length, 0);
  assert.strictEqual(byArea.get('/src/H5FDsubfiling/').length, 1);
});

test('attributeFiles: file in /src/ is not stolen by /src/H5FDsubfiling/', () => {
  const areas = [
    { pattern: '/src/',             label: 'src',            owners: ['alice'] },
    { pattern: '/src/H5FDsubfiling/', label: 'src/H5FDsubfiling', owners: ['bob'] },
  ];
  const files = [{ filename: 'src/H5public.h', changes: 5 }];
  const byArea = attributeFiles(files, areas);
  assert.strictEqual(byArea.get('/src/').length, 1);
  assert.strictEqual(byArea.get('/src/H5FDsubfiling/').length, 0);
});

test('attributeFiles: unmatched file appears in no area', () => {
  const areas = [{ pattern: '/src/', label: 'src', owners: ['alice'] }];
  const files = [{ filename: 'fortran/H5f.F90', changes: 3 }];
  const byArea = attributeFiles(files, areas);
  assert.strictEqual(byArea.get('/src/').length, 0);
});

// ----------------------------------------------------------------
// computeApprovals
// ----------------------------------------------------------------

test('computeApprovals: basic approval', () => {
  const approved = computeApprovals([{ user: { login: 'alice' }, state: 'APPROVED' }]);
  assert.ok(approved.has('alice'));
});

test('computeApprovals: CHANGES_REQUESTED after APPROVED cancels approval', () => {
  const approved = computeApprovals([
    { user: { login: 'alice' }, state: 'APPROVED' },
    { user: { login: 'alice' }, state: 'CHANGES_REQUESTED' },
  ]);
  assert.strictEqual(approved.has('alice'), false);
});

test('computeApprovals: DISMISSED after APPROVED cancels approval', () => {
  const approved = computeApprovals([
    { user: { login: 'alice' }, state: 'APPROVED' },
    { user: { login: 'alice' }, state: 'DISMISSED' },
  ]);
  assert.strictEqual(approved.has('alice'), false);
});

test('computeApprovals: COMMENTED after APPROVED does not cancel approval', () => {
  const approved = computeApprovals([
    { user: { login: 'alice' }, state: 'APPROVED' },
    { user: { login: 'alice' }, state: 'COMMENTED' },
  ]);
  assert.ok(approved.has('alice'));
});

test('computeApprovals: null user is skipped (ghost / deleted account)', () => {
  const approved = computeApprovals([
    { user: null, state: 'APPROVED' },
    { user: { login: 'bob' }, state: 'APPROVED' },
  ]);
  assert.ok(approved.has('bob'));
  assert.strictEqual(approved.size, 1);
});

test('computeApprovals: independent approvals from two users', () => {
  const approved = computeApprovals([
    { user: { login: 'alice' }, state: 'APPROVED' },
    { user: { login: 'bob' },   state: 'APPROVED' },
  ]);
  assert.ok(approved.has('alice'));
  assert.ok(approved.has('bob'));
});

// ----------------------------------------------------------------
// computeChangesRequested
// ----------------------------------------------------------------

test('computeChangesRequested: basic change request', () => {
  const changesRequested = computeChangesRequested([{ user: { login: 'alice' }, state: 'CHANGES_REQUESTED' }]);
  assert.ok(changesRequested.has('alice'));
});

test('computeChangesRequested: APPROVED after CHANGES_REQUESTED clears it', () => {
  const changesRequested = computeChangesRequested([
    { user: { login: 'alice' }, state: 'CHANGES_REQUESTED' },
    { user: { login: 'alice' }, state: 'APPROVED' },
  ]);
  assert.strictEqual(changesRequested.has('alice'), false);
});

test('computeChangesRequested: DISMISSED after CHANGES_REQUESTED clears it', () => {
  const changesRequested = computeChangesRequested([
    { user: { login: 'alice' }, state: 'CHANGES_REQUESTED' },
    { user: { login: 'alice' }, state: 'DISMISSED' },
  ]);
  assert.strictEqual(changesRequested.has('alice'), false);
});

test('computeChangesRequested: drive-by reviewer (never a requested reviewer) still counts', () => {
  const changesRequested = computeChangesRequested([
    { user: { login: 'driveby' }, state: 'CHANGES_REQUESTED' },
  ]);
  assert.ok(changesRequested.has('driveby'));
});

test('computeChangesRequested: null user is skipped (ghost / deleted account)', () => {
  const changesRequested = computeChangesRequested([
    { user: null, state: 'CHANGES_REQUESTED' },
    { user: { login: 'bob' }, state: 'CHANGES_REQUESTED' },
  ]);
  assert.strictEqual(changesRequested.size, 1);
  assert.ok(changesRequested.has('bob'));
});

// ----------------------------------------------------------------
// buildChangeRequestFileMap
// ----------------------------------------------------------------

test('buildChangeRequestFileMap: maps a change-requester to their commented files', () => {
  const map = buildChangeRequestFileMap(
    [{ user: { login: 'alice' }, path: 'src/H5F.c' }],
    new Set(['alice'])
  );
  assert.ok(map.get('alice').has('src/H5F.c'));
});

test('buildChangeRequestFileMap: excludes commenters not in changesRequestedUsers', () => {
  const map = buildChangeRequestFileMap(
    [{ user: { login: 'bob' }, path: 'src/H5F.c' }],
    new Set(['alice'])
  );
  assert.strictEqual(map.has('bob'), false);
});

test('buildChangeRequestFileMap: skips comments with a null user (ghost / deleted account)', () => {
  const map = buildChangeRequestFileMap(
    [{ user: null, path: 'src/H5F.c' }],
    new Set(['alice'])
  );
  assert.strictEqual(map.size, 0);
});

test('buildChangeRequestFileMap: aggregates multiple files from the same reviewer', () => {
  const map = buildChangeRequestFileMap(
    [
      { user: { login: 'alice' }, path: 'src/H5F.c' },
      { user: { login: 'alice' }, path: 'src/H5D.c' },
    ],
    new Set(['alice'])
  );
  assert.strictEqual(map.get('alice').size, 2);
});

// ----------------------------------------------------------------
// chooseReviewers helpers
// ----------------------------------------------------------------

function makeArea(label, owners, linesChanged, files) {
  return { pattern: `/${label}/`, label, owners, linesChanged, files: files || [] };
}

const BASE_CONFIG = {
  prAuthor: 'charlie',
  existingRequested: new Set(),
  reviewerLoad: {},
  LINE_THRESHOLD: 300,
  AREA_THRESHOLDS: {},
  PUBLIC_HEADER: /public\.h$/,
};

// ----------------------------------------------------------------
// chooseReviewers
// ----------------------------------------------------------------

test('chooseReviewers: complex area (lines >= threshold) picks first non-author owner', () => {
  const area = makeArea('src', ['alice', 'bob'], 400);
  const { selected } = chooseReviewers([area], { ...BASE_CONFIG, prAuthor: 'bob' });
  assert.ok(selected.has('alice'));
  assert.strictEqual(selected.has('bob'), false);
});

test('chooseReviewers: linesChanged === threshold is complex (boundary >=)', () => {
  const area = makeArea('src', ['alice'], 300);
  const { selected, log } = chooseReviewers([area], { ...BASE_CONFIG });
  assert.ok(selected.has('alice'));
  assert.ok(log.some(l => l.includes('complex')));
});

test('chooseReviewers: linesChanged === threshold - 1 is NOT complex', () => {
  const area = makeArea('src', ['alice'], 299);
  const { selected, log } = chooseReviewers([area], { ...BASE_CONFIG });
  assert.ok(selected.has('alice'));
  assert.ok(!log.some(l => l.includes('complex')));
});

test('chooseReviewers: public header triggers complexity regardless of line count', () => {
  const area = makeArea('src', ['alice', 'bob'], 1, [{ filename: 'src/H5public.h', changes: 1 }]);
  const { selected } = chooseReviewers([area], { ...BASE_CONFIG });
  assert.ok(selected.has('alice'));
});

test('chooseReviewers: per-area threshold override (test area at 400 lines is NOT complex at 500 threshold)', () => {
  const area = makeArea('test', ['alice'], 400);
  const { selected, log } = chooseReviewers([area], {
    ...BASE_CONFIG,
    AREA_THRESHOLDS: { test: 500 },
  });
  assert.ok(selected.has('alice'));
  assert.ok(!log.some(l => l.includes('complex')));
});

test('chooseReviewers: per-area threshold override (test area at 500 lines IS complex at 500 threshold)', () => {
  const area = makeArea('test', ['alice'], 500);
  const { selected, log } = chooseReviewers([area], {
    ...BASE_CONFIG,
    AREA_THRESHOLDS: { test: 500 },
  });
  assert.ok(selected.has('alice'));
  assert.ok(log.some(l => l.includes('complex')));
});

test('chooseReviewers: cohesion reuses already-selected owner for second area', () => {
  const areas = [
    makeArea('src',  ['alice', 'bob'],     10),
    makeArea('test', ['alice', 'charlie'], 10),
  ];
  const { selected } = chooseReviewers(areas, { ...BASE_CONFIG });
  // First area load-balances to alice (equal loads, alice is first).
  // Second area reuses alice via cohesion instead of picking charlie.
  assert.ok(selected.has('alice'));
  assert.strictEqual(selected.has('charlie'), false);
  assert.strictEqual(selected.size, 1);
});

test('chooseReviewers: load-balanced pick selects owner with fewer open requests', () => {
  const area = makeArea('src', ['alice', 'bob'], 10);
  const { selected } = chooseReviewers([area], {
    ...BASE_CONFIG,
    reviewerLoad: { alice: 5, bob: 2 },
  });
  assert.ok(selected.has('bob'));
  assert.strictEqual(selected.has('alice'), false);
});

test('chooseReviewers: tie in load broken by CODEOWNERS order (first-listed wins)', () => {
  const area = makeArea('src', ['alice', 'bob'], 10);
  const { selected } = chooseReviewers([area], {
    ...BASE_CONFIG,
    reviewerLoad: { alice: 3, bob: 3 },
  });
  assert.ok(selected.has('alice'));
});

test('chooseReviewers: author-is-sole-owner produces empty selection without crash', () => {
  const area = makeArea('src', ['alice'], 10);
  const { selected, log } = chooseReviewers([area], { ...BASE_CONFIG, prAuthor: 'alice' });
  assert.strictEqual(selected.size, 0);
  assert.ok(log.some(l => l.includes('all owners are the PR author')));
});

test('chooseReviewers: area already in existingRequested is skipped', () => {
  const area = makeArea('src', ['alice', 'bob'], 10);
  const { selected } = chooseReviewers([area], {
    ...BASE_CONFIG,
    existingRequested: new Set(['alice']),
  });
  assert.strictEqual(selected.size, 0);
});

test('chooseReviewers: updatedRequested contains both existing and newly selected', () => {
  const area = makeArea('src', ['alice'], 10);
  const { updatedRequested } = chooseReviewers([area], {
    ...BASE_CONFIG,
    existingRequested: new Set(['bob']),
  });
  assert.ok(updatedRequested.has('bob'));
  assert.ok(updatedRequested.has('alice'));
});

// ----------------------------------------------------------------
// buildBody
// ----------------------------------------------------------------

test('buildBody: unchecked area shows open box and owner mention', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(), new Set(['alice']));
  assert.ok(body.includes('- [ ] **src**'));
  assert.ok(body.includes('— @alice'));
  assert.ok(!body.includes('✅'));
});

test('buildBody: approved area shows checked box and tick', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(['alice']), new Set(['alice']));
  assert.ok(body.includes('- [x] **src** ✅'));
});

test('buildBody: all areas done appends global sign-off line', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(['alice']), new Set(['alice']));
  assert.ok(body.includes('> ✅ All areas have been signed off.'));
});

test('buildBody: partial approval does not show global sign-off line', () => {
  const areas = [
    makeArea('src',  ['alice'], 10),
    makeArea('test', ['bob'],   10),
  ];
  const body = buildBody(areas, new Set(['alice']), new Set(['alice', 'bob']));
  assert.ok(!body.includes('> ✅ All areas have been signed off.'));
});

test('buildBody: area with no confirmed reviewer shows no @-mention', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(), new Set());
  assert.ok(body.includes('- [ ] **src**'));
  assert.ok(!body.includes('@alice'));
});

test('buildBody: mention shows approver when a non-requested owner signs off', () => {
  // alice was load-balanced as the reviewer; bob (also an owner) approves instead
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body  = buildBody(areas, new Set(['bob']), new Set(['alice']));
  assert.ok(body.includes('- [x] **src** ✅'));
  assert.ok(body.includes('— @bob'));
  assert.ok(!body.includes('@alice'));
});

test('buildBody: shows multiple requested reviewers when more than one is assigned', () => {
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body  = buildBody(areas, new Set(), new Set(['alice', 'bob']));
  assert.ok(body.includes('— @alice, @bob'));
});

test('buildBody: always contains the marker', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(), new Set());
  assert.ok(body.includes('<!-- hdf5-review-checklist-v1 -->'));
});

// Non-CODEOWNER reviewer fallback (added by #6446): when no owner of the area
// is in confirmedRequested, a manually-assigned non-owner reviewer is shown and
// their approval counts as sign-off.

test('buildBody: non-owner reviewer shown as pending when no area owner is assigned', () => {
  // alice owns /src/ but was not requested; charlie (not an owner) was manually assigned
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(), new Set(['charlie']));
  assert.ok(body.includes('- [ ] **src**'));
  assert.ok(body.includes('— @charlie'));
  assert.ok(!body.includes('@alice'));
});

test('buildBody: non-owner reviewer approval signs off area when no CODEOWNER is assigned', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(['charlie']), new Set(['charlie']));
  assert.ok(body.includes('- [x] **src** ✅'));
  assert.ok(body.includes('— @charlie'));
});

test('buildBody: non-owner reviewer approval does NOT sign off when a CODEOWNER was assigned', () => {
  // alice (owner) was assigned; charlie (non-owner) also approves — alice's sign-off is still required
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(['charlie']), new Set(['alice', 'charlie']));
  const srcRow = body.split('\n').find(l => l.startsWith('- ['));
  assert.ok(srcRow.startsWith('- [ ] **src**'));
  assert.ok(!srcRow.includes('✅'));
});

// "Additional reviewers" line: a requested reviewer who isn't an owner of any
// touched area and wasn't pulled in as a no-CODEOWNER fallback either (e.g. a
// project lead added by hand for unrelated areas, not path ownership).

test('buildBody: reviewer not tied to any area appears in Additional reviewers line', () => {
  const areas = [makeArea('tools', ['mattjala'], 10), makeArea('release_docs', ['lrknox'], 5)];
  const body  = buildBody(areas, new Set(), new Set(['mattjala', 'lrknox', 'fortnern']));
  assert.ok(body.includes('**Additional reviewers** (not owners of a touched area): @fortnern'));
});

test('buildBody: Additional reviewers approval is shown with a checkmark', () => {
  const areas = [makeArea('tools', ['mattjala'], 10)];
  const body  = buildBody(areas, new Set(['fortnern']), new Set(['mattjala', 'fortnern']));
  assert.ok(body.includes('@fortnern ✅'));
  const toolsRow = body.split('\n').find(l => l.startsWith('- ['));
  assert.ok(!toolsRow.includes('✅')); // fortnern's approval doesn't sign off an area he doesn't own
});

test('buildBody: no Additional reviewers line when every reviewer owns a touched area', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(), new Set(['alice']));
  assert.ok(!body.includes('Additional reviewers'));
});

test('buildBody: reviewer used as no-CODEOWNER fallback is not double-listed as an extra', () => {
  const areas = [makeArea('orphan', ['nobody_requested'], 10)];
  const body  = buildBody(areas, new Set(), new Set(['charlie']));
  assert.ok(!body.includes('Additional reviewers'));
  assert.ok(body.includes('— @charlie'));
});

// Change-requester sub-lines: someone with an outstanding CHANGES_REQUESTED
// review is listed on a separate line under each area their inline comments
// touch — whether or not they're a CODEOWNER or a requested reviewer at all.

test('buildBody: change-requester on an area file gets a sub-line under that area', () => {
  const areas = [makeArea('src', ['alice'], 10, [{ filename: 'src/H5F.c', changes: 10 }])];
  const changeRequestFiles = new Map([['dan', new Set(['src/H5F.c'])]]);
  const body = buildBody(areas, new Set(), new Set(['alice']), changeRequestFiles);
  assert.ok(body.includes('  - ⚠️ Changes requested by @dan'));
});

test('buildBody: drive-by change-requester (not a CODEOWNER or requested reviewer) still gets a sub-line', () => {
  const areas = [makeArea('src', ['alice'], 10, [{ filename: 'src/H5F.c', changes: 10 }])];
  const changeRequestFiles = new Map([['driveby', new Set(['src/H5F.c'])]]);
  const body = buildBody(areas, new Set(), new Set(['alice']), changeRequestFiles);
  assert.ok(body.includes('  - ⚠️ Changes requested by @driveby'));
});

test('buildBody: change-requester is scoped to the area their comments touch, not every area', () => {
  const areas = [
    makeArea('src',     ['alice'], 10, [{ filename: 'src/H5F.c', changes: 10 }]),
    makeArea('fortran', ['bob'],   10, [{ filename: 'fortran/H5f.F90', changes: 10 }]),
  ];
  const changeRequestFiles = new Map([['dan', new Set(['src/H5F.c'])]]);
  const body = buildBody(areas, new Set(), new Set(['alice', 'bob']), changeRequestFiles);
  const lines = body.split('\n');
  const srcIdx     = lines.findIndex(l => l.includes('**src**'));
  const fortranIdx = lines.findIndex(l => l.includes('**fortran**'));
  assert.ok(lines[srcIdx + 1].includes('⚠️ Changes requested by @dan'));
  assert.ok(!lines[fortranIdx + 1] || !lines[fortranIdx + 1].includes('@dan'));
});

test('buildBody: multiple change-requesters on the same area each get their own line', () => {
  const areas = [makeArea('src', ['alice'], 10, [{ filename: 'src/H5F.c', changes: 10 }])];
  const changeRequestFiles = new Map([
    ['dan',  new Set(['src/H5F.c'])],
    ['erin', new Set(['src/H5F.c'])],
  ]);
  const body = buildBody(areas, new Set(), new Set(['alice']), changeRequestFiles);
  assert.ok(body.includes('  - ⚠️ Changes requested by @dan'));
  assert.ok(body.includes('  - ⚠️ Changes requested by @erin'));
});

test('buildBody: no change-requesters produces no sub-lines and omitting the param is safe', () => {
  const areas = [makeArea('src', ['alice'], 10, [{ filename: 'src/H5F.c', changes: 10 }])];
  const body  = buildBody(areas, new Set(), new Set(['alice']));
  assert.ok(!body.includes('⚠️'));
});

test('buildBody: change-requester with no commented files in any touched area gets no sub-line', () => {
  const areas = [makeArea('src', ['alice'], 10, [{ filename: 'src/H5F.c', changes: 10 }])];
  const changeRequestFiles = new Map([['dan', new Set(['unrelated/file.c'])]]);
  const body = buildBody(areas, new Set(), new Set(['alice']), changeRequestFiles);
  assert.ok(!body.includes('⚠️'));
});

// Manually-added-CODEOWNER sub-lines: a CODEOWNER who was review-requested
// directly by a human (rather than auto-picked) gets their own required-
// approval line, and the area doesn't sign off until they've approved too.

test('buildBody: manually-added CODEOWNER gets a separate required-approval line', () => {
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body = buildBody(areas, new Set(), new Set(['alice', 'bob']), new Map(), new Set(['bob']));
  assert.ok(body.includes('  - [ ] @bob (manually added) — approval required'));
});

test('buildBody: manually-added CODEOWNER is left out of the main mention line', () => {
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body = buildBody(areas, new Set(), new Set(['alice', 'bob']), new Map(), new Set(['bob']));
  const mainRow = body.split('\n').find(l => l.startsWith('- ['));
  assert.ok(mainRow.includes('— @alice'));
  assert.ok(!mainRow.includes('@bob'));
});

test('buildBody: area does not sign off when auto-pick approved but manually-added CODEOWNER has not', () => {
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body = buildBody(areas, new Set(['alice']), new Set(['alice', 'bob']), new Map(), new Set(['bob']));
  const mainRow = body.split('\n').find(l => l.startsWith('- ['));
  assert.ok(mainRow.startsWith('- [ ] **src**'));
  assert.ok(!mainRow.includes('✅'));
  assert.ok(body.includes('  - [ ] @bob (manually added) — approval required'));
});

test('buildBody: area signs off once both the auto-pick and the manually-added CODEOWNER approve', () => {
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body = buildBody(areas, new Set(['alice', 'bob']), new Set(['alice', 'bob']), new Map(), new Set(['bob']));
  const mainRow = body.split('\n').find(l => l.startsWith('- ['));
  assert.ok(mainRow.startsWith('- [x] **src** ✅'));
  assert.ok(body.includes('  - [x] @bob (manually added) ✅'));
});

test('buildBody: no manually-added CODEOWNERS produces no sub-lines and omitting the param is safe', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body = buildBody(areas, new Set(), new Set(['alice']));
  assert.ok(!body.includes('manually added'));
});

test('buildBody: manually-added sole owner of an area only needs their own approval', () => {
  // alice is the only owner of this area and was manually requested — no
  // separate auto-pick exists, so her own approval alone should sign it off.
  const areas = [makeArea('src', ['alice'], 10)];
  const body = buildBody(areas, new Set(['alice']), new Set(['alice']), new Map(), new Set(['alice']));
  const mainRow = body.split('\n').find(l => l.startsWith('- ['));
  assert.ok(mainRow.startsWith('- [x] **src** ✅'));
  assert.ok(body.includes('  - [x] @alice (manually added) ✅'));
});

test('buildBody: a manually-added CODEOWNER who is no longer in confirmedRequested (removed) gets no line', () => {
  // bob was manually added at some point (still in the persisted marker) but
  // has since been removed from the PR — confirmedRequested no longer has
  // him, so he must not show up as still owing an approval.
  const areas = [makeArea('src', ['alice', 'bob'], 10)];
  const body = buildBody(areas, new Set(), new Set(['alice']), new Map(), new Set(['bob']));
  assert.ok(!body.includes('bob'));
  assert.ok(!body.includes('manually added'));
});

// ----------------------------------------------------------------
// parseExcluded / serializeExcluded — persisted "explicitly removed" list
// ----------------------------------------------------------------

test('parseExcluded: no comment body yet returns an empty set', () => {
  const excluded = parseExcluded(undefined);
  assert.strictEqual(excluded.size, 0);
});

test('parseExcluded: comment with no exclusion marker returns an empty set', () => {
  const excluded = parseExcluded('<!-- hdf5-review-checklist-v1 -->\nsome body text');
  assert.strictEqual(excluded.size, 0);
});

test('parseExcluded: extracts logins from the hidden marker', () => {
  const body = '<!-- hdf5-review-checklist-v1 -->\nbody\n<!-- hdf5-review-checklist-excluded:alice,bob-->';
  const excluded = parseExcluded(body);
  assert.ok(excluded.has('alice'));
  assert.ok(excluded.has('bob'));
  assert.strictEqual(excluded.size, 2);
});

test('parseExcluded: empty exclusion list round-trips to an empty set', () => {
  const body = '<!-- hdf5-review-checklist-v1 -->\nbody\n<!-- hdf5-review-checklist-excluded:-->';
  const excluded = parseExcluded(body);
  assert.strictEqual(excluded.size, 0);
});

test('serializeExcluded: empty set produces the empty marker', () => {
  assert.strictEqual(serializeExcluded(new Set()), '<!-- hdf5-review-checklist-excluded:-->');
});

test('serializeExcluded: round-trips through parseExcluded', () => {
  const original = new Set(['alice', 'bob']);
  const roundTripped = parseExcluded(serializeExcluded(original));
  assert.deepStrictEqual([...roundTripped].sort(), ['alice', 'bob']);
});

// ----------------------------------------------------------------
// withExcluded — used by /remove-reviewer to persist a deliberate removal
// into the checklist comment's exclusion marker.
// ----------------------------------------------------------------

test('withExcluded: replaces an existing marker in place, preserving the rest of the body', () => {
  const body = `${MARKER}\nsome checklist text\n<!-- hdf5-review-checklist-excluded:alice-->`;
  const updated = withExcluded(body, new Set(['alice', 'bob']));
  assert.ok(updated.includes('some checklist text'));
  assert.ok(updated.startsWith(MARKER));
  assert.deepStrictEqual([...parseExcluded(updated)].sort(), ['alice', 'bob']);
  // Only one marker present afterward — not appended alongside the old one.
  assert.strictEqual(updated.split('hdf5-review-checklist-excluded:').length - 1, 1);
});

test('withExcluded: appends a marker when the body has none', () => {
  const body = `${MARKER}\nsome checklist text`;
  const updated = withExcluded(body, new Set(['alice']));
  assert.ok(updated.includes('some checklist text'));
  assert.deepStrictEqual([...parseExcluded(updated)], ['alice']);
});

test('withExcluded: round-trips an empty set to the empty marker', () => {
  const body = `${MARKER}\ntext\n<!-- hdf5-review-checklist-excluded:alice-->`;
  const updated = withExcluded(body, new Set());
  assert.strictEqual(parseExcluded(updated).size, 0);
});

// ----------------------------------------------------------------
// parseManuallyAdded / serializeManuallyAdded — persisted "manually
// requested CODEOWNER" list (mirrors parseExcluded / serializeExcluded).
// ----------------------------------------------------------------

test('parseManuallyAdded: no comment body yet returns an empty set', () => {
  const manuallyAdded = parseManuallyAdded(undefined);
  assert.strictEqual(manuallyAdded.size, 0);
});

test('parseManuallyAdded: comment with no manual marker returns an empty set', () => {
  const manuallyAdded = parseManuallyAdded('<!-- hdf5-review-checklist-v1 -->\nsome body text');
  assert.strictEqual(manuallyAdded.size, 0);
});

test('parseManuallyAdded: extracts logins from the hidden marker', () => {
  const body = '<!-- hdf5-review-checklist-v1 -->\nbody\n<!-- hdf5-review-checklist-manual:alice,bob-->';
  const manuallyAdded = parseManuallyAdded(body);
  assert.ok(manuallyAdded.has('alice'));
  assert.ok(manuallyAdded.has('bob'));
  assert.strictEqual(manuallyAdded.size, 2);
});

test('serializeManuallyAdded: empty set produces the empty marker', () => {
  assert.strictEqual(serializeManuallyAdded(new Set()), '<!-- hdf5-review-checklist-manual:-->');
});

test('serializeManuallyAdded: round-trips through parseManuallyAdded', () => {
  const original = new Set(['alice', 'bob']);
  const roundTripped = parseManuallyAdded(serializeManuallyAdded(original));
  assert.deepStrictEqual([...roundTripped].sort(), ['alice', 'bob']);
});

// ----------------------------------------------------------------
// planSynchronizeSwaps
// ----------------------------------------------------------------

function makeSyncCtx(overrides) {
  return {
    prAuthor:        'lrknox',
    existingRequested: new Set(),
    updatedExcluded: new Set(),
    touchedAreaOwners: new Set(['jhendersonHDF', 'hyoklee', 'glennsong09', 'lrknox']),
    ...overrides,
  };
}

test('planSynchronizeSwaps: dismissed reviewer swaps out fresh CODEOWNERS pick (PR 6475 scenario)', () => {
  // Jordan reviewed and got dismissed; GitHub auto-assigned Joe (hyoklee) for the fixup push.
  const areas = [makeArea('.github', ['hyoklee', 'lrknox', 'jhendersonHDF', 'glennsong09'], 5)];
  const reviews = [{ user: { login: 'jhendersonHDF' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({ existingRequested: new Set(['hyoklee']) });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 1);
  assert.strictEqual(swaps[0].dismissedOwner, 'jhendersonHDF');
  assert.strictEqual(swaps[0].freshPick, 'hyoklee');
});

test('planSynchronizeSwaps: no swaps when dismissed reviewer is already re-requested', () => {
  const areas = [makeArea('.github', ['hyoklee', 'jhendersonHDF'], 5)];
  const reviews = [{ user: { login: 'jhendersonHDF' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({ existingRequested: new Set(['jhendersonHDF']) });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 0);
});

test('planSynchronizeSwaps: no fresh pick when GitHub assigned no one for the area', () => {
  // Dismissed reviewer exists but GitHub didn't auto-assign anyone new.
  const areas = [makeArea('.github', ['hyoklee', 'jhendersonHDF'], 5)];
  const reviews = [{ user: { login: 'jhendersonHDF' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({ existingRequested: new Set() });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 1);
  assert.strictEqual(swaps[0].dismissedOwner, 'jhendersonHDF');
  assert.strictEqual(swaps[0].freshPick, null);
});

test('planSynchronizeSwaps: dismissed reviewer who is excluded is skipped', () => {
  const areas = [makeArea('.github', ['hyoklee', 'jhendersonHDF'], 5)];
  const reviews = [{ user: { login: 'jhendersonHDF' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({
    existingRequested: new Set(['hyoklee']),
    updatedExcluded: new Set(['jhendersonHDF']),
  });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 0);
});

test('planSynchronizeSwaps: PR author as dismissed reviewer is skipped', () => {
  const areas = [makeArea('.github', ['lrknox', 'jhendersonHDF'], 5)];
  const reviews = [{ user: { login: 'lrknox' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({ existingRequested: new Set(['jhendersonHDF']) });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 0);
});

test('planSynchronizeSwaps: no dismissed reviews produces no swaps', () => {
  const areas = [makeArea('.github', ['hyoklee', 'jhendersonHDF'], 5)];
  const reviews = [{ user: { login: 'jhendersonHDF' }, state: 'APPROVED' }];
  const ctx = makeSyncCtx({ existingRequested: new Set(['hyoklee']) });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 0);
});

test('planSynchronizeSwaps: manually added non-CODEOWNER is not treated as a fresh pick', () => {
  // 'outsider' is in existingRequested but NOT in touchedAreaOwners (manually added).
  // Should not be removed as a fresh CODEOWNERS pick.
  const areas = [makeArea('.github', ['hyoklee', 'jhendersonHDF'], 5)];
  const reviews = [{ user: { login: 'jhendersonHDF' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({
    existingRequested: new Set(['outsider']),
    touchedAreaOwners: new Set(['hyoklee', 'jhendersonHDF', 'glennsong09']),
  });
  const swaps = planSynchronizeSwaps(areas, reviews, ctx);
  assert.strictEqual(swaps.length, 1);
  assert.strictEqual(swaps[0].freshPick, null); // outsider not swapped out
});

test('planSynchronizeSwaps: fresh pick covering a second, unrelated touched area is not removed', () => {
  // joe covers both areas; jordan was dismissed only on area "a". Removing joe
  // to restore jordan would silently uncover area "b", which has nothing to
  // do with the dismissal.
  const areaA = makeArea('a', ['jordan', 'joe'], 5);
  const areaB = makeArea('b', ['joe', 'glenn'], 5);
  const reviews = [{ user: { login: 'jordan' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({
    prAuthor: 'author',
    existingRequested: new Set(['joe']),
    touchedAreaOwners: new Set(['jordan', 'joe', 'glenn']),
  });
  const swaps = planSynchronizeSwaps([areaA, areaB], reviews, ctx);
  assert.strictEqual(swaps.length, 1);
  assert.strictEqual(swaps[0].area.label, 'a');
  assert.strictEqual(swaps[0].dismissedOwner, 'jordan');
  assert.strictEqual(swaps[0].freshPick, null); // joe is NOT removed — still needed for area "b"
});

test('planSynchronizeSwaps: fresh pick is removed when not needed by any other area', () => {
  // Sanity check that the "needed elsewhere" guard doesn't over-fire: when the
  // candidate truly owns only the one area in question, they ARE swapped out.
  const areaA = makeArea('a', ['jordan', 'joe'], 5);
  const areaB = makeArea('b', ['glenn'], 5); // joe is not an owner of area b
  const reviews = [{ user: { login: 'jordan' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({
    prAuthor: 'author',
    existingRequested: new Set(['joe']),
    touchedAreaOwners: new Set(['jordan', 'joe', 'glenn']),
  });
  const swaps = planSynchronizeSwaps([areaA, areaB], reviews, ctx);
  assert.strictEqual(swaps.length, 1);
  assert.strictEqual(swaps[0].freshPick, 'joe');
});

test('planSynchronizeSwaps: two areas independently dismissed for the same owner both restore them', () => {
  // jordan owns both areas and was dismissed on the PR as a whole (one review
  // covers both); each area independently plans to re-request him. The
  // consuming loop re-requesting him twice is idempotent, not a correctness bug.
  const areaA = makeArea('a', ['jordan', 'joe'], 5);
  const areaB = makeArea('b', ['jordan', 'glenn'], 5);
  const reviews = [{ user: { login: 'jordan' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({
    prAuthor: 'author',
    existingRequested: new Set(['joe', 'glenn']),
    touchedAreaOwners: new Set(['jordan', 'joe', 'glenn']),
  });
  const swaps = planSynchronizeSwaps([areaA, areaB], reviews, ctx);
  assert.strictEqual(swaps.length, 2);
  assert.ok(swaps.every(s => s.dismissedOwner === 'jordan'));
});

test('planSynchronizeSwaps: dismissedOwner already covering a different area is not re-flagged', () => {
  // jordan was dismissed but is already requested (e.g. restored by a prior
  // area's swap, or independently re-requested) — no duplicate swap planned.
  const areaA = makeArea('a', ['jordan', 'joe'], 5);
  const reviews = [{ user: { login: 'jordan' }, state: 'DISMISSED' }];
  const ctx = makeSyncCtx({
    prAuthor: 'author',
    existingRequested: new Set(['jordan']),
    touchedAreaOwners: new Set(['jordan', 'joe']),
  });
  const swaps = planSynchronizeSwaps([areaA], reviews, ctx);
  assert.strictEqual(swaps.length, 0);
});

// ----------------------------------------------------------------
// coordinateReviewers — ready_for_review CODEOWNERS-avalanche pruning
// ----------------------------------------------------------------

function makeCoordinateBaseArgs(overrides) {
  const area = makeArea('.github', ['hyoklee', 'lrknox', 'jhendersonHDF', 'glennsong09'], 5);
  return {
    owner: 'HDFGroup', repo: 'hdf5', pr_number: 1,
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'jhendersonHDF' }, { login: 'glennsong09' }],
    },
    allCodeOwners: new Set(['hyoklee', 'lrknox', 'jhendersonHDF', 'glennsong09']),
    catchAllOwners: new Set(),
    touchedAreas: [area],
    reviewerLoad: {},
    excludedReviewers: new Set(),
    allReviews: [],
    // Most scenarios model an already-established PR (checklist already
    // posted); tests exercising the fresh-PR race override this explicitly.
    hasExistingComment: true,
    LINE_THRESHOLD: 300,
    AREA_THRESHOLDS: {},
    PUBLIC_HEADER: /public\.h$/,
    ...overrides,
  };
}

asyncTest('coordinateReviewers: ready_for_review prunes the CODEOWNERS avalanche to one load-balanced pick', async () => {
  const github = makeGithubMock();
  const context = { eventName: 'pull_request_target', payload: { action: 'ready_for_review', sender: { type: 'User' } } };
  const args = makeCoordinateBaseArgs();

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  // hyoklee is first non-author owner in CODEOWNERS order with equal (zero) load.
  assert.deepStrictEqual([...confirmedRequested], ['hyoklee']);
  // The other two avalanche-assigned owners get removed.
  assert.ok(github.calls.removeRequestedReviewers.includes('jhendersonHDF'));
  assert.ok(github.calls.removeRequestedReviewers.includes('glennsong09'));
  assert.strictEqual(github.calls.removeRequestedReviewers.length, 2);
  // hyoklee was already requested, so no redundant request call.
  assert.strictEqual(github.calls.requestReviewers.length, 0);
});

asyncTest('coordinateReviewers: ready_for_review on a draft-opened PR (still draft) does not prune', async () => {
  // Sanity check the branch ordering: if somehow still draft (shouldn't
  // happen for a real ready_for_review payload, but guards the isDraft
  // branch precedence), the draft path's "leave alone" rule wins.
  const github = makeGithubMock();
  const context = { eventName: 'pull_request_target', payload: { action: 'ready_for_review', sender: { type: 'User' } } };
  const args = makeCoordinateBaseArgs({ prData: { ...makeCoordinateBaseArgs().prData, draft: true } });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(github.calls.removeRequestedReviewers.length, 0);
  assert.deepStrictEqual([...confirmedRequested].sort(), ['glennsong09', 'hyoklee', 'jhendersonHDF']);
});

asyncTest('coordinateReviewers: plain synchronize (no dismissed reviews, no avalanche) is left to additive fill', async () => {
  // When only one CODEOWNER is requested per area (normal steady state after
  // prior pruning), a plain synchronize with no dismissed reviews must stay on
  // the additive-fill path: the area is already covered, nothing is removed,
  // nothing new is requested.
  const github = makeGithubMock();
  const context = { eventName: 'pull_request_target', payload: { action: 'synchronize', sender: { type: 'User' } } };
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }], // one .github owner — normal steady state
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(github.calls.removeRequestedReviewers.length, 0);
  assert.strictEqual(github.calls.requestReviewers.length, 0);
  assert.ok(confirmedRequested.has('hyoklee'));
});

asyncTest('coordinateReviewers: review_requested survives the opened race and still prunes (PR #6479 scenario)', async () => {
  // GitHub's CODEOWNERS engine fires one review_requested per auto-assigned
  // owner; each re-triggers this workflow, and concurrency: cancel-in-progress
  // means any of those runs — not necessarily the "opened" run — can be the
  // one that actually executes. hasExistingComment: false (no checklist
  // posted yet) is what lets a surviving review_requested run still prune
  // the avalanche instead of falling through to additive-fill and keeping
  // all three.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'review_requested', requested_reviewer: { login: 'jhendersonHDF' }, sender: { type: 'User' } },
  };
  const args = makeCoordinateBaseArgs({ hasExistingComment: false });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(confirmedRequested.size, 1);
  assert.ok(github.calls.removeRequestedReviewers.length > 0);
});

asyncTest('coordinateReviewers: review_requested on an already-established PR is NOT treated as a fresh-PR prune', async () => {
  // Contrast case: once a checklist comment exists, a routine review_requested
  // later in the PR's life (e.g. a human manually adding a reviewer) must stay
  // on the additive-fill path — it must not be reinterpreted as "first
  // coordination pass" and prune reviewers an established PR already has.
  // Use one requested reviewer per area so the per-area avalanche detector
  // does not also fire — this isolates the isFirstCoordinationPass behavior.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'review_requested', requested_reviewer: { login: 'hyoklee' }, sender: { type: 'User' } },
  };
  const args = makeCoordinateBaseArgs({
    hasExistingComment: true,
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }], // one .github owner — no avalanche to detect
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(github.calls.removeRequestedReviewers.length, 0);
  assert.ok(confirmedRequested.has('hyoklee'));
});

// ----------------------------------------------------------------
// coordinateReviewers — per-area CODEOWNERS avalanche detection (PR #6484)
//
// When a synchronize push first touches a new CODEOWNERS area, GitHub
// auto-assigns ALL that area's owners simultaneously. The surviving
// review_requested run (after cancel-in-progress) may then fall into
// the additive-fill path, see the area as "already has owners → skip",
// and leave all of them listed. The per-area avalanche detector must
// prune that area to one load-balanced pick even on synchronize.
// ----------------------------------------------------------------

asyncTest('coordinateReviewers: synchronize with per-area avalanche prunes the area to one pick', async () => {
  // Model PR #6484: PR has an existing checklist (hasExistingComment: true),
  // a synchronize push touched a new area (.github), GitHub assigned 3 of its
  // owners, the surviving run must prune to one.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'synchronize', sender: { type: 'User' } },
  };
  // Default args: 3 .github owners in requested_reviewers, hasExistingComment: true.
  // That satisfies "existing PR + multiple CODEOWNERS for same area" = avalanche.
  const args = makeCoordinateBaseArgs();

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(confirmedRequested.size, 1, 'Should prune to exactly one reviewer');
  // Exactly 2 removed (the 2 non-picked avalanche owners).
  assert.strictEqual(github.calls.removeRequestedReviewers.length, 2);
  // The kept reviewer is never re-requested (already on the PR).
  assert.strictEqual(github.calls.requestReviewers.length, 0);
});

asyncTest('coordinateReviewers: synchronize with one owner per area does not prune', async () => {
  // Contrast: when each area already has exactly one CODEOWNER requested
  // (normal steady state), synchronize must NOT trigger avalanche pruning.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'synchronize', sender: { type: 'User' } },
  };
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }], // only one .github owner — no avalanche
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  // hyoklee stays, nothing pruned.
  assert.ok(confirmedRequested.has('hyoklee'));
  assert.strictEqual(github.calls.removeRequestedReviewers.length, 0);
});

// ----------------------------------------------------------------
// coordinateReviewers — scope-shrink pruning (PR #6528 scenario): a
// CODEOWNER requested for an area the PR used to touch, before a later push
// narrowed the diff down, is never in touchedAreaOwners (that set only
// reflects areas touched right now) — so none of the avalanche/first-pass
// pruning considers removing them without this dedicated check.
// ----------------------------------------------------------------

asyncTest('coordinateReviewers: reviewer whose area dropped out of scope is removed', async () => {
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'synchronize', sender: { type: 'User' } },
  };
  const args = makeCoordinateBaseArgs({
    // gheber owns /docs/, not the .github area this fixture's touchedAreas
    // covers — modeling a push that dropped doc changes from the diff.
    allCodeOwners: new Set(['hyoklee', 'lrknox', 'jhendersonHDF', 'glennsong09', 'gheber']),
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'gheber' }],
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.ok(github.calls.removeRequestedReviewers.includes('gheber'));
  assert.ok(!confirmedRequested.has('gheber'));
  assert.ok(confirmedRequested.has('hyoklee'));
});

asyncTest('coordinateReviewers: manually-added CODEOWNER survives scope-shrink pruning', async () => {
  // gheber's area is out of scope same as above, but a human deliberately
  // requested him directly — that must not be undone by a later push.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'synchronize', sender: { type: 'User' } },
  };
  const args = makeCoordinateBaseArgs({
    allCodeOwners: new Set(['hyoklee', 'lrknox', 'jhendersonHDF', 'glennsong09', 'gheber']),
    manuallyAdded: new Set(['gheber']),
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'gheber' }],
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.ok(!github.calls.removeRequestedReviewers.includes('gheber'));
  assert.ok(confirmedRequested.has('gheber'));
});

asyncTest('coordinateReviewers: non-CODEOWNER reviewer is untouched by scope-shrink pruning', async () => {
  // driveby isn't a repo CODEOWNER at all (e.g. manually added for judgment,
  // not path ownership) — scope-shrink pruning only concerns itself with
  // CODEOWNERS-based auto-assignment, same as every other pruning path here.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: { action: 'synchronize', sender: { type: 'User' } },
  };
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'driveby' }],
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.ok(!github.calls.removeRequestedReviewers.includes('driveby'));
  assert.ok(confirmedRequested.has('driveby'));
});

// ----------------------------------------------------------------
// coordinateReviewers — a direct review_requested must survive avalanche
// detection on the very same run (reported bug: manually re-requesting a
// reviewer via the GitHub UI got them immediately removed again, because
// their area now had two currently-requested owners — themselves plus
// whichever pick an earlier ready_for_review pruning pass had already made
// — which is indistinguishable from an unpruned CODEOWNERS avalanche unless
// the just-requested login is carved out). The area is still pruned to one
// pick, same as any other avalanche — it's just forced to be the login that
// was directly requested, rather than the load-balancer's own choice.
// ----------------------------------------------------------------

asyncTest('coordinateReviewers: review_requested for a specific login becomes that area\'s forced pick', async () => {
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: {
      action: 'review_requested',
      requested_reviewer: { login: 'jhendersonHDF' },
      sender: { type: 'User' },
    },
  };
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      // hyoklee is the load-balanced pick an earlier pass already made;
      // jhendersonHDF was just manually re-requested on top of it.
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'jhendersonHDF' }],
    },
    // Rig the load-balancer so a fresh pick would land on hyoklee, not
    // jhendersonHDF — proving jhendersonHDF survives because they were
    // just requested, not because they'd have won the pick anyway.
    reviewerLoad: { hyoklee: 0, jhendersonHDF: 99, glennsong09: 0 },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.deepStrictEqual(github.calls.removeRequestedReviewers, ['hyoklee'], 'The old pick is swapped out');
  assert.ok(confirmedRequested.has('jhendersonHDF'), 'The just-requested reviewer must stay');
  assert.ok(!confirmedRequested.has('hyoklee'), 'The area keeps exactly one reviewer');
});

// ----------------------------------------------------------------
// coordinateReviewers — a genuine multi-owner avalanche must still prune to
// one even when the surviving run's own action is one of the avalanche's own
// review_requested sub-events (PR #6530: ready_for_review fired the
// ready_for_review action plus one review_requested per CODEOWNERS owner,
// all near-simultaneously; concurrency: cancel-in-progress let one of the
// review_requested runs survive instead of ready_for_review itself. Since a
// checklist comment already existed from the draft-open phase, that survivor
// fell through past the ready_for_review-only pruning branch entirely).
// ----------------------------------------------------------------

asyncTest('coordinateReviewers: review_requested surviving a ready_for_review avalanche still prunes to one', async () => {
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: {
      action: 'review_requested',
      requested_reviewer: { login: 'jhendersonHDF' },
      sender: { type: 'User' },
    },
  };
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      // GitHub's own CODEOWNERS avalanche assigned all 3 non-author owners —
      // never pruned, since the surviving run isn't the ready_for_review action.
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'jhendersonHDF' }, { login: 'glennsong09' }],
    },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(confirmedRequested.size, 1, 'Should prune to exactly one reviewer');
  assert.ok(confirmedRequested.has('jhendersonHDF'), 'The directly-requested login wins the forced pick');
  assert.strictEqual(github.calls.removeRequestedReviewers.length, 2, 'The other two avalanche owners are removed');
});

asyncTest('coordinateReviewers: bot-sourced review_requested is not treated as a forced pick', async () => {
  // The bot's own requestReviewers calls (e.g. re-requesting a dismissed
  // reviewer, or its normal load-balanced auto-pick) fire this identical
  // review_requested event with a Bot sender — must not be mistaken for a
  // deliberate human override the way a User-sent one is.
  const github = makeGithubMock();
  const context = {
    eventName: 'pull_request_target',
    payload: {
      action: 'review_requested',
      requested_reviewer: { login: 'jhendersonHDF' },
      sender: { type: 'Bot' },
    },
  };
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'jhendersonHDF' }, { login: 'glennsong09' }],
    },
    reviewerLoad: { hyoklee: 0, jhendersonHDF: 99, glennsong09: 0 },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.strictEqual(confirmedRequested.size, 1, 'Should still prune to exactly one reviewer');
  assert.ok(confirmedRequested.has('hyoklee'), 'The normal load-balanced pick wins, not the bot-sourced login');
});

// ----------------------------------------------------------------
// resolveAreaPicks — sticky assignments survive avalanche re-pruning
// (reported bug: a reviewer who was already assigned to an area gets
// replaced by a different load-balanced pick every time a new avalanche is
// detected for that area, because re-pruning re-ran the load-balancer from
// scratch with no memory of who was already there). This is distinct from
// the forced-pick tests above, which only cover the SAME run a human
// directly review-requests someone — these cover persistence across LATER
// runs, which is what ASSIGNED_PREFIX / assignedReviewers exists for.
// ----------------------------------------------------------------

test('resolveAreaPicks: a valid sticky assignment is kept over a fresh load-balanced pick', () => {
  const area = makeArea('fortran', ['alice', 'bob'], 50);
  const { picks, log } = resolveAreaPicks([area], {
    existingRequested: new Set(['alice', 'bob']), // avalanche: both currently requested
    assignedByArea: new Map([['fortran', 'alice']]),
    prAuthor: 'charlie',
    // Rigged so a fresh pick would land on bob, not alice — proves alice
    // survives because of the sticky assignment, not by coincidence.
    reviewerLoad: { alice: 99, bob: 0 },
    LINE_THRESHOLD: 300, AREA_THRESHOLDS: {}, PUBLIC_HEADER: /public\.h$/,
  });
  assert.strictEqual(picks.get('fortran'), 'alice');
  assert.ok(log.some(l => l.includes('sticky')));
});

test('resolveAreaPicks: a sticky assignment no longer requested falls back to a fresh pick', () => {
  // alice was the sticky pick but has since been removed from the PR
  // (e.g. an explicit removal) — must not "keep" someone who isn't there.
  const area = makeArea('fortran', ['alice', 'bob'], 50);
  const { picks } = resolveAreaPicks([area], {
    existingRequested: new Set(['bob']),
    assignedByArea: new Map([['fortran', 'alice']]),
    prAuthor: 'charlie',
    reviewerLoad: {},
    LINE_THRESHOLD: 300, AREA_THRESHOLDS: {}, PUBLIC_HEADER: /public\.h$/,
  });
  assert.strictEqual(picks.get('fortran'), 'bob');
});

test('resolveAreaPicks: a single already-requested owner is kept without invoking the load-balancer', () => {
  const area = makeArea('fortran', ['alice', 'bob'], 50);
  const { picks, log } = resolveAreaPicks([area], {
    existingRequested: new Set(['bob']), // no avalanche — only bob requested
    assignedByArea: new Map(), // no sticky record yet
    prAuthor: 'charlie',
    reviewerLoad: { bob: 99, alice: 0 }, // fresh pick would prefer alice
    LINE_THRESHOLD: 300, AREA_THRESHOLDS: {}, PUBLIC_HEADER: /public\.h$/,
  });
  assert.strictEqual(picks.get('fortran'), 'bob');
  assert.ok(log.some(l => l.includes('no avalanche')));
});

test('resolveAreaPicks: no sticky and no single owner falls back to a fresh load-balanced pick', () => {
  const area = makeArea('fortran', ['alice', 'bob'], 50);
  const { picks } = resolveAreaPicks([area], {
    existingRequested: new Set(['alice', 'bob']),
    assignedByArea: new Map(),
    prAuthor: 'charlie',
    reviewerLoad: { alice: 0, bob: 99 },
    LINE_THRESHOLD: 300, AREA_THRESHOLDS: {}, PUBLIC_HEADER: /public\.h$/,
  });
  assert.strictEqual(picks.get('fortran'), 'alice');
});

// ----------------------------------------------------------------
// parseAssigned / serializeAssigned
// ----------------------------------------------------------------

test('serializeAssigned/parseAssigned round-trip', () => {
  const map = new Map([['fortran', 'alice'], ['.github', 'bob']]);
  const body = `some text\n${serializeAssigned(map)}\nmore text`;
  const parsed = parseAssigned(body);
  assert.strictEqual(parsed.get('fortran'), 'alice');
  assert.strictEqual(parsed.get('.github'), 'bob');
});

test('parseAssigned: no marker returns an empty Map', () => {
  assert.strictEqual(parseAssigned('no marker here').size, 0);
  assert.strictEqual(parseAssigned(undefined).size, 0);
});

// ----------------------------------------------------------------
// coordinateReviewers — sticky assignments across separate coordination
// passes (reported bugs: reviewer churn on later pushes, manual review
// requests getting silently undone, and reviewers vanishing when a draft is
// marked ready for review). The forced-pick tests above only cover survival
// within the SAME run a human directly review-requests someone; these cover
// survival across LATER runs.
// ----------------------------------------------------------------

asyncTest('coordinateReviewers: a settled reviewer survives a later avalanche even when load has shifted', async () => {
  // jhendersonHDF was already the settled reviewer for .github (recorded in
  // assignedReviewers from a prior run). A later push causes GitHub to
  // re-avalanche the area (all owners requested again). Without the sticky
  // record, re-running the load-balancer with jhendersonHDF now heavily
  // loaded would swap them out for glennsong09 — that's the reported bug.
  const github = makeGithubMock();
  const context = { eventName: 'pull_request_target', payload: { action: 'synchronize', sender: { type: 'User' } } };
  const args = makeCoordinateBaseArgs({
    assignedReviewers: new Map([['.github', 'jhendersonHDF']]),
    reviewerLoad: { hyoklee: 0, jhendersonHDF: 50, glennsong09: 0 },
  });

  const { confirmedRequested, assignedReviewers } = await coordinateReviewers(github, context, makeCore(), args);

  assert.ok(confirmedRequested.has('jhendersonHDF'), 'The already-settled reviewer must stay despite higher load');
  assert.ok(!github.calls.removeRequestedReviewers.includes('jhendersonHDF'));
  assert.strictEqual(assignedReviewers.get('.github'), 'jhendersonHDF');
});

asyncTest('coordinateReviewers: a manual review request survives a subsequent avalanche event (not just the same run)', async () => {
  // Step 1: a human manually requests jhendersonHDF — the forced-pick
  // mechanism covers this run (see the forced-pick tests above). The sticky
  // marker must now record jhendersonHDF for .github so a LATER event — one
  // where justRequestedLogin is no longer set — doesn't treat the leftover
  // two-owner state as an unresolved avalanche and re-roll it via the
  // ordinary load-balanced path.
  const github1 = makeGithubMock();
  const context1 = {
    eventName: 'pull_request_target',
    payload: { action: 'review_requested', requested_reviewer: { login: 'jhendersonHDF' }, sender: { type: 'User' } },
  };
  const args1 = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'jhendersonHDF' }],
    },
    reviewerLoad: { hyoklee: 0, jhendersonHDF: 99, glennsong09: 0 },
  });
  const step1 = await coordinateReviewers(github1, context1, makeCore(), args1);
  assert.strictEqual(step1.assignedReviewers.get('.github'), 'jhendersonHDF');

  // Step 2: some unrelated later event (e.g. another push) re-evaluates the
  // PR. Both hyoklee and jhendersonHDF are still requested (GitHub never
  // removed either), which — absent the sticky record from step 1 — looks
  // exactly like an unpruned avalanche.
  const github2 = makeGithubMock();
  const context2 = { eventName: 'pull_request_target', payload: { action: 'synchronize', sender: { type: 'User' } } };
  const args2 = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'jhendersonHDF' }],
    },
    assignedReviewers: step1.assignedReviewers,
    reviewerLoad: { hyoklee: 0, jhendersonHDF: 99, glennsong09: 0 },
  });
  const step2 = await coordinateReviewers(github2, context2, makeCore(), args2);

  assert.ok(step2.confirmedRequested.has('jhendersonHDF'), 'Manually requested reviewer must still survive');
  assert.ok(!github2.calls.removeRequestedReviewers.includes('jhendersonHDF'));
});

asyncTest('coordinateReviewers: ready_for_review keeps an already-settled reviewer instead of re-picking fresh', async () => {
  // A PR sat in draft, was manually assigned jhendersonHDF (recorded as a
  // sticky assignment by an earlier run), and is now marked ready for
  // review. GitHub re-avalanches .github's owners on the ready transition.
  // The old behavior always re-picked fresh here regardless of any prior
  // settlement — this is the bug behind "marking ready for review removes
  // reviewers and replaces them with different people."
  const github = makeGithubMock();
  const context = { eventName: 'pull_request_target', payload: { action: 'ready_for_review', sender: { type: 'User' } } };
  const args = makeCoordinateBaseArgs({
    assignedReviewers: new Map([['.github', 'jhendersonHDF']]),
    // Load favors hyoklee heavily — old code would pick hyoklee fresh.
    reviewerLoad: { hyoklee: 0, jhendersonHDF: 50, glennsong09: 0 },
  });

  const { confirmedRequested } = await coordinateReviewers(github, context, makeCore(), args);

  assert.deepStrictEqual([...confirmedRequested], ['jhendersonHDF']);
  assert.ok(github.calls.removeRequestedReviewers.includes('hyoklee'));
  assert.ok(github.calls.removeRequestedReviewers.includes('glennsong09'));
  assert.ok(!github.calls.removeRequestedReviewers.includes('jhendersonHDF'));
});

// ----------------------------------------------------------------
// coordinateReviewers — bot-self-triggered review_request_removed must not
// create a sticky exclusion (the bot's own removeUnselected/removeRequestedReviewers
// calls fire this very event and would otherwise self-trigger a run that reads
// its own bookkeeping removal as a deliberate human decision).
// ----------------------------------------------------------------

function makeRemovalContext(senderType) {
  return {
    eventName: 'pull_request_target',
    payload: {
      action: 'review_request_removed',
      requested_reviewer: { login: 'jhendersonHDF' },
      sender: { type: senderType },
    },
  };
}

asyncTest('coordinateReviewers: bot-sender review_request_removed does not persist a sticky exclusion', async () => {
  const github = makeGithubMock();
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'glennsong09' }],
    },
  });

  const { excludedReviewers } = await coordinateReviewers(github, makeRemovalContext('Bot'), makeCore(), args);

  assert.ok(!excludedReviewers.has('jhendersonHDF'));
});

asyncTest('coordinateReviewers: human-sender review_request_removed does persist a sticky exclusion', async () => {
  const github = makeGithubMock();
  const args = makeCoordinateBaseArgs({
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'glennsong09' }],
    },
  });

  const { excludedReviewers } = await coordinateReviewers(github, makeRemovalContext('User'), makeCore(), args);

  assert.ok(excludedReviewers.has('jhendersonHDF'));
});

// ----------------------------------------------------------------
// coordinateReviewers — manually-added-CODEOWNER tracking. A human directly
// review-requesting a CODEOWNER (as opposed to the bot's own load-balanced
// requestReviewers call, or GitHub's CODEOWNERS auto-assignment surviving
// the cancel-in-progress race) marks them as needing their own approval —
// see MANUAL_PREFIX and the buildBody tests above.
// ----------------------------------------------------------------

function makeManualAddContext(senderType, login) {
  return {
    eventName: 'pull_request_target',
    payload: {
      action: 'review_requested',
      requested_reviewer: { login },
      sender: { type: senderType },
    },
  };
}

asyncTest('coordinateReviewers: human review_requested for a CODEOWNER marks them manually-added', async () => {
  const github = makeGithubMock();
  const args = makeCoordinateBaseArgs({
    hasExistingComment: true,
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'jhendersonHDF' }], // one .github owner — no avalanche
    },
  });

  const { manuallyAdded } = await coordinateReviewers(
    github, makeManualAddContext('User', 'jhendersonHDF'), makeCore(), args
  );

  assert.ok(manuallyAdded.has('jhendersonHDF'));
});

asyncTest('coordinateReviewers: bot-sender review_requested does NOT mark the reviewer manually-added', async () => {
  // The bot's own load-balanced requestReviewers call fires this identical
  // webhook event with a bot sender — must not be mistaken for a human's
  // deliberate choice, or every auto-picked owner would end up requiring
  // a redundant "manually added" approval.
  const github = makeGithubMock();
  const args = makeCoordinateBaseArgs({
    hasExistingComment: true,
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'jhendersonHDF' }],
    },
  });

  const { manuallyAdded } = await coordinateReviewers(
    github, makeManualAddContext('Bot', 'jhendersonHDF'), makeCore(), args
  );

  assert.ok(!manuallyAdded.has('jhendersonHDF'));
});

asyncTest('coordinateReviewers: review_requested for a non-CODEOWNER does not mark them manually-added', async () => {
  const github = makeGithubMock();
  const args = makeCoordinateBaseArgs({
    hasExistingComment: true,
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'jhendersonHDF' }, { login: 'driveby' }],
    },
  });

  const { manuallyAdded } = await coordinateReviewers(
    github, makeManualAddContext('User', 'driveby'), makeCore(), args
  );

  assert.ok(!manuallyAdded.has('driveby'));
});

asyncTest('coordinateReviewers: human-sender review_request_removed clears a prior manually-added flag', async () => {
  const github = makeGithubMock();
  const args = makeCoordinateBaseArgs({
    manuallyAdded: new Set(['jhendersonHDF']),
    prData: {
      user: { login: 'lrknox' },
      draft: false,
      requested_reviewers: [{ login: 'hyoklee' }, { login: 'glennsong09' }],
    },
  });

  const { manuallyAdded } = await coordinateReviewers(github, makeRemovalContext('User'), makeCore(), args);

  assert.ok(!manuallyAdded.has('jhendersonHDF'));
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
