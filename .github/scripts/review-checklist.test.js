'use strict';
// Run with: node .github/scripts/review-checklist.test.js

const assert = require('assert');
const {
  MARKER,
  matchesPattern,
  labelFromPattern,
  attributeFiles,
  computeApprovals,
  chooseReviewers,
  buildBody,
  parseExcluded,
  serializeExcluded,
  withExcluded,
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
