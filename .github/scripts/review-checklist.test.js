'use strict';
// Run with: node .github/scripts/review-checklist.test.js

const assert = require('assert');
const {
  matchesPattern,
  labelFromPattern,
  attributeFiles,
  computeApprovals,
  chooseReviewers,
  buildBody,
} = require('./review-checklist.js');

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

test('buildBody: always contains the marker', () => {
  const areas = [makeArea('src', ['alice'], 10)];
  const body  = buildBody(areas, new Set(), new Set());
  assert.ok(body.includes('<!-- hdf5-review-checklist-v1 -->'));
});

// ----------------------------------------------------------------
// Summary
// ----------------------------------------------------------------

console.log('');
console.log(`${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
