'use strict';
// Run with: node .github/scripts/review-checklist.test.js

const assert = require('assert');
const { matchesPattern, labelFromPattern } = require('./review-checklist.js');

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
// Summary
// ----------------------------------------------------------------

console.log('');
console.log(`${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
