# HDF5 Java FFM Tests

FFM (Foreign Function & Memory) API tests for HDF5 Java bindings.

## Test Files

Tests use direct FFM bindings to HDF5 C APIs:
- `TestH5*ffm.java` - FFM tests for each H5 module
- `FfmTestSupport.java` - Shared FFM utilities and patterns

## Current Coverage (435 tests - 16/18 modules)

| Module | Tests | APIs | Coverage | Status |
|--------|-------|------|----------|--------|
| H5T | 92 | 176 | 52% | Complete |
| H5P | 80 | 266 | 30% | Active |
| H5S | 41 | 43 | 95% | Complete |
| H5A | 29 | 55 | 53% | Complete |
| H5D | 28 | 56 | 50% | Complete |
| H5F | 20 | 57 | 35% | Active |
| H5O | 19 | 55 | 35% | Active |
| H5Z | 17 | 2 | 100% | ✅ **NEW** |
| H5L | 16 | 38 | 42% | Complete |
| H5G | 15 | 37 | 41% | Complete |
| H5I | 15 | 21 | 71% | Complete |
| H5R | 15 | 27 | 56% | Complete |
| H5E | 14 | 32 | 44% | Complete |
| H5VL | 12 | 12 | 100% | ✅ **NEW** |
| H5PL | 11 | 9 | 100% | ✅ **NEW** |
| H5FD | 11 | 3 | 100% | ✅ **NEW** |

## Running Tests

```bash
# All FFM tests
cd build && ctest -R "JUnitFFM" -V

# Specific module
cd build
export LD_LIBRARY_PATH="$PWD/bin:$LD_LIBRARY_PATH"
java --enable-native-access=ALL-UNNAMED \
  -cp ".:java/jsrc/javahdf5-2.0.0-SNAPSHOT.jar:..." \
  -ea org.junit.runner.JUnitCore jtest.TestH5Affm
```

## FFM Best Practices

See `.claude/FFM_MEMORY_PATTERNS.md` for comprehensive guide.

**Key patterns:**
- Use `FfmTestSupport` helper functions for memory allocation
- Use `try-with-resources` for Arena lifecycle management
- Initialize memory before passing to C APIs
- Clean up HDF5 IDs with `closeQuietly()`

## Module Implementation Status

### Implemented (16 modules - 89%)
- **Core Modules:** H5A, H5D, H5E, H5F, H5G, H5I, H5L, H5O, H5P, H5R, H5S, H5T
- **Infrastructure Modules:** H5VL, H5PL, H5Z, H5FD ✅ **NEW (Oct 15, 2025)**

### Deferred (2 modules - 11%)
- **H5ES** (Event Sets) - Not required for merge (per user decision Oct 15, 2025)
- **H5M** (Maps) - Requires `HDF5_ENABLE_MAP_API=ON`, experimental API

## Known Limitations

### H5M (Maps) - NOT TESTED
**Reason:** Requires `HDF5_ENABLE_MAP_API=ON` (OFF by default)
- Experimental API, subject to change
- Requires VOL connector with Map support (not native format)
- FFM bindings generated with MAP_API=OFF
- **Status:** Deferred to future implementation

### H5VL (VOL) - NATIVE ONLY
**Reason:** Testing limited to native VOL connector
- Custom VOL connectors require external plugins
- Tests verify VOL registration/query APIs with native connector
- Full VOL connector testing requires specialized setup

### H5ES (Event Sets) - NOT REQUIRED FOR MERGE
**Reason:** User decision (October 15, 2025)
- Async API testing deferred to follow-on task
- Not blocking merge to develop branch

## Test Development

**Creating new tests:**
1. Copy pattern from existing `TestH5*ffm.java`
2. Follow FFM memory allocation patterns
3. Use `@BeforeClass`, `@Before`, `@After`, `@AfterClass` for setup/teardown
4. Use `FfmTestSupport` utilities consistently
5. Document any special FFM patterns needed

**Common patterns:**
```java
// Arena management
try (Arena arena = Arena.ofConfined()) {
    // Allocate and use memory
    MemorySegment value = allocateInt(arena);
    setInt(value, 42);
    // Use value...
}

// HDF5 ID cleanup
long id = -1;
try {
    id = H5Fcreate(...);
    // Use id...
} finally {
    closeQuietly(id);
}
```

## Future Work

**Test expansion priorities (Phase 2):**
1. ✅ H5VL, H5PL, H5Z, H5FD implementation (51 tests) - **COMPLETE**
2. H5P expansion (80→150 tests, +70)
3. H5T expansion (92→120 tests, +28)
4. H5F, H5O expansion (20→45, 19→45 tests, +51)

**Current Target:** 584 tests covering 16/18 public modules (89%)

**Post-Merge (optional):**
- H5ES implementation (~18 tests) - Follow-on task
- H5M implementation (~20 tests) - If MAP_API enabled
