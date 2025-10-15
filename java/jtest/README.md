# HDF5 Java FFM Tests

FFM (Foreign Function & Memory) API tests for HDF5 Java bindings.

## Test Files

Tests use direct FFM bindings to HDF5 C APIs:
- `TestH5*ffm.java` - FFM tests for each H5 module
- `FfmTestSupport.java` - Shared FFM utilities and patterns

## Current Coverage (384 tests)

| Module | Tests | APIs | Coverage | Status |
|--------|-------|------|----------|--------|
| H5T | 92 | 176 | 52% | Complete |
| H5P | 76 | 266 | 29% | Active |
| H5S | 41 | 43 | 95% | Complete |
| H5A | 27 | 55 | 49% | Complete |
| H5D | 27 | 56 | 48% | Complete |
| H5F | 20 | 57 | 35% | Active |
| H5O | 19 | 55 | 35% | Active |
| H5L | 16 | 38 | 42% | Complete |
| H5G | 15 | 37 | 41% | Complete |
| H5I | 15 | 21 | 71% | Complete |
| H5R | 13 | 27 | 48% | Complete |
| H5E | 14 | 32 | 44% | Complete |

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

### Implemented (12 modules)
- H5A, H5D, H5E, H5F, H5G, H5I, H5L, H5O, H5P, H5R, H5S, H5T

### Planned (4 modules)
- **H5VL** (VOL connectors) - Native VOL testing only
- **H5PL** (Plugin management)
- **H5Z** (Filter registration)
- **H5FD** (VFD registration)

### Deferred (2 modules)
- **H5ES** (Event Sets) - Deferred until other modules complete
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

### H5ES (Event Sets) - DEFERRED
**Reason:** Deferred until adequate coverage of sync APIs
- Required for async operation testing
- Will be implemented before merge to develop

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

## Documentation

- **FFM patterns:** `.claude/FFM_MEMORY_PATTERNS.md`
- **Test coverage:** `.claude/COMPLETE_API_COVERAGE_ANALYSIS_2025-10-15.md`
- **Development plan:** `.claude/DEVELOPMENT_SUMMARY_2025-10-15.md`
- **Main guide:** `CLAUDE.md` (repository root)

## Future Work

**Test expansion priorities:**
1. H5VL, H5PL, H5Z, H5FD implementation (~22 tests)
2. H5P expansion (76→150 tests)
3. H5T expansion (92→120 tests)
4. H5F, H5O expansion (20→45, 19→45 tests)
5. H5ES implementation before merge (~18 tests)
6. H5M implementation if MAP_API enabled (deferred)

**Target:** 577 tests covering 17/18 public modules before merge
