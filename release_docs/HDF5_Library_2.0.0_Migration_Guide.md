# HDF5 2.0.0 Library Migration Guide

## Myth busters up front

- [Upgrading HDF5 upgrades my files in-place and breaks my archive](#myth-upgrading-hdf5-upgrades-my-files-in-place-and-breaks-my-archive)
- [2.0.0 means I must rewrite my entire application](#myth-200-means-i-must-rewrite-my-entire-application)
- [All files written by 2.0.0 are unreadable by 1.x](#myth-all-files-written-by-200-are-unreadable-by-1x)
- [If something breaks, it must be the file format](#myth-if-something-breaks-it-must-be-the-file-format)

## Introduction

The [HDF5 library version 2.0.0](https://www.hdfgroup.org/2025/11/10/release-of-hdf5-2-0-0-newsletter-207/) was released on **November 10, 2025**.
This guide is written for “ordinary” HDF5 users: people who have real applications and workflows, want the upgrade to be boring, and mainly need clarity on **risk, compatibility, and a practical upgrade plan**.

With few exceptions, the recommendations in this guide apply to the 1.x family of releases. However, if you are contemplating an HDF5 library upgrade, our recommendation is to move directly to 2.0.0 to take advantage of the latest features and improvements.

## What HDF5 2.0.0 means (and what it doesn’t)

### “2.0.0” does not mean “your existing HDF5 files are obsolete”

HDF5’s compatibility story is mostly about **file format compatibility**, and HDF5 provides a forward/backward compatibility guarantee in plain terms: the latest library can read files created by earlier versions, and older libraries can read newer files *as long as the file doesn’t use features introduced later*.

### “2.0.0” does mean “expect *some* API/ABI churn”

HDF5 2.0.0 introduces new APIs and behavioral changes. While standard user APIs are preserved via compatibility macros, a small set of advanced developer APIs (specifically in the VOL layer) were removed entirely.
That said, most users can avoid code edits initially by using the **API Compatibility Macros**, which were created specifically to reduce migration pain.

## The three kinds of compatibility you should care about

### 1) File compatibility (the one most users worry about)

**Default file creation behavior changed in 2.0.0.** By default, new files will use the **1.8-era format** (rather than the oldest/earliest format), for better performance and space usage.
The practical headline is:

- **Files created with 2.0.0 defaults are still readable by HDF5 as old as 1.8.0**.
- If you must support readers older than 1.8, you can explicitly set version bounds with `H5Pset_libver_bounds()`.

Also note: **some new datatypes/features are inherently not readable by older libraries**. For example, **the new complex number datatype class** is not readable by previous HDF5 versions.

### 2) API compatibility (whether your source code compiles)

This is where the **API compatibility macros** matter. They allow old source to compile against newer libraries by mapping calls like `H5Lvisit` to older/newer “versioned” implementations.

In 2.0.0, the default API mapping is the **2.0.x (“v200”)** set.
But you can compile your application using an older API mapping (e.g., 1.14.x or 1.10.x) using `-DH5_USE_114_API`, `-DH5_USE_110_API`, etc.

### 3) ABI/runtime compatibility (whether your binary runs with the library you think it runs with)

Even if your code compiles, many “it worked yesterday” failures come from:

- linking against one HDF5 at build time but loading a different one at runtime
- mixing HDF5 from system packages + Conda + Spack + vendor MPI stacks
- plugins/filters built for a different HDF5 than the one you’re loading

This guide includes checks to catch that early.

## A simple decision tree

### Step 1: Do you *produce* HDF5 files for other people/tools?

- **No** → You mostly care about “can I read my existing files with 2.0.0?” (Usually yes.)
- **Yes** → Identify the **oldest HDF5 library version** that must be able to read the files you produce.

  - If the oldest reader is **≥ 1.8.0**, 2.0.0 defaults are likely fine.
  - If the oldest reader is **< 1.8.0**, plan to set `H5Pset_libver_bounds()` explicitly.

### Step 2: Are you a “plain HDF5 API user,” or do you touch advanced internals?

Most users are “plain API” users and won’t hit removals. But if you use VOL/VFD internals or niche calls, skim the removed API list; some functions were removed in 2.0.0.

### Step 3: Do you build HDF5 from source?

If yes, be aware HDF5 2.0.0 transitioned to **CMake-only builds**.
(Autotools is no longer used in HDF5 itself.)
Your *application* can still use whatever build system you want.

## Recommended upgrade strategies

### Strategy A: “Zero/low code-change” migration (recommended starting point)

**Goal:** upgrade the library first, keep your application stable.

1. Install/build HDF5 2.0.0
2. Rebuild your application **with an application API mapping** that matches what it was originally written for:

   - `-DH5_USE_114_API` (if your codebase assumes 1.14-style APIs)
   - `-DH5_USE_110_API` (if it’s older 1.10-era)
   - etc.
3. Run tests, validate read/write behavior.
4. Only then consider modernizing specific calls.

### Strategy B: Targeted modernization (small edits, big payoff)

If you hit compile errors, they’re often concentrated in a small set:

- signature changes like `H5Dread_chunk()`, `H5Tdecode()`, `H5Iregister_type()`
- removed APIs (usually advanced)

Fix those, keep everything else on compatibility macros.

### Strategy C: “New API only” hardening (later, not day one)

Eventually, you may want to build without deprecated symbols / deprecated mappings for cleanliness, but it’s not the fastest path to an upgrade. (The macro docs lay out an incremental plan for doing this safely.)

## Pre-upgrade checklist

Use this when you’re planning the change (before installing anything).

### Compatibility requirements

- [ ] Identify the **oldest reader** that must open files you create (your own tools, collaborators, archived pipelines, vendor software, etc.).
- [ ] If you need **< 1.8 compatibility**, plan to set `H5Pset_libver_bounds()` for file creation.
- [ ] Decide whether you will allow **new 2.0.0-only features** (e.g., complex datatypes). If you need older readers, avoid those features or use established conventions.

### Application inventory

- [ ] Record how you currently build/link:

  - compiler + flags
  - shared vs static HDF5
  - MPI/parallel HDF5 usage
  - compression filters/plugins
- [ ] Identify whether you call any of these (known 2.0.0 signature changes):

  - `H5Dread_chunk()`, `H5Tdecode()`, `H5Iregister_type()`
- [ ] Identify whether you call any of the removed APIs (advanced users):

  - `H5FDperform_init()`, `H5VLpeek_connector_id_by_name()`, `H5VLpeek_connector_id_by_value()`, `H5VLfinish_lib_state()`, `H5VLstart_lib_state()`, etc.

### Runtime hygiene (prevents 80% of “mystery” failures)

- [ ] Ensure you can run a command that prints the HDF5 version at runtime (e.g., `H5check_version()` in a tiny test binary).
- [ ] Plan to install 2.0.0 **side-by-side** with the old library first (don’t overwrite system HDF5 unless you control the whole environment).

## During-upgrade checklist

### Install/build HDF5 2.0.0

- [ ] If building HDF5 from source: note HDF5 moved to **CMake-only builds**.
- [ ] If using ROS3 / S3 workflows: be aware the ROS3 VFD backend changed and now depends on `aws-c-s3` when built with ROS3.

### Rebuild your application (start with compatibility macros)

- [ ] Compile with the appropriate `H5_USE_*_API` application mapping:

  - default behavior in 2.0.0 is effectively `H5_USE_200_API` (v200).
  - use `H5_USE_114_API`, `H5_USE_110_API`, etc. to “pin” to the API level your code expects.
- [ ] If you’re unsure what the installed library’s default API mapping is, check `libhdf5.settings` (it records things like “Default API mapping: v200” and whether deprecated symbols are enabled).
- [ ] If you’re on Windows and using shared libraries, ensure you set `H5_BUILT_AS_DYNAMIC_LIB` in your application build.

### Smoke tests (quick confidence)

- [ ] Open and read representative existing files (small + large + “weird” ones).
- [ ] Write a new file, reopen it, run `h5dump`/`h5diff`-style validations (or your own validators).
- [ ] If you distribute files to older readers: test opening the newly created file with that older version (or in a container).

## Post-upgrade checklist

### Functional correctness

- [ ] Run your full application test suite.
- [ ] Validate a handful of “golden” HDF5 outputs with structural diff tooling.

### Compatibility confirmation (only if needed)

- [ ] If you must support older readers, confirm you set version bounds intentionally (don’t rely on tribal knowledge).
- [ ] Add an automated test that creates a file and verifies your chosen compatibility policy.

### Performance sanity

HDF5 2.0.0 includes major VDS improvements (up to ~2500% faster VDS read/write in the release messaging).

- [ ] If you use Virtual Datasets heavily, benchmark before/after.
- [ ] If you don’t use VDS, still spot-check any I/O hotspots.

## Representative migration examples

### Example 1: “No code changes” rebuild using an application mapping

**Command-line build (conceptual):**

```bash
h5cc -DH5_USE_114_API my_app.c -o my_app
```

The macro documentation explicitly lists `-DH5_USE_114_API`, `-DH5_USE_110_API`, etc. as application mapping options.
The HDF5 download page also gives an example of building a 1.10-era application against 2.0.0 using `-DH5_USE_110_API`.

**CMake example:**

```cmake
find_package(HDF5 REQUIRED COMPONENTS C)
add_executable(my_app main.c)
target_link_libraries(my_app PRIVATE HDF5::HDF5)

# If your codebase expects 1.14 APIs:
target_compile_definitions(my_app PRIVATE H5_USE_114_API)
```

**Why this works:** the compatibility macros choose the appropriate “versioned” implementation based on (1) how the library was built and (2) what you defined when compiling your app.

### Example 2: Fixing a signature change cleanly (H5Dread_chunk)

In 2.0.0, `H5Dread_chunk()` has a signature change (and maps to the new signature by default unless you configure older mappings).

What to do in practice:

- If you want minimal changes now: build with the older API mapping (Strategy A).
- If you want to modernize this call: update to the newer form and handle the buffer sizing safely.

The release-specific notes describe that `H5Dread_chunk()` gained a new parameter, and the old form was renamed (deprecated) while the new one is the recommended target.

**Modernized usage pattern (illustrative):**

```c
size_t buf_size = my_buf_capacity;
herr_t status = H5Dread_chunk2(dset_id, dxpl_id, offset, &filters,
                               buf, &buf_size);

if (status < 0) {
    /* handle error */
}

/* If buf_size got updated, you can reallocate and retry, etc. */
```

(Exact error-handling policy is application-specific, but the *idea* is: treat the extra parameter as a safe “tell me how big this needs to be” mechanism.)

### Example 3: Creating files that older HDF5 can read

HDF5 2.0.0 changes default file creation behavior to the 1.8-era format.
If you need broader backward compatibility, explicitly set version bounds:

```c
hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);

/* Make an intentional compatibility choice for file creation */
H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST);

hid_t file = H5Fcreate("out.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
```

The forum announcement about the defaults explicitly calls out using `H5Pset_libver_bounds()` to ensure backward compatibility when needed.

**Important caveat:** if you adopt **2.0.0-only features** (like the new complex datatype class), older HDF5 versions will not be able to read them.
So: “set bounds” + “avoid incompatible features” go together if you truly need older-reader support.

### Example 4: “It doesn’t compile”: removed APIs

If your build breaks with missing symbols, check whether you use APIs that were removed in 2.0.0. The release-specific “software changes” page lists removed public APIs, including several VOL-related functions and `H5FDperform_init()`.

Practical approach:

1. Identify the missing symbol(s).
2. Confirm they’re in the removed list.
3. Search for the recommended replacements in the 2.0.0 reference/manual and release notes (some removals reflect internal refactors; you may need to change approach rather than rename a call).

## Myths and confusion to clear up

### Myth: “Upgrading HDF5 upgrades my files in-place and breaks my archive”

Reality: opening old files with a new library is a normal use case; the HDF5 library’s compatibility guarantee is designed for this.
The main way you “break” compatibility is by **writing new files with newer features** and expecting older readers to understand them.

### Myth: “2.0.0 means I must rewrite my entire application”

Reality: you usually don’t. The API compatibility macros exist specifically to avoid large-scale edits and provide a bridge from old APIs to new ones.

### Myth: “All files written by 2.0.0 are unreadable by 1.x”

Reality: by default, 2.0.0 writes files using the 1.8-era format and those files are expected to be readable by HDF5 1.8.0 and newer.
If you need older compatibility, explicitly set bounds.

### Myth: “If something breaks, it must be the file format”

Reality: more often it’s...

- a build-time vs runtime library mismatch
- a plugin/filter mismatch
- an API signature change in a specific function

## Troubleshooting quick hits

### “My old app compiles, but crashes / behaves oddly”

- Confirm at runtime you’re loading the HDF5 you think you are (avoid mixed installs).
- Check whether you use plugins/filters and whether they match the loaded HDF5.

### “My files created on system A won’t open on system B”

- Check the reader’s HDF5 version. Default-created 2.0.0 files are intended to be readable by 1.8.0+.
- If system B is older than 1.8.0, set `H5Pset_libver_bounds()` in the writer.

### “I use ROS3 and now my build changed”

- 2.0.0 switched the ROS3 S3 backend and it now requires `aws-c-s3` to build that feature.

### “Tools output changed”

- `h5dump` added `--lformat` and its XML output mode is deprecated.

## A “good default” migration plan

If you want an opinionated plan that reduces fear:

1. Install HDF5 2.0.0 side-by-side.
2. Rebuild your application with `-DH5_USE_114_API` (or your historical API level).
3. Run your test suite + open/write a representative set of files.
4. If you distribute files, decide and document your “oldest supported reader.”
5. Add `H5Pset_libver_bounds()` explicitly if that policy requires it.
6. Modernize only the few changed APIs you actually hit (e.g., the signature-change trio).
7. Later: migrate toward the latest API mapping and (optionally) reduce deprecated usage following the incremental plan outlined in the compatibility macro docs.

## Acknowledgments

This material is based upon work supported by the National Science Foundation under Federal Award No. 2534078. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.
