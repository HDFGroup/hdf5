# HDF5 Patch Release Charter

Status: Draft. Owner: [TBD]. Changes to this charter are made by pull request and reviewed at the next release retro.

## Purpose

This charter defines what an HDF5 patch release is, what it guarantees to users, and how decisions about it are made. Each patch release has a tracking issue created from the "Patch release plan" issue form. That issue is the plan for one release; this charter is the set of rules every plan follows. Where the two conflict, the charter wins, or gets changed by pull request.

## What a patch release is

A patch release (x.y.Z) fixes bugs in an existing release series without asking anything of the people who upgrade. Users can replace any earlier x.y release with x.y.Z without changing code, rebuilding applications, or converting files.

## Guarantees

**API.** No public functions, types, macros, or constants are added, removed, or changed in signature or meaning. This applies to the C library and to the Fortran, C++, Java, and high-level libraries.

**ABI.** The shared library soname does not change. Applications and plugins built against an earlier x.y release run against x.y.Z without recompiling or relinking. This is verified for every release by an automated ABI comparison against the previous release, not by review alone.

**File format.** No new file format features are introduced, and no existing feature starts writing a different encoding. Files written by x.y.Z can be read by every earlier x.y release. The one exception is a fix that stops the library from writing invalid or corrupt files; such a fix is called out in the release notes.

**Behavior and build configuration.** Default property values and library behavior change only to fix a bug. Build options are not removed or renamed. Minimum required versions of compilers, CMake, and dependencies are not raised unless a security fix requires it, in which case the release notes say so. No new deprecations are introduced.

## What is eligible

A change is eligible when it satisfies the guarantees above and does one of the following:

- Fixes a regression relative to an earlier release.
- Fixes data corruption or data loss.
- Fixes a crash, hang, or memory error, including problems found by fuzzing or sanitizers.
- Fixes a security vulnerability.
- Fixes a build or test failure on a Tier 1 platform, including with a newly released version of a Tier 1 compiler.
- Corrects wrong or misleading documentation, including release notes.
- Changes tests, CI, or release tooling without changing installed files.

## What is not eligible

New features or API, file format changes, changes to defaults, refactoring and cleanup, performance improvements that don't fix a performance regression, new deprecations, and dependency upgrades that aren't needed for a security or build fix. These go in the next minor release.

## Gray areas

When it isn't clear whether a change qualifies, the release manager decides and records the reasoning as a comment on the tracking issue. When in doubt, defer to the next minor release: a delayed fix usually costs users less than a broken patch-release guarantee.

## Cadence and triggers

Each supported series gets a scheduled patch release every [TBD: N weeks]. A scheduled release is skipped if no eligible fixes have landed since the last one.

An out-of-band release is started when any of the following arises:

- A high-severity security vulnerability [TBD: define, e.g. CVSS 7.0 or higher, or actively exploited].
- A data corruption bug that affects common use.
- A regression that breaks a major downstream project or common use.
- A build failure on a Tier 1 platform with no reasonable workaround.

Out-of-band releases follow the same plan in compressed form, and the tracking issue records which steps were shortened.

## Supported series

[TBD: which series receive patch releases and for how long, e.g. the current minor series receives scheduled patch releases, and the previous minor series receives security and data corruption fixes for N months after its successor ships.]

## Branches and backports

Fixes land on the development branch first and are backported to the maintenance branch by pull request with the [TBD: label name] label. There are two exceptions: embargoed security fixes, which are prepared privately and land on all affected branches at disclosure, and fixes for code that no longer exists on the development branch.

Every backport pull request includes a release note entry and a test that fails without the fix, or a stated reason why a test isn't practical. The maintenance branch CI is kept green at all times, not just before a release.

## Platform tiers

Tier 1 platforms are tested for every release, and we guarantee the release works on them; any new failure on Tier 1 blocks the release. Tier 2 platforms are tested on a best-effort basis; failures there are documented in the release notes but don't block. The current list is maintained at [TBD: link to supported platforms list].

Known flaky tests are listed by name in each tracking issue. A failure in any test not on that list counts as a real failure.

## Roles

The **release manager** owns the plan and the tracking issue, runs the process, approves scope changes after freeze, and makes the go/no-go decision. Each release also has a **shadow**, who by default becomes release manager for the next release.

**Sign-off owners** for engineering, testing, security, and documentation each confirm that their area meets the definition of done, and each can block the release for problems in their area. A criterion can be waived only with the agreement of the owner of that area, and the waiver and its reason are recorded on the tracking issue.

## Code freeze and exceptions

After code freeze, only fixes for release blockers are merged into the maintenance branch. Adding or removing scope after freeze requires release manager approval, recorded as a comment on the tracking issue with the reason. Every fix merged after freeze produces a new release candidate.

## Definition of done

A release can ship when all of the following are true:

- Tier 1 shows no new failures relative to the previous release.
- The ABI comparison against the previous release is clean.
- No release blockers are open in the milestone.
- Downstream testing results have been reviewed.
- Release notes, including known issues and upgrade guidance, have been reviewed by the documentation owner.
- Security advisories are staged.
- Every sign-off owner has recorded sign-off on the tracking issue.

The final release is the last release candidate rebuilt with only the version string changed, so that what ships is what was tested.

## Hotfixes

If a problem found after a release would have met the criteria for an out-of-band release, a follow-up patch release is started rather than waiting for the next scheduled one. The decision is recorded on the original release's tracking issue.

## Learning from each release

Every patch release ends with a blameless retro within two weeks of the release, using the tracking issue's comments as the timeline and its metrics table as data. Each retro produces at most three action items with owners, and they are carried into the next release's tracking issue. Changes to this charter or to the issue form go through pull requests that reference the retro that prompted them.
