# HDF5 — Revamp AI Sustainability Sandbox

> **This is a development sandbox clone of the [HDF5 library](https://github.com/HDFGroup/hdf5), maintained by Revamp Engineering (ACFI) in collaboration with HDF Group.**
>
> No changes in this repository are merged to the main HDF5 repo without going through the standard PR review process on the upstream repository.

---

## What Is This?

This repository is the working environment for the **HDF5 AI Sustainability Initiative** — a 12-week engagement between HDF Group and Revamp Engineering focused on:

- **Complexity reduction** in selected HDF5 C modules via verification-gated AI-assisted refactoring
- **Test suite optimization** through redundancy elimination (reducing mini-test runtime with zero coverage regression)
- **Complexity tracking** across HDF5 release history using Lizard and related metrics

All work is scoped to the **C library only** (the C++ interface is deprecated and excluded). The tool's design constraint is subtractive: the worst-case output is nothing. It cannot make the codebase worse — only improved or untouched.

## Scope Constraints

| Allowed | Not Allowed |
|---------|-------------|
| Targeted refactoring (max 5–10 files per diff) | Subsystem rewrites or architectural changes |
| Logic consolidation / removing duplicated code | New features or new public API surface |
| Test de-duplication | Auto-commits or direct writes to develop |
| Static analysis gating | Custom model training |
| Risk scoring with AI-generated rationale | GPU-heavy infrastructure |

## Repository Structure

This clone tracks the `develop` branch of `HDFGroup/hdf5`. Revamp-specific additions:

```
.github/workflows/ci-basic.yml   ← Sandbox CI (build + test + Lizard snapshot)
revamp/                           ← Revamp pipeline code (added in Phase 2+)
  indexing/                       ← tree-sitter parser, embedder, ChromaDB store
  analysis/                       ← complexity, static analysis, security, memory safety
  llm/                            ← primary LLM, verifier LLM, RAG, prompts
  gates/                          ← verification pipeline gates
  ci/                             ← additional CI workflow configs
  reports/                        ← complexity timeline, diff queue, evaluation
```

## CI Pipeline

The sandbox runs a basic GitHub Actions workflow on every push to `develop` and on PRs:

1. **Configure** — CMake with Ninja, C library only (no C++/Fortran/Java)
2. **Build** — Release build on Ubuntu latest
3. **Test** — Serial test suite via CTest (parallel/MPI tests excluded on CI runner)
4. **Lizard snapshot** — Informational complexity scan on `src/` (push to develop only)

This will be extended in Phase 2 with the full 6-gate verification pipeline.

## Access & Roles

| Person | Role | Access |
|--------|------|--------|
| Scot Breitenfeld | HDF Group — Director of Engineering | Read (monitoring) |
| Gerd Heber | HDF Group — Executive Director | Read (monitoring) |
| Abhinav Gupta | Revamp — Pipeline & CI Lead / Scrum Lead | Write |
| Diti Chhaproo | Revamp — Project Manager | Write |
| Sydney | Revamp — Static Analysis & Safety Lead | Write |
| Firdavs | Revamp — Indexing & RAG Lead | Write |
| Basil | Revamp — LLM Engine Lead | Write |
| William | Revamp — Pipeline Support | Write |

## Syncing With Upstream

This clone should be periodically synced with `HDFGroup/hdf5:develop` to stay current:

```bash
git remote add upstream https://github.com/HDFGroup/hdf5.git
git fetch upstream
git merge upstream/develop
```

## Delivering Changes to HDF Group

Successful changes are delivered as **pull requests** to the main `HDFGroup/hdf5` repository (not patches). Rejections are handled via PR comments. Major changes require the HDF5 RFC process.

## Contact

- **Revamp**: ag135@illinois.edu (Abhinav), djc11@illinois.edu (Diti)
- **HDF Group**: Scot Breitenfeld, Gerd Heber

## License

This repository inherits the [BSD license](COPYING) from the upstream HDF5 project.
