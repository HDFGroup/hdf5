# Revamp Pipeline — Architecture Overview

## Purpose

The Revamp pipeline is an AI-assisted code sustainability tool targeting the HDF5 C library. It uses a multi-layer verification architecture to generate, verify, and deliver complexity-reducing refactoring diffs as pull requests to the upstream HDF5 repository.

**Core design principle:** The worst case is that the tool produces nothing. It cannot make the codebase worse — only improved or untouched.

## Pipeline Layers

```
HDF5 Source (develop branch)
        │
        ▼
┌─────────────────────┐
│  1. INDEXING        │  tree-sitter parse → embed → ChromaDB
│  /indexing/         │  Produces the Style Knowledge Base
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│  2. ANALYSIS        │  Lizard CCN · cppcheck · clang-tidy
│  /analysis/         │  CodeQL · AddressSanitizer
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│  3. LLM ENGINE      │  Claude Sonnet (primary)
│  /llm/              │  GPT-4o (verifier / fallback)
│                     │  RAG context from ChromaDB
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│  4. VERIFICATION    │  6 binary gates — all must pass
│  /gates/            │  ① Build  ② Static  ③ Dual-LLM
│                     │  ④ Memory ⑤ Test    ⑥ Scorer
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│  5. REPORTING       │  Complexity timeline dashboard
│  /reports/          │  Diff queue · Evaluation metrics
└────────┬────────────┘
         │
         ▼
    PR to upstream
    HDFGroup/hdf5
```

## Gate Philosophy

All 6 gates are binary pass/fail. A diff must pass every gate or it is discarded silently — never delivered in a broken state. There is no partial approval.

| Gate | Tool | Owner |
|------|------|-------|
| Build | CMake + Ninja | Abhinav |
| Static | cppcheck + clang-tidy | Sydney |
| Dual-LLM | Claude Sonnet + GPT-4o | Basil |
| Memory | AddressSanitizer | Sydney |
| Test | CTest (serial) | Abhinav |
| Scorer | Lizard CCN delta | Abhinav |

## Scope Constraints

- **C library only.** The C++ interface is deprecated and excluded.
- **No new files.** Diffs are subtractive or neutral — never additive.
- **No new public API.** Hard-coded into the LLM system prompt.
- **Max 10 files per diff.** Configurable in config.yaml.
- **Zero new symbol exports.** Enforced by the dual-LLM gate.

## Delivery Workflow

Verified diffs are delivered as **pull requests** to `HDFGroup/hdf5`. Major changes require the HDF5 RFC process. Rejections are handled via PR comments. The sandbox repo (`sp26-hdfgroup/hdf5-sandbox`) is used for all development; HDF engineers monitor via read access.
