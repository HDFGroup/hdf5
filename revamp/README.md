# Revamp — HDF5 AI Sustainability Pipeline

> AI-assisted code sustainability tooling for the HDF5 C library.  
> Built by Revamp Engineering (ACFI) in collaboration with [HDF Group](https://www.hdfgroup.org/).

---

## What This Does

Revamp is a verification-gated pipeline that uses LLMs to generate, verify, and deliver complexity-reducing refactoring diffs for the HDF5 C library. Every candidate diff passes 6 binary gates before it can be submitted as a PR to the upstream repo.

**Design guarantee:** The worst case is that the tool produces nothing. It cannot make the codebase worse — only improved or untouched.

## Quick Start

```bash
git clone https://github.com/<your-org>/revamp.git
cd revamp

# Install Python dependencies
pip install -r requirements.txt

# Copy and edit config
cp config.yaml config.local.yaml
# Set your API keys and sandbox repo path in config.local.yaml

# Run the pipeline
python orchestrator.py
```

---

## Architecture

The pipeline has 5 layers, each in its own directory:

```
revamp/
├── orchestrator.py          # Entry point — coordinates all layers
├── config.yaml              # Central configuration
├── verify_e2e.py            # End-to-end gate runner
│
├── indexing/                # Layer 1: Parse + embed HDF5 C source
│   ├── tree_sitter_parser.py
│   ├── embedder.py
│   └── chroma_store.py      # Style Knowledge Base (ChromaDB)
│
├── analysis/                # Layer 2: Static + security + memory analysis
│   ├── complexity.py        # Lizard CCN
│   ├── static_analysis.py   # cppcheck + clang-tidy
│   ├── security.py          # CodeQL
│   └── memory_safety.py     # AddressSanitizer
│
├── llm/                     # Layer 3: LLM refactoring + verification
│   ├── primary.py           # Claude Sonnet — generates diffs
│   ├── verifier.py          # GPT-4o — independent review
│   ├── rag.py               # RAG context retrieval
│   └── prompts/             # System prompt templates
│
├── gates/                   # Layer 4: Binary verification gates
│   ├── apply.py             # Diff application + rollback
│   ├── build_gate.py        # Gate 1: CMake build
│   ├── static_gate.py       # Gate 2: Static analysis
│   ├── dual_llm_gate.py     # Gate 3: Dual-LLM approval
│   ├── memory_gate.py       # Gate 4: Memory safety (ASan)
│   ├── test_gate.py         # Gate 5: CTest regression
│   └── scorer.py            # Gate 6: CCN improvement check
│
├── ci/                      # Layer 5: CI/CD configurations
│   ├── github_actions/      # Workflow YAMLs
│   └── docker/              # Container definitions
│
├── reports/                 # Generated outputs
│   ├── complexity_timeline/ # Interactive HTML dashboard (Week 1 deliverable)
│   ├── diff_queue/          # Diffs awaiting human review
│   └── evaluation/          # Pipeline accuracy metrics
│
└── docs/
    └── architecture.md      # Full architecture deep-dive
```

---

## Verification Gates

All 6 gates must pass. Any failure silently discards the diff.

| # | Gate | Tool | Blocks on |
|---|------|------|-----------|
| 1 | Build | CMake + Ninja | Compilation failure |
| 2 | Static | cppcheck + clang-tidy | New warnings |
| 3 | Dual-LLM | Claude Sonnet + GPT-4o | Either LLM rejects |
| 4 | Memory | AddressSanitizer | New memory errors |
| 5 | Test | CTest (serial) | Test regressions |
| 6 | Scorer | Lizard CCN delta | No complexity improvement |

---

## Team & Role Ownership

| Name | Role | Owns |
|------|------|------|
| **Abhinav** | Pipeline & CI Lead / Scrum Lead | `orchestrator.py`, `verify_e2e.py`, `/gates`, `/ci`, `/reports` |
| **Diti** | Project Manager / PRD Owner | Client comms, PRD, sprint planning |
| **Firdavs** | Indexing & RAG Lead | `/indexing` |
| **Sydney** | Static Analysis & Safety Lead | `/analysis` |
| **Basil** | LLM Engine Lead | `/llm` |
| **William** | Pipeline Support | `/gates`, `/ci` (under Abhinav) |

**HDF Group contacts:**
- Scot Breitenfeld — Director of Engineering (monitoring, PR reviewer)
- Gerd Heber — Executive Director (stakeholder)

---

## Scope Constraints

| Allowed | Not Allowed |
|---------|-------------|
| Refactoring existing C functions | New public API or new exports |
| Test de-duplication | New files |
| Complexity reduction | C++ / Fortran / Java changes |
| Static analysis improvements | Subsystem rewrites |
| Delivering diffs as PRs | Direct commits to upstream |

---

## Sandbox Repository

All development happens against the HDF5 sandbox clone:  
`https://github.com/sp26-hdfgroup/hdf5-sandbox`

Changes are delivered to the main HDF5 repo via PR:  
`https://github.com/HDFGroup/hdf5`

---

## Contact

- Abhinav (Scrum Lead): ag135@illinois.edu
- Diti (PM): djc11@illinois.edu
