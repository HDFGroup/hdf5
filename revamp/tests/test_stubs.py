"""
test_stubs.py — Stub verification tests
=========================================
Confirms all stub modules import correctly and raise NotImplementedError
as expected. These tests ensure the scaffold is always importable even
before implementation begins.
"""

import pytest


def test_orchestrator_imports():
    from revamp import orchestrator
    assert hasattr(orchestrator, "run_pipeline")


def test_indexing_imports():
    from revamp.indexing import tree_sitter_parser, embedder, chroma_store
    with pytest.raises(NotImplementedError):
        tree_sitter_parser.parse_file(None)


def test_analysis_imports():
    from revamp.analysis import complexity, static_analysis, security, memory_safety
    with pytest.raises(NotImplementedError):
        complexity.run_lizard(None)


def test_llm_imports():
    from revamp.llm import primary, verifier, rag
    with pytest.raises(NotImplementedError):
        primary.generate_diff(None, None, None)


def test_gates_import():
    from revamp.gates import build_gate, static_gate, dual_llm_gate
    from revamp.gates import memory_gate, test_gate, scorer
    with pytest.raises(NotImplementedError):
        build_gate.run(None, None)
