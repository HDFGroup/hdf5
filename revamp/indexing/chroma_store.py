"""
chroma_store.py — ChromaDB Vector Store Interface
==================================================
Manages the local ChromaDB instance that stores and retrieves
function embeddings (the Style Knowledge Base).

Used by rag.py to retrieve semantically similar functions as
few-shot examples and context for LLM prompts.

Owner: Firdavs (Indexing & RAG Lead)
"""


def upsert(functions: list) -> None:
    """Insert or update function embeddings in the ChromaDB store."""
    raise NotImplementedError("ChromaDB upsert not yet implemented.")


def query(text: str, n_results: int = 5) -> list:
    """
    Retrieve the top-n most similar functions to a query string.

    Args:
        text:      Natural language or code query.
        n_results: Number of similar functions to return.

    Returns:
        List of matching function dicts with similarity scores.
    """
    raise NotImplementedError("ChromaDB query not yet implemented.")
