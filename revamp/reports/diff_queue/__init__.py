"""
reports/diff_queue/__init__.py
==============================
Tracks the queue of candidate diffs awaiting human review before PR submission.

Each entry records:
    - Target function and file
    - Gate results (all 6 gates)
    - Complexity delta
    - LLM rationale summary
    - Status: pending | approved | rejected

Owner: Abhinav (Pipeline & CI Lead)
"""
