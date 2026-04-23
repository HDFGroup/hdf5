"""
reports/evaluation/__init__.py
================================
Stores evaluation results tracking pipeline accuracy over time.

Tracks the open research question from the HDF Group meeting:
    "What is the relationship between code complexity and
     AI understanding accuracy?"

Metrics recorded per refactoring attempt:
    - CCN of target function
    - Gate pass/fail outcomes
    - LLM approval rates vs. human approval rates
    - Final PR acceptance by HDF engineers

Owner: Abhinav (Pipeline & CI Lead)
"""
