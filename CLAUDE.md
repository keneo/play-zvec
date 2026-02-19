# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

R bindings for `zvec` (a vector database library). R calls into the Python `zvec` package via `reticulate`. The project uses `renv` for R dependency management and a dedicated conda environment for Python.

## Environment Setup

The script uses an arm64 Python virtualenv (`rzvec-venv/`) located in the project root. The x86_64 Anaconda environment (`rzvec`) is **incompatible** on Apple Silicon — use the virtualenv instead.

One-time setup (from the project root):

```bash
/opt/homebrew/bin/python3.12 -m venv rzvec-venv
rzvec-venv/bin/pip install zvec
```

Restore R dependencies:
```bash
Rscript -e "renv::restore()"
```

## Running Scripts

```bash
Rscript failing_script.R
```

Or open `play-zvec.Rproj` in RStudio and source scripts interactively.

## Architecture

- **`failing_script.R`** — Example script demonstrating the full workflow: define a schema, create/open a collection, insert docs with vector embeddings, and run a vector query.
- **`reticulate` bridge** — All `zvec` objects (`CollectionSchema`, `VectorSchema`, `DataType`, `Doc`, `VectorQuery`) are Python objects accessed through `reticulate::import("zvec")`. Integer arguments must be explicitly cast with `as.integer()`.
- **`renv`** — Lockfile (`renv.lock`) pins R package versions; `.Rprofile` auto-activates renv on session start.

## Key Patterns

- Use `as.integer()` for any dimension/count arguments passed to Python (e.g., `as.integer(4)` for vector dimensions, `as.integer(10)` for `topk`).
- Use `reticulate::dict()` when passing named Python dicts (e.g., `vectors=dict(embedding=c(...))`).
- The virtualenv is activated via `use_virtualenv(file.path(getwd(), "rzvec-venv"), required = TRUE)` at the top of each script. Scripts must be run from the project root so `getwd()` resolves correctly.
