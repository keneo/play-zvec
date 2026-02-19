# rzvec

R bindings for the [zvec](https://pypi.org/project/zvec/) vector database, via
[reticulate](https://rstudio.github.io/reticulate/).

## Installation

### 1. Install the R package from GitHub

```r
# install.packages("remotes")
remotes::install_github("bartek-szabat/rzvec")
```

### 2. Install the Python `zvec` package

`rzvec` ships a helper that creates a dedicated Python virtualenv and installs
`zvec` into it:

```r
library(rzvec)
rzvec_install()   # one-time setup; creates ~/Library/Caches/R/rzvec/rzvec-venv
```

Alternatively, point reticulate at an existing virtualenv or conda env that
already has `zvec` installed, then load `rzvec` normally.

## Quick start

```r
library(reticulate)
library(rzvec)

# Define a schema with a single 4-dimensional float vector field
schema <- collection_schema(
  "my_docs",
  vectors = vector_schema("embedding", zvec_data_type()$VECTOR_FP32, 4L)
)

# Create a collection on disk
col <- create_collection("./my_collection", schema)

# Insert documents
col_insert(col,
  zvec_doc("doc_1", vectors = list(embedding = c(0.1, 0.2, 0.3, 0.4))),
  zvec_doc("doc_2", vectors = list(embedding = c(0.2, 0.3, 0.4, 0.1)))
)

# Nearest-neighbour search
results <- col_query(col,
  vector_query("embedding", vector = c(0.4, 0.3, 0.3, 0.1)),
  topk = 5L
)
```

## API reference

| Category | Functions |
|---|---|
| Schema | `collection_schema()`, `vector_schema()`, `field_schema()`, `zvec_data_type()` |
| Lifecycle | `create_collection()`, `open_collection()` |
| CRUD | `col_insert()`, `col_upsert()`, `col_update()`, `col_delete()`, `col_delete_by_filter()`, `col_fetch()` |
| Search | `col_query()` |
| Index | `hnsw_index_param()`, `ivf_index_param()`, `flat_index_param()`, `invert_index_param()`, `col_create_index()`, `col_drop_index()` |
| Maintenance | `col_flush()`, `col_optimize()`, `col_schema()`, `col_stats()` |
| Setup | `rzvec_install()` |

## License

MIT
