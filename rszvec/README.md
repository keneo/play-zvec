# rszvec

A minimal, R-native interface to the [zvec](https://pypi.org/project/zvec/)
vector database. Wraps [`rzvec`](../rzvec/) with five functions — no schemas,
no Python, results as plain `data.frame`s.

## Installation

```r
# install.packages("remotes")
remotes::install_github("keneo/play-zvec/rszvec")
```

### Python setup (one-time)

On Linux, install these system packages first:

```bash
sudo apt install python3-venv python3-dev python3-pip
```

Then:

```r
library(rszvec)
rszvec_install()   # creates a virtualenv and installs the zvec Python package
```

## Quick start

```r
library(rszvec)

col <- rszvec_open("/tmp/my_collection", dim = 4)

rszvec_add(col, "doc_1", c(0.1, 0.2, 0.3, 0.4))
rszvec_add(col, "doc_2", c(0.2, 0.3, 0.4, 0.1))

rszvec_search(col, c(0.4, 0.3, 0.3, 0.1), n = 5)
#      id score
# 1 doc_2  0.30
# 2 doc_1  0.23
```

Bulk insert from a matrix (one row per document):

```r
mat <- matrix(c(0.1, 0.2, 0.3, 0.4,
                0.2, 0.3, 0.4, 0.1), nrow = 2, byrow = TRUE)
rszvec_add_many(col, ids = c("doc_1", "doc_2"), vectors = mat)
```

## API reference

| Function | Description |
|---|---|
| `rszvec_open(path, dim)` | Open an existing collection or create a new one |
| `rszvec_add(col, id, vector)` | Insert a single document |
| `rszvec_add_many(col, ids, vectors)` | Insert multiple documents from a list or matrix |
| `rszvec_search(col, vector, n)` | Nearest-neighbour search; returns a `data.frame` |
| `rszvec_delete(col, id)` | Delete a document by ID |
| `rszvec_install(...)` | One-time Python/virtualenv setup |

## See also

[`rzvec`](../rzvec/) — the full zvec API for R, with control over schemas,
index parameters, upsert, filters, and more.

## License

MIT
