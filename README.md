# play-zvec

R bindings for the [zvec](https://pypi.org/project/zvec/) vector database at
three levels of abstraction.

| Package | Who it's for | What you get |
|---|---|---|
| [`rzvec`](rzvec/) | R developers who want the full zvec API | Thin, idiomatic R wrappers over every zvec Python class and method |
| [`rszvec`](rszvec/) | R developers who want minimal ceremony | 5-function API; results come back as plain `data.frame`s |

Raw Python access via `reticulate` is also demonstrated in
[`play-zvec-example.R`](play-zvec-example.R).

## Quick start

### rszvec — simplest path

```r
# install.packages("remotes")
remotes::install_github("keneo/play-zvec/rszvec")

library(rszvec)
rszvec_install()   # one-time: creates a Python virtualenv and installs zvec

col <- rszvec_open("/tmp/my_collection", dim = 4)

rszvec_add(col, "doc_1", c(0.1, 0.2, 0.3, 0.4))
rszvec_add(col, "doc_2", c(0.2, 0.3, 0.4, 0.1))

rszvec_search(col, c(0.4, 0.3, 0.3, 0.1), n = 5)
#      id score
# 1 doc_2  0.30
# 2 doc_1  0.23
```

### rzvec — full API

```r
remotes::install_github("keneo/play-zvec/rzvec")

library(rzvec)
rzvec_install()   # one-time setup

schema <- collection_schema(
  "my_docs",
  vectors = vector_schema("embedding", zvec_data_type()$VECTOR_FP32, 4L)
)
col <- create_collection("/tmp/my_collection", schema)

col_insert(col,
  zvec_doc("doc_1", vectors = list(embedding = c(0.1, 0.2, 0.3, 0.4))),
  zvec_doc("doc_2", vectors = list(embedding = c(0.2, 0.3, 0.4, 0.1)))
)

col_query(col, vector_query("embedding", vector = c(0.4, 0.3, 0.3, 0.1)), topk = 5L)
```

## Example scripts

| Script | Abstraction level |
|---|---|
| [`play-zvec-example.R`](play-zvec-example.R) | Raw `reticulate` + Python zvec API |
| [`play-rzvec-example.R`](play-rzvec-example.R) | `rzvec` R package |
| [`play-rszvec-example.R`](play-rszvec-example.R) | `rszvec` R package |

## Platform support

Determined by the `zvec` Python package, which ships binary wheels for:

| Platform | x86_64 | arm64 |
|---|---|---|
| Linux | ✅ | ✅ |
| macOS | — | ✅ |
| Windows | — | — |

Python 3.10, 3.11, and 3.12 are supported. Python 3.9 and earlier are not.

On Linux, the following system packages must be installed before calling
`rszvec_install()` / `rzvec_install()`:

```bash
sudo apt install python3-venv python3-dev python3-pip
```

## License

MIT
