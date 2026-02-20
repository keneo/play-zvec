# bench-rszvec-vs-zvec.R
#
# Benchmarks three abstraction levels against each other:
#   rszvec   — high-level R API (5 functions, data.frame results)
#   rzvec    — thin R wrappers over every Python method
#   zvec_py  — pure Python calling zvec directly (bench-zvec-python.py)
#
# Run from anywhere:
#   Rscript bench/bench-rszvec-vs-zvec.R
#
# Outputs are written next to this script (bench/).

# Ensure paths are relative to this script, not the caller's working directory.
.bench_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f)) dirname(normalizePath(f, mustWork = FALSE)) else getwd()
})
setwd(.bench_dir)
# Results are written to bench-rszvec-vs-zvec.csv.
# Use report-rszvec-vs-zvec.R to visualise them.

library(rszvec)   # sets RETICULATE_PYTHON via .onLoad
library(rzvec)

# ---------------------------------------------------------------------------
# Step 1: run the pure-Python baseline and collect its CSV
# ---------------------------------------------------------------------------

python_bin <- file.path(
  tools::R_user_dir("rzvec", "cache"), "rzvec-venv", "bin", "python"
)
if (!file.exists(python_bin))
  stop("rzvec venv not found at: ", python_bin,
       "\nRun rszvec_install() first.")

cat("Running pure-Python baseline...\n")
ret <- system2(python_bin, "bench-zvec-python.py", stdout = TRUE, stderr = TRUE)
cat(ret, sep = "\n")

if (!file.exists("bench-zvec-python.csv"))
  stop("bench-zvec-python.py did not produce bench-zvec-python.csv")

py_results <- read.csv("bench-zvec-python.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Parameters  (must match bench-zvec-python.py)
# ---------------------------------------------------------------------------

set.seed(42)
DIM             <- 128L
N_SINGLE        <- 300L
N_BULK_SIZES    <- c(500L, 2000L, 5000L)
N_SEARCH_SIZES  <- c(1000L, 10000L, 50000L)
N_QUERIES       <- 100L

cat(sprintf("\nR-layer benchmark  dim=%d\n\n", DIM))

# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

timed <- function(expr) {
  gc(verbose = FALSE)
  t <- proc.time()
  force(expr)
  (proc.time() - t)[["elapsed"]]
}

fresh_dir <- function(tag) {
  d <- sprintf("/tmp/zvec_bench_%s", tag)
  unlink(d, recursive = TRUE)
  d
}

drop_col <- function(col, dir) {
  col <- NULL
  gc(verbose = FALSE)
  Sys.sleep(0.05)
  unlink(dir, recursive = TRUE)
}

make_rs_col <- function(dir) rszvec_open(dir, dim = DIM)

make_rv_col <- function(dir) {
  create_collection(dir, collection_schema(
    "bench",
    vectors = vector_schema("vec", zvec_data_type()$VECTOR_FP32, DIM)
  ))
}

rv_bulk_insert <- function(col, ids, mat) {
  n    <- nrow(mat)
  docs <- lapply(seq_len(n), function(i)
    zvec_doc(ids[[i]], vectors = list(vec = mat[i, ])))
  for (s in seq(1L, n, by = 1000L)) {
    idx <- seq(s, min(s + 999L, n))
    do.call(col_insert, c(list(col), docs[idx]))
  }
}

results <- list()

record <- function(operation, impl, n, reps, secs) {
  results[[length(results) + 1L]] <<- data.frame(
    operation  = operation,
    impl       = impl,
    n          = as.integer(n),
    dim        = DIM,
    reps       = as.integer(reps),
    total_secs = secs,
    per_op_ms  = secs / reps * 1000,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 1. Single insert
# ---------------------------------------------------------------------------

cat("1. Single insert  (n =", N_SINGLE, "calls per implementation)\n")

ids  <- paste0("d", seq_len(N_SINGLE))
vecs <- lapply(seq_len(N_SINGLE), function(i) rnorm(DIM))

for (impl in c("rszvec", "rzvec")) {
  d   <- fresh_dir(sprintf("%s_single", impl))
  col <- if (impl == "rszvec") make_rs_col(d) else make_rv_col(d)

  t <- if (impl == "rszvec")
    timed(for (i in seq_len(N_SINGLE)) rszvec_add(col, ids[[i]], vecs[[i]]))
  else
    timed(for (i in seq_len(N_SINGLE))
      col_insert(col, zvec_doc(ids[[i]], vectors = list(vec = vecs[[i]]))))

  record("single_insert", impl, N_SINGLE, N_SINGLE, t)
  cat(sprintf("  %-8s  %6.2f ms/call\n", impl, t / N_SINGLE * 1000))
  drop_col(col, d)
}

# ---------------------------------------------------------------------------
# 2. Bulk insert
# ---------------------------------------------------------------------------

cat("\n2. Bulk insert\n")

for (n in N_BULK_SIZES) {
  ids <- paste0("d", seq_len(n))
  mat <- matrix(rnorm(n * DIM), nrow = n)

  for (impl in c("rszvec", "rzvec")) {
    d   <- fresh_dir(sprintf("%s_bulk_%d", impl, n))
    col <- if (impl == "rszvec") make_rs_col(d) else make_rv_col(d)

    t <- if (impl == "rszvec")
      timed(rszvec_add_many(col, ids, mat))
    else
      timed(rv_bulk_insert(col, ids, mat))

    record("bulk_insert", impl, n, n, t)
    cat(sprintf("  %-8s  n = %5s  %7.0f docs/s\n",
                impl, format(n, big.mark = ","), n / t))
    drop_col(col, d)
  }
}

# ---------------------------------------------------------------------------
# 3. Search latency
# ---------------------------------------------------------------------------

cat("\n3. Search latency  (", N_QUERIES, "queries per configuration)\n", sep = "")

for (n in N_SEARCH_SIZES) {
  ids     <- paste0("d", seq_len(n))
  mat     <- matrix(rnorm(n * DIM), nrow = n)
  queries <- lapply(seq_len(N_QUERIES), function(i) rnorm(DIM))

  for (impl in c("rszvec", "rzvec")) {
    d   <- fresh_dir(sprintf("%s_search_%d", impl, n))
    col <- if (impl == "rszvec") make_rs_col(d) else make_rv_col(d)

    if (impl == "rszvec") rszvec_add_many(col, ids, mat) else rv_bulk_insert(col, ids, mat)

    t <- if (impl == "rszvec")
      timed(for (q in queries) rszvec_search(col, q, n = 10L))
    else
      timed(for (q in queries) col_query(col, vector_query("vec", vector = q), topk = 10L))

    record("search", impl, n, N_QUERIES, t)
    cat(sprintf("  %-8s  n = %6s  %6.2f ms/query\n",
                impl, format(n, big.mark = ","), t / N_QUERIES * 1000))
    drop_col(col, d)
  }
}

# ---------------------------------------------------------------------------
# Combine R results with Python baseline and save
# ---------------------------------------------------------------------------

df  <- rbind(do.call(rbind, results), py_results)
out <- "bench-rszvec-vs-zvec.csv"
write.csv(df, out, row.names = FALSE)
cat("\nSaved", nrow(df), "rows ->", out, "\n")
