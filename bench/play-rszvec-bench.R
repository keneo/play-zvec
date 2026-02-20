library(rszvec)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

timed <- function(expr) {
  gc(verbose = FALSE)
  t <- proc.time()
  force(expr)
  elapsed <- (proc.time() - t)[["elapsed"]]
  elapsed
}

header <- function(...) cat("\n", sprintf(...), "\n", rep("-", 60), "\n", sep = "")

report <- function(label, secs, n = NULL) {
  if (!is.null(n)) {
    rate <- n / secs
    cat(sprintf("  %-40s %6.2f s  (%s docs/s)\n",
                label, secs, format(round(rate), big.mark = ",")))
  } else {
    cat(sprintf("  %-40s %6.2f ms\n", label, secs * 1000))
  }
}

# zvec holds a RocksDB lock on the collection directory. We must NULL the
# handle and GC before unlink() to avoid "lock held by current process" errors.
with_col <- function(n_docs, dim, fn) {
  dir <- sprintf("/tmp/rzvec_bench_%d_%d", n_docs, dim)
  unlink(dir, recursive = TRUE)
  col <- rszvec_open(dir, dim = dim)
  on.exit({
    col <- NULL
    gc(verbose = FALSE)
    Sys.sleep(0.05)
    unlink(dir, recursive = TRUE)
  })
  fn(col)
}

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------

set.seed(42)
DIM       <- 128L          # typical sentence-embedding size
SIZES     <- c(1e3, 1e4, 1e5)
N_QUERIES <- 200L          # queries to average over for search latency

cat("rszvec performance benchmark\n")
cat(sprintf("dim=%d  query_reps=%d\n", DIM, N_QUERIES))

# ---------------------------------------------------------------------------
# 1. Bulk insert throughput — rszvec_add_many() with a matrix
# ---------------------------------------------------------------------------

header("1. Bulk insert — rszvec_add_many(col, ids, matrix)")

for (n in SIZES) {
  ids <- paste0("d", seq_len(n))
  mat <- matrix(rnorm(n * DIM), nrow = n)
  with_col(n, DIM, function(col) {
    t <- timed(rszvec_add_many(col, ids, mat))
    report(sprintf("n = %s", format(n, big.mark = ",")), t, n)
  })
}

# ---------------------------------------------------------------------------
# 2. Single-insert throughput — rszvec_add() in a loop
# ---------------------------------------------------------------------------

header("2. Single insert — rszvec_add() loop  (n = 1,000)")

n   <- 1000L
ids <- paste0("d", seq_len(n))
mat <- matrix(rnorm(n * DIM), nrow = n)
with_col(n, DIM, function(col) {
  t <- timed(for (i in seq_len(n)) rszvec_add(col, ids[i], mat[i, ]))
  report("n = 1,000", t, n)
  cat("  (compare to bulk above to see overhead per call)\n")
})

# ---------------------------------------------------------------------------
# 3. Search latency vs. collection size
# ---------------------------------------------------------------------------

header("3. Search latency — rszvec_search(col, query, n=10)")

for (n in SIZES) {
  ids <- paste0("d", seq_len(n))
  mat <- matrix(rnorm(n * DIM), nrow = n)
  with_col(n, DIM, function(col) {
    rszvec_add_many(col, ids, mat)
    queries <- lapply(seq_len(N_QUERIES), function(i) rnorm(DIM))
    t <- timed(for (q in queries) rszvec_search(col, q, n = 10L))
    report(sprintf("n = %s  (avg over %d queries)",
                   format(n, big.mark = ","), N_QUERIES),
           t / N_QUERIES)
  })
}

# ---------------------------------------------------------------------------
# 4. Search latency vs. top-k
# ---------------------------------------------------------------------------

header("4. Search latency vs. top-k  (n = 10,000 docs)")

ids <- paste0("d", seq_len(10000L))
mat <- matrix(rnorm(10000L * DIM), nrow = 10000L)
with_col(10000L, DIM, function(col) {
  rszvec_add_many(col, ids, mat)
  queries <- lapply(seq_len(N_QUERIES), function(i) rnorm(DIM))
  for (k in c(1L, 10L, 50L, 100L)) {
    t <- timed(for (q in queries) rszvec_search(col, q, n = k))
    report(sprintf("top-%d", k), t / N_QUERIES)
  }
})

# ---------------------------------------------------------------------------
# 5. Search latency vs. vector dimension
# ---------------------------------------------------------------------------

header("5. Search latency vs. dimension  (n = 10,000 docs, top-10)")

for (dim in c(32L, 128L, 384L, 768L)) {
  ids <- paste0("d", seq_len(10000L))
  mat <- matrix(rnorm(10000L * dim), nrow = 10000L)
  with_col(10000L, dim, function(col) {
    rszvec_add_many(col, ids, mat)
    queries <- lapply(seq_len(N_QUERIES), function(i) rnorm(dim))
    t <- timed(for (q in queries) rszvec_search(col, q, n = 10L))
    report(sprintf("dim = %d", dim), t / N_QUERIES)
  })
}

cat("\nDone.\n")
