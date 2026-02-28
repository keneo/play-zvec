library(reticulate)

# Locate the rzvec-venv virtualenv. It lives in the zvec-r-bindings project root,
# which may be at different depths relative to getwd() depending on how tests
# are invoked (devtools::test, testthat::test_dir, R CMD check, etc.).
# rprojroot::find_root() searches upward for the .Rproj file, which is the
# most reliable anchor.
.proj_root <- tryCatch(
  rprojroot::find_root(rprojroot::is_rstudio_project),
  error = function(e) {
    # Fallback: walk upward through candidate dirs.
    dirs <- c(getwd(),
              dirname(getwd()),
              dirname(dirname(getwd())),
              dirname(dirname(dirname(getwd()))))
    for (d in dirs) {
      if (dir.exists(file.path(d, "rzvec-venv"))) return(d)
    }
    NULL
  }
)

# Do NOT call normalizePath() here — that would follow the symlink and resolve
# to the base Homebrew Python instead of the virtualenv's interpreter.
.python_bin <- if (!is.null(.proj_root)) {
  p <- file.path(.proj_root, "rzvec-venv", "bin", "python")
  if (file.exists(p)) p else NULL
} else {
  NULL
}

# Setting RETICULATE_PYTHON before any Python is loaded is the most reliable
# way to direct reticulate to the correct interpreter.
if (!is.null(.python_bin)) {
  Sys.setenv(RETICULATE_PYTHON = .python_bin)
}

library(rzvec)

# Skip a test when the zvec Python package is not importable.
skip_if_no_zvec <- function() {
  skip_if_not(
    reticulate::py_module_available("zvec"),
    "zvec Python package not available"
  )
}

# zvec rejects macOS /var/folders/... paths (UUID-like components fail its path
# regex). Use paths under /tmp/ which pass the regex.
# IMPORTANT: do NOT pre-create the directory — zvec's create_and_open() requires
# the target path to be absent and creates it itself.
zvec_tempdir <- function(env = parent.frame()) {
  uid <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  base <- if (Sys.info()[["sysname"]] == "Darwin") "/tmp" else tempdir()
  dir <- file.path(base, paste0("zvec_", uid))
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}

# Create a fresh collection in a self-cleaning temp directory.
# Cleanup is deferred to the calling test's environment via withr.
# withr::defer runs in LIFO order, so col$destroy() (registered second)
# executes before unlink() (registered first inside zvec_tempdir),
# ensuring RocksDB handles are closed before the directory is removed.
new_temp_col <- function(name = "test_col", dim = 4L, env = parent.frame()) {
  dir <- zvec_tempdir(env = env)
  schema <- collection_schema(
    name,
    vectors = vector_schema("emb", zvec_data_type()$VECTOR_FP32, dim)
  )
  col <- create_collection(dir, schema)
  withr::defer(try(col$destroy(), silent = TRUE), envir = env)
  col
}

# Four orthogonal unit vectors useful for dot-product ordering tests.
VEC_A <- c(1, 0, 0, 0)
VEC_B <- c(0, 1, 0, 0)
VEC_C <- c(0, 0, 1, 0)
VEC_D <- c(0, 0, 0, 1)
