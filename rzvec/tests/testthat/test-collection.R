test_that("create_collection() creates directory on disk", {
  skip_if_no_zvec()
  dir <- zvec_tempdir()
  schema <- collection_schema("test_col",
    vectors = vector_schema("emb", zvec_data_type()$VECTOR_FP32, 4L))
  col <- create_collection(dir, schema)
  expect_s3_class(col, "python.builtin.object")
  expect_true(dir.exists(dir))
})

test_that("open_collection() reopens an existing collection", {
  skip_if_no_zvec()
  dir <- zvec_tempdir()
  schema <- collection_schema("reopen_col",
    vectors = vector_schema("emb", zvec_data_type()$VECTOR_FP32, 4L))
  col1 <- create_collection(dir, schema)
  # Release the write lock before reopening: drop the R reference and run
  # both the R and Python garbage collectors.
  rm(col1); gc()
  reticulate::py_run_string("import gc; gc.collect()")

  col2 <- open_collection(dir)
  expect_s3_class(col2, "python.builtin.object")
})

test_that("col_insert() inserts documents without error", {
  skip_if_no_zvec()
  col <- new_temp_col()
  expect_no_error(
    col_insert(col,
      zvec_doc("a", vectors = list(emb = VEC_A)),
      zvec_doc("b", vectors = list(emb = VEC_B))
    )
  )
})

test_that("col_query() returns a list of results", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col,
    zvec_doc("a", vectors = list(emb = VEC_A)),
    zvec_doc("b", vectors = list(emb = VEC_B))
  )
  res <- col_query(col, vector_query("emb", vector = VEC_A), topk = 10L)
  expect_type(res, "list")
  expect_gte(length(res), 1L)
})

test_that("col_query() returns results in descending score order", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col,
    zvec_doc("near", vectors = list(emb = VEC_A)),   # dot(VEC_A, VEC_A) = 1
    zvec_doc("far",  vectors = list(emb = VEC_B))    # dot(VEC_B, VEC_A) = 0
  )
  res <- col_query(col, vector_query("emb", vector = VEC_A), topk = 10L)
  expect_equal(length(res), 2L)
  expect_equal(res[[1]]$id, "near")
  expect_equal(res[[2]]$id, "far")
  expect_gte(res[[1]]$score, res[[2]]$score)
})

test_that("col_query() respects topk", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col,
    zvec_doc("a", vectors = list(emb = VEC_A)),
    zvec_doc("b", vectors = list(emb = VEC_B)),
    zvec_doc("c", vectors = list(emb = VEC_C)),
    zvec_doc("d", vectors = list(emb = VEC_D))
  )
  res <- col_query(col, vector_query("emb", vector = VEC_A), topk = 2L)
  expect_lte(length(res), 2L)
})

test_that("col_query() with numeric topk coerces to integer", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("a", vectors = list(emb = VEC_A)))
  # Pass numeric 5 instead of 5L — col_query must coerce internally.
  expect_no_error(
    col_query(col, vector_query("emb", vector = VEC_A), topk = 5)
  )
})

test_that("col_fetch() retrieves inserted documents", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("fetch_me", vectors = list(emb = VEC_A)))
  res <- col_fetch(col, list("fetch_me"))
  expect_length(res, 1L)
  expect_equal(res[[1]]$id, "fetch_me")
})

test_that("col_fetch() silently drops non-existent IDs", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("exists", vectors = list(emb = VEC_A)))
  res <- col_fetch(col, list("exists", "ghost"))
  expect_length(res, 1L)
  expect_equal(res[[1]]$id, "exists")
})

test_that("col_upsert() inserts new documents", {
  skip_if_no_zvec()
  col <- new_temp_col()
  expect_no_error(
    col_upsert(col, zvec_doc("upserted", vectors = list(emb = VEC_A)))
  )
  res <- col_query(col, vector_query("emb", vector = VEC_A), topk = 5L)
  ids <- vapply(res, function(r) r$id, character(1))
  expect_true("upserted" %in% ids)
})

test_that("col_upsert() overwrites an existing document", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("u1", vectors = list(emb = VEC_A)))
  # Overwrite with VEC_B
  col_upsert(col, zvec_doc("u1", vectors = list(emb = VEC_B)))

  # Query near VEC_B — "u1" should appear near the top
  res <- col_query(col, vector_query("emb", vector = VEC_B), topk = 5L)
  ids <- vapply(res, function(r) r$id, character(1))
  expect_true("u1" %in% ids)
})

test_that("col_delete() removes a document from query results", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col,
    zvec_doc("keep",   vectors = list(emb = VEC_A)),
    zvec_doc("remove", vectors = list(emb = VEC_B))
  )
  col_delete(col, list("remove"))

  res   <- col_query(col, vector_query("emb", vector = VEC_B), topk = 10L)
  ids   <- vapply(res, function(r) r$id, character(1))
  expect_false("remove" %in% ids)
})

test_that("col_flush() completes without error", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("flush_doc", vectors = list(emb = VEC_A)))
  expect_no_error(col_flush(col))
})

test_that("col_schema() returns the collection schema", {
  skip_if_no_zvec()
  col <- new_temp_col(name = "schema_test_col")
  s <- col_schema(col)
  expect_s3_class(s, "python.builtin.object")
  expect_equal(s$name, "schema_test_col")
})

test_that("col_stats() returns a stats object", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("s1", vectors = list(emb = VEC_A)))
  stats <- col_stats(col)
  expect_s3_class(stats, "python.builtin.object")
})
