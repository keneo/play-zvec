test_that("hnsw_index_param() creates a Python object", {
  skip_if_no_zvec()
  p <- hnsw_index_param()
  expect_s3_class(p, "python.builtin.object")
})

test_that("flat_index_param() creates a Python object", {
  skip_if_no_zvec()
  p <- flat_index_param()
  expect_s3_class(p, "python.builtin.object")
})

test_that("ivf_index_param() creates a Python object", {
  skip_if_no_zvec()
  p <- ivf_index_param()
  expect_s3_class(p, "python.builtin.object")
})

test_that("invert_index_param() creates a Python object", {
  skip_if_no_zvec()
  p <- invert_index_param()
  expect_s3_class(p, "python.builtin.object")
})

test_that("col_create_index() and col_drop_index() run without error", {
  skip_if_no_zvec()
  dir <- zvec_tempdir()
  schema <- collection_schema("index_test_col",
    vectors = vector_schema("emb", zvec_data_type()$VECTOR_FP32, 4L))
  col <- create_collection(dir, schema)
  withr::defer(try(col$destroy(), silent = TRUE))
  col_insert(col,
    zvec_doc("i1", vectors = list(emb = c(1, 0, 0, 0))),
    zvec_doc("i2", vectors = list(emb = c(0, 1, 0, 0)))
  )
  col_flush(col)

  expect_no_error(col_create_index(col, "emb", hnsw_index_param()))
  expect_no_error(col_drop_index(col, "emb"))
})

test_that("col_optimize() runs without error", {
  skip_if_no_zvec()
  col <- new_temp_col()
  col_insert(col, zvec_doc("opt1", vectors = list(emb = c(1, 0, 0, 0))))
  col_flush(col)
  expect_no_error(col_optimize(col))
})
