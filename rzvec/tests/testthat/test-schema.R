test_that("zvec_data_type() returns accessible DataType enum", {
  skip_if_no_zvec()
  dt <- zvec_data_type()
  expect_s3_class(dt, "python.builtin.object")
  expect_s3_class(dt$VECTOR_FP32, "python.builtin.object")
  expect_s3_class(dt$STRING,      "python.builtin.object")
  expect_s3_class(dt$INT64,       "python.builtin.object")
})

test_that("vector_schema() creates Python object and coerces dimension to integer", {
  skip_if_no_zvec()
  # Pass numeric 4 (not 4L) — the wrapper must coerce it.
  vs <- vector_schema("embedding", zvec_data_type()$VECTOR_FP32, 4)
  expect_s3_class(vs, "python.builtin.object")
  expect_equal(vs$name, "embedding")
  expect_equal(reticulate::py_to_r(vs$dimension), 4L)
})

test_that("vector_schema() accepts integer dimension directly", {
  skip_if_no_zvec()
  vs <- vector_schema("vec", zvec_data_type()$VECTOR_FP32, 16L)
  expect_equal(reticulate::py_to_r(vs$dimension), 16L)
})

test_that("collection_schema() creates Python object with correct name", {
  skip_if_no_zvec()
  cs <- collection_schema("my_collection")
  expect_s3_class(cs, "python.builtin.object")
  expect_equal(cs$name, "my_collection")
})

test_that("collection_schema() accepts a single vector_schema()", {
  skip_if_no_zvec()
  vs <- vector_schema("emb", zvec_data_type()$VECTOR_FP32, 4L)
  cs <- collection_schema("col_with_vec", vectors = vs)
  expect_s3_class(cs, "python.builtin.object")
  expect_equal(cs$name, "col_with_vec")
})

test_that("field_schema() creates Python FieldSchema object", {
  skip_if_no_zvec()
  fs <- field_schema("title", zvec_data_type()$STRING)
  expect_s3_class(fs, "python.builtin.object")
  expect_equal(fs$name, "title")
})

test_that("field_schema() nullable argument is forwarded", {
  skip_if_no_zvec()
  fs_nullable     <- field_schema("score", zvec_data_type()$FLOAT, nullable = TRUE)
  fs_not_nullable <- field_schema("score", zvec_data_type()$FLOAT, nullable = FALSE)
  expect_true(reticulate::py_to_r(fs_nullable$nullable))
  expect_false(reticulate::py_to_r(fs_not_nullable$nullable))
})

test_that("collection_schema() accepts a list of field_schema() objects", {
  skip_if_no_zvec()
  fs <- field_schema("author", zvec_data_type()$STRING)
  vs <- vector_schema("emb", zvec_data_type()$VECTOR_FP32, 4L)
  cs <- collection_schema("full_schema", fields = list(fs), vectors = vs)
  expect_s3_class(cs, "python.builtin.object")
})
