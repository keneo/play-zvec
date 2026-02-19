test_that("zvec_doc() creates Python Doc with correct id", {
  skip_if_no_zvec()
  doc <- zvec_doc("doc_1")
  expect_s3_class(doc, "python.builtin.object")
  expect_equal(doc$id, "doc_1")
})

test_that("zvec_doc() accepts named vector list", {
  skip_if_no_zvec()
  doc <- zvec_doc("doc_vec", vectors = list(emb = c(0.1, 0.2, 0.3, 0.4)))
  expect_s3_class(doc, "python.builtin.object")
  expect_equal(doc$id, "doc_vec")
})

test_that("zvec_doc() accepts named fields list", {
  skip_if_no_zvec()
  doc <- zvec_doc("doc_fields", fields = list(title = "Hello"))
  expect_s3_class(doc, "python.builtin.object")
  expect_equal(doc$id, "doc_fields")
})

test_that("zvec_doc() accepts both vectors and fields", {
  skip_if_no_zvec()
  doc <- zvec_doc(
    "doc_full",
    vectors = list(emb = c(1, 0, 0, 0)),
    fields  = list(title = "test")
  )
  expect_equal(doc$id, "doc_full")
})

test_that("vector_query() creates Python VectorQuery with field_name and vector", {
  skip_if_no_zvec()
  vq <- vector_query("emb", vector = c(1, 0, 0, 0))
  expect_s3_class(vq, "python.builtin.object")
  expect_equal(vq$field_name, "emb")
})

test_that("vector_query() accepts NULL vector (id-based lookup)", {
  skip_if_no_zvec()
  vq <- vector_query("emb", id = "doc_1")
  expect_s3_class(vq, "python.builtin.object")
})
