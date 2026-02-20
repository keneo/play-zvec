.VEC_FIELD <- "vec"
.COL_NAME  <- "rszvec_col"

#' Open or create a vector collection
#'
#' Opens an existing collection at `path` if the directory already exists,
#' or creates a new one with the given vector dimensionality.
#'
#' @param path Directory path for the collection.
#' @param dim Integer vector dimension. Required when creating a new collection;
#'   ignored when opening an existing one.
#'
#' @return A collection handle (opaque object passed to other `rszvec_*` functions).
#' @export
rszvec_open <- function(path, dim = NULL) {
  if (dir.exists(path)) {
    rzvec::open_collection(path)
  } else {
    if (is.null(dim)) {
      stop("`dim` is required when creating a new collection", call. = FALSE)
    }
    schema <- rzvec::collection_schema(
      .COL_NAME,
      vectors = rzvec::vector_schema(
        .VEC_FIELD,
        rzvec::zvec_data_type()$VECTOR_FP32,
        as.integer(dim)
      )
    )
    rzvec::create_collection(path, schema)
  }
}

#' Add a single document to a collection
#'
#' @param col Collection handle returned by [rszvec_open()].
#' @param id Character string identifier for the document.
#' @param vector Numeric vector embedding.
#'
#' @return `col`, invisibly.
#' @export
rszvec_add <- function(col, id, vector) {
  rzvec::col_insert(col, rzvec::zvec_doc(id, vectors = list(vec = vector)))
  invisible(col)
}

#' Add multiple documents to a collection
#'
#' @param col Collection handle returned by [rszvec_open()].
#' @param ids Character vector of document identifiers.
#' @param vectors Either a numeric matrix with one row per document, or a list
#'   of numeric vectors.
#'
#' @return `col`, invisibly.
#' @export
rszvec_add_many <- function(col, ids, vectors, batch_size = 1000L) {
  if (is.matrix(vectors)) {
    vectors <- lapply(seq_len(nrow(vectors)), function(i) vectors[i, ])
  }
  n <- length(ids)
  starts <- seq(1L, n, by = batch_size)
  for (s in starts) {
    idx <- seq(s, min(s + batch_size - 1L, n))
    docs <- lapply(idx, function(i) {
      rzvec::zvec_doc(ids[[i]], vectors = list(vec = vectors[[i]]))
    })
    do.call(rzvec::col_insert, c(list(col), docs))
  }
  invisible(col)
}

#' Search for nearest neighbours
#'
#' @param col Collection handle returned by [rszvec_open()].
#' @param vector Numeric query vector.
#' @param n Maximum number of results to return.
#'
#' @return A `data.frame` with columns `id` (character) and `score` (numeric),
#'   ordered by descending score. Returns a zero-row data frame when no results
#'   are found.
#' @export
rszvec_search <- function(col, vector, n = 10L) {
  res <- rzvec::col_query(
    col,
    rzvec::vector_query(.VEC_FIELD, vector = vector),
    topk = as.integer(n)
  )
  if (length(res) == 0L) {
    return(data.frame(id = character(0), score = numeric(0)))
  }
  data.frame(
    id    = vapply(res, function(r) r$id,    character(1)),
    score = vapply(res, function(r) r$score, numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Delete a document from a collection
#'
#' @param col Collection handle returned by [rszvec_open()].
#' @param id Character string identifier of the document to delete.
#'
#' @return `col`, invisibly.
#' @export
rszvec_delete <- function(col, id) {
  rzvec::col_delete(col, list(id))
  invisible(col)
}
