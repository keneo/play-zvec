#' Create and open a collection
#'
#' Creates a new collection on disk at `path` with the given `schema` and opens
#' it. Equivalent to `zvec.create_and_open()`.
#'
#' @param path Character. Directory path for the collection.
#' @param schema A [collection_schema()] object.
#' @param option Optional collection options, or `NULL`.
#'
#' @return A Python collection object.
#' @export
create_collection <- function(path, schema, option = NULL) {
  .zv()$create_and_open(path = path, schema = schema, option = option)
}

#' Open an existing collection
#'
#' @param path Character. Directory path of the collection.
#' @param option Optional collection options, or `NULL`.
#'
#' @return A Python collection object.
#' @export
open_collection <- function(path, option = NULL) {
  if (is.null(option)) .zv()$open(path) else .zv()$open(path, option)
}

#' Insert documents into a collection
#'
#' @param col A collection object returned by [create_collection()] or
#'   [open_collection()].
#' @param ... [zvec_doc()] objects to insert.
#'
#' @return The result of the insert call (invisibly).
#' @export
col_insert <- function(col, ...) {
  invisible(col$insert(list(...)))
}

#' Upsert documents into a collection
#'
#' @param col A collection object.
#' @param ... [zvec_doc()] objects to upsert.
#'
#' @return The result of the upsert call (invisibly).
#' @export
col_upsert <- function(col, ...) {
  invisible(col$upsert(list(...)))
}

#' Update documents in a collection
#'
#' @param col A collection object.
#' @param ... [zvec_doc()] objects to update.
#'
#' @return The result of the update call (invisibly).
#' @export
col_update <- function(col, ...) {
  invisible(col$update(list(...)))
}

#' Delete documents by ID
#'
#' @param col A collection object.
#' @param ids Character vector of document IDs to delete.
#'
#' @return The result of the delete call (invisibly).
#' @export
col_delete <- function(col, ids) {
  invisible(col$delete(ids))
}

#' Delete documents by filter
#'
#' @param col A collection object.
#' @param filter A filter expression.
#'
#' @return The result of the delete call (invisibly).
#' @export
col_delete_by_filter <- function(col, filter) {
  invisible(col$delete_by_filter(filter))
}

#' Fetch documents by ID
#'
#' @param col A collection object.
#' @param ids Character vector of document IDs to fetch.
#'
#' @return A list of [zvec_doc()] objects.
#' @export
col_fetch <- function(col, ids) {
  col$fetch(ids)
}

#' Query a collection by vector similarity
#'
#' @param col A collection object.
#' @param vectors A [vector_query()] object, or a list thereof, or `NULL`.
#' @param topk Integer. Number of nearest neighbours to return. Default `10L`.
#' @param filter A filter expression, or `NULL`.
#' @param include_vector Logical. Whether to include vectors in the result.
#'   Default `FALSE`.
#' @param output_fields Character vector of field names to include, or `NULL`.
#' @param reranker A reranker object, or `NULL`.
#'
#' @return Query results from the collection.
#' @export
col_query <- function(col, vectors = NULL, topk = 10L, filter = NULL,
                      include_vector = FALSE, output_fields = NULL,
                      reranker = NULL) {
  col$query(
    vectors = vectors,
    topk = as.integer(topk),
    filter = filter,
    include_vector = include_vector,
    output_fields = output_fields,
    reranker = reranker
  )
}

#' Flush a collection to disk
#'
#' @param col A collection object.
#'
#' @return Invisibly `NULL`.
#' @export
col_flush <- function(col) {
  invisible(col$flush())
}

#' Get the schema of a collection
#'
#' @param col A collection object.
#'
#' @return The Python `CollectionSchema` of the collection.
#' @export
col_schema <- function(col) {
  col$schema
}

#' Get statistics for a collection
#'
#' @param col A collection object.
#'
#' @return A statistics object for the collection.
#' @export
col_stats <- function(col) {
  col$stats
}
