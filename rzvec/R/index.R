#' Create HNSW index parameters
#'
#' @param ... Named arguments passed to `zvec$HnswIndexParam()`.
#'
#' @return A Python `HnswIndexParam` object.
#' @export
hnsw_index_param <- function(...) {
  .zv()$HnswIndexParam(...)
}

#' Create IVF index parameters
#'
#' @param ... Named arguments passed to `zvec$IVFIndexParam()`.
#'
#' @return A Python `IVFIndexParam` object.
#' @export
ivf_index_param <- function(...) {
  .zv()$IVFIndexParam(...)
}

#' Create flat index parameters
#'
#' @param ... Named arguments passed to `zvec$FlatIndexParam()`.
#'
#' @return A Python `FlatIndexParam` object.
#' @export
flat_index_param <- function(...) {
  .zv()$FlatIndexParam(...)
}

#' Create inverted index parameters
#'
#' @param ... Named arguments passed to `zvec$InvertIndexParam()`.
#'
#' @return A Python `InvertIndexParam` object.
#' @export
invert_index_param <- function(...) {
  .zv()$InvertIndexParam(...)
}

#' Create an index on a collection field
#'
#' @param col A collection object.
#' @param field_name Character. Name of the field to index.
#' @param index_param An index param object (e.g., from [hnsw_index_param()]).
#' @param option Optional index creation options, or `NULL`.
#'
#' @return The result of the create_index call (invisibly).
#' @export
col_create_index <- function(col, field_name, index_param, option = NULL) {
  invisible(col$create_index(
    field_name = field_name,
    index_param = index_param,
    option = option
  ))
}

#' Drop an index from a collection field
#'
#' @param col A collection object.
#' @param field_name Character. Name of the field whose index to drop.
#'
#' @return The result of the drop_index call (invisibly).
#' @export
col_drop_index <- function(col, field_name) {
  invisible(col$drop_index(field_name = field_name))
}

#' Optimize a collection
#'
#' Triggers compaction or other optimization routines on the collection.
#'
#' @param col A collection object.
#' @param option Optional optimization options, or `NULL`.
#'
#' @return The result of the optimize call (invisibly).
#' @export
col_optimize <- function(col, option = NULL) {
  invisible(col$optimize(option = option))
}
