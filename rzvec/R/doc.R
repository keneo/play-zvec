#' Create a zvec document
#'
#' @param id Character. Document ID.
#' @param vectors Named list of vector values, or `NULL`.
#' @param fields Named list of field values, or `NULL`.
#'
#' @return A Python `Doc` object.
#' @export
zvec_doc <- function(id, vectors = NULL, fields = NULL) {
  .zv()$Doc(
    id = id,
    vectors = if (!is.null(vectors)) reticulate::dict(vectors) else NULL,
    fields = if (!is.null(fields)) reticulate::dict(fields) else NULL
  )
}

#' Create a vector query
#'
#' @param field_name Character. The name of the vector field to search.
#' @param vector Numeric vector of query values, or `NULL`.
#' @param id Character document ID for ID-based lookup, or `NULL`.
#' @param param Additional query parameters, or `NULL`.
#'
#' @return A Python `VectorQuery` object.
#' @export
vector_query <- function(field_name, vector = NULL, id = NULL, param = NULL) {
  .zv()$VectorQuery(
    field_name = field_name,
    vector = vector,
    id = id,
    param = param
  )
}
