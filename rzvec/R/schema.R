#' Create a collection schema
#'
#' @param name Character. Name of the collection.
#' @param fields List of field schemas created with [field_schema()], or `NULL`.
#' @param vectors A single [vector_schema()] or list thereof, or `NULL`.
#'
#' @return A Python `CollectionSchema` object.
#' @export
collection_schema <- function(name, fields = NULL, vectors = NULL) {
  .zv()$CollectionSchema(name = name, fields = fields, vectors = vectors)
}

#' Create a field schema
#'
#' @param name Character. Field name.
#' @param data_type A data type constant from [zvec_data_type()].
#' @param nullable Logical. Whether the field is nullable. Default `FALSE`.
#' @param index_param An index param object, or `NULL`.
#'
#' @return A Python `FieldSchema` object.
#' @export
field_schema <- function(name, data_type, nullable = FALSE, index_param = NULL) {
  .zv()$FieldSchema(
    name = name,
    data_type = data_type,
    nullable = nullable,
    index_param = index_param
  )
}

#' Create a vector field schema
#'
#' @param name Character. Vector field name.
#' @param data_type A data type constant from [zvec_data_type()], typically
#'   `zvec_data_type()$VECTOR_FP32`.
#' @param dimension Integer. Number of dimensions.
#' @param index_param An index param object, or `NULL`.
#'
#' @return A Python `VectorSchema` object.
#' @export
vector_schema <- function(name, data_type, dimension, index_param = NULL) {
  .zv()$VectorSchema(
    name = name,
    data_type = data_type,
    dimension = as.integer(dimension),
    index_param = index_param
  )
}

#' Access zvec DataType constants
#'
#' Returns the Python `DataType` enum object. Access individual constants with
#' `$`, e.g. `zvec_data_type()$VECTOR_FP32`.
#'
#' @return The Python `DataType` object.
#' @export
zvec_data_type <- function() {
  .zv()$DataType
}
