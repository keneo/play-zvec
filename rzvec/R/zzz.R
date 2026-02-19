.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkg_env$zvec <- reticulate::import("zvec", delay_load = TRUE)
}

.zv <- function() .pkg_env$zvec

#' Install zvec Python package
#'
#' Creates a virtualenv in the user cache directory and installs the `zvec`
#' Python package into it, then activates it for the current session.
#'
#' @param envname Name of the virtualenv to create. Defaults to `"rzvec-venv"`.
#' @param method Passed to [reticulate::py_install()].
#' @param ... Additional arguments passed to [reticulate::py_install()].
#'
#' @return Invisibly `NULL`.
#' @export
rzvec_install <- function(envname = "rzvec-venv", method = "auto", ...) {
  venv_dir <- file.path(tools::R_user_dir("rzvec", "cache"), envname)
  reticulate::virtualenv_create(venv_dir)
  reticulate::py_install("zvec", envname = venv_dir, method = "virtualenv", ...)
  reticulate::use_virtualenv(venv_dir, required = TRUE)
  invisible(NULL)
}
