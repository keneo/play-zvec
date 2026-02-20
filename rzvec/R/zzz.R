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
#' On Linux x86_64 systems without AVX-512 support (e.g. AMD processors or
#' pre-Skylake Intel), the pre-built wheel would crash with SIGILL. In that
#' case, `rzvec_install()` automatically builds `zvec` from source instead.
#' Building from source requires `git`, `cmake`, `ninja-build`, and `g++`.
#'
#' @param envname Name of the virtualenv to create. Defaults to `"rzvec-venv"`.
#' @param ... Additional arguments passed to [reticulate::py_install()]
#'   (wheel install path only; ignored when building from source).
#'
#' @return Invisibly `NULL`.
#' @export
rzvec_install <- function(envname = "rzvec-venv", ...) {
  venv_dir <- file.path(tools::R_user_dir("rzvec", "cache"), envname)
  reticulate::virtualenv_create(venv_dir)

  if (.linux_x86_without_avx512()) {
    message(
      "WARNING: AVX-512 is not available on this system. The pre-built zvec\n",
      "wheel requires AVX-512 and would crash at runtime (see known bug:\n",
      "https://github.com/alibaba/zvec/issues/128).\n",
      "\n",
      "Falling back to building zvec from source. This may take several minutes.\n",
      "Requires: git, cmake, ninja-build, g++"
    )
    .install_zvec_from_source(venv_dir)
  } else {
    reticulate::py_install("zvec", envname = venv_dir, method = "virtualenv", ...)
  }

  reticulate::use_virtualenv(venv_dir, required = TRUE)
  invisible(NULL)
}

# TODO: remove this workaround once https://github.com/alibaba/zvec/issues/128
# is fixed and zvec ships a wheel compiled without AVX-512 assumptions.
.linux_x86_without_avx512 <- function() {
  si <- Sys.info()
  if (si[["sysname"]] != "Linux" || si[["machine"]] != "x86_64") return(FALSE)
  cpuinfo <- tryCatch(readLines("/proc/cpuinfo"), error = function(e) character(0))
  !any(grepl("avx512f", cpuinfo))
}

.install_zvec_from_source <- function(venv_dir) {
  src_dir <- file.path(tempdir(), "zvec-src")
  on.exit(unlink(src_dir, recursive = TRUE), add = TRUE)

  if (system2("git", c("clone", "--recurse-submodules", "--depth=1",
                        "https://github.com/alibaba/zvec.git", src_dir)) != 0)
    stop("git clone failed — is git installed?", call. = FALSE)

  pip <- file.path(venv_dir, "bin", "pip")
  if (system2(pip, c("install", src_dir)) != 0)
    stop(
      "Building zvec from source failed.\n",
      "Ensure cmake, ninja-build, and g++ are installed.",
      call. = FALSE
    )

  invisible(NULL)
}
