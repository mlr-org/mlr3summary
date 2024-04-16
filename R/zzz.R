#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import backports
#' @import cli
#' @importFrom future.apply future_Map
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nolint
  # nocov start
  backports::import(pkgname)
} # nocov end

mlr3misc::leanify_package()
