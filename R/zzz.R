#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import backports
#' @import cli
#' @importFrom future.apply future_Map future_mapply
#' @importFrom stats sd var
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nolint
  # nocov start
  backports::import(pkgname)
} # nocov end

leanify_package()
