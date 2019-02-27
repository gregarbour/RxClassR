#' @importFrom httr status_code content GET timeout
#' @import dplyr
#' @importFrom jsonlite fromJSON
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Hey check me out I made a package!")
}
