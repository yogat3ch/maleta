#' Clean rds files with NULL values
#'
#' @param files \code{(chr)} of files to clean NULL saves
#'
#' @return \code{(chr)} of valid files after cleaning

clean_null <- function(files) {
  .rds <- stringr::str_subset(files, "rds$")
  .sizes <- file.size(.rds)
  file.remove(.rds[.sizes == 44])
  files[!files %in% .rds[.sizes == 44]]
}
