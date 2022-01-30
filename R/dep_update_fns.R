#' Update a dependency saved in Dropbox (if it's older)
#'
#' @param dep_filepath \code{(chr)} of the filepath
#' @param dest_local_dir \code{(dest_dir)} directory where the downloaded file will be saved.
#' @param dest_remote_dir \code{(dest_dir)} directory on the remote server where `dep_filepath` is saved. The location will be transformed as follows: \code{\link[base]{file.path}(dest_remote_dir, \link[base]{basename}(dep_filepath)}
#'
#' @return dep_filepath contents
#' @export


dep_update_dropbox <- function(dep_filepath = file.path("data", paste0("deps_",pkgload::pkg_name() , ".rds")), dest_dir = "data") {
  db_auth()
  remote <- rdrop2::drop_exists(dep_filepath)

  file_exists <- c(local = file.exists(dep_filepath), remote = remote)
  if (!any(file_exists))
    UU::gbort("{.path {dep_filepath}} does not exist locally or remotely.")

  if (file_exists["local"]) {
    last_updated <- file.info(dep_filepath)$mtime
    if (file_exists["remote"]) {
      md <- rdrop2::drop_get_metadata(dep_filepath)
      needs_update <- md$client_modified > last_updated
    } else
      needs_update <- FALSE
  } else {
    needs_update <- TRUE
  }

  if (needs_update)
    rdrop2::drop_download(dep_filepath, dep_filepath, overwrite = TRUE)
  UU::file_fn(dep_filepath)(dep_filepath)
}


#' Construct a dependency update function
#'
#' @param args \code{(pairlist)} See \link[rlang]{pairlist2}
#' @param body \code{(expression)} See \link[rlang]{new_function}
#' @param fn \code{(function)} Use to pass the default dependency update function
#' @seealso rlang::new_function
#' @return \code{(function)}
#' @export

dep_update_fn <- function(args, body, fn) {
  if (missing(args) && rlang::is_function(fn))
    fn
  else
    rlang::new_function(args, body)
}
