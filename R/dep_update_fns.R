#' Update a dependency saved in Dropbox (if it's older)
#'
#' @param local_filepaths \code{(chr)} of the filepath(s)
#' @param remote_dir \code{(chr)} directory on the remote server where `local_filepath` is saved. The location will be transformed as follows: \code{\link[base]{file.path}(dest_remote_dir, \link[base]{basename}(dep_filepath)}
#'
#' @return dep_filepath contents
#' @export


dep_update_dropbox <- function(local_filepaths = readRDS(file.path("data", paste0("deps_",pkgload::pkg_name() , ".rds"))), remote_dir = NULL) {
  db_auth()
  local_deps <- tibble::tibble(local_fp = local_filepaths,
                               name = basename(local_fp))
  remote_deps <- rdrop2::drop_dir(remote_dir %||% "")
  local_deps <- dplyr::left_join(local_deps, remote_deps, by = "name") |>
    dplyr::mutate(local_updated = file.info(local_fp)$mtime,
                  missing = is.na(client_modified) & is.na(local_updated),
                  needs_update = (client_modified > local_updated) %|% is_downloadable,
                  downloaded = conditional_download(needs_update, path_lower, local_fp))





  # Messages
  new <- which(local_deps$downloaded)
  same <- which(!(local_deps$downloaded %|% FALSE))
  na <- which(local_deps$missing)
  nms <- local_deps$name
  msgs <- list(
    Updated = make_msg_list(nms[new], glue::glue_collapse("{cli::col_green(cli::symbol$tick)} {x}", sep = "\n")),
    `Not Updated` =  make_msg_list(nms[same], glue::glue_collapse("{cli::col_grey(cli::symbol$line)} {x}", sep = "\n")),
    Missing = make_msg_list(nms[na], glue::glue_collapse("{cli::col_red(cli::symbol$warning)} {x}", sep = "\n"))
  )
  purrr::iwalk(msgs, ~{
    if (!is.null(.x)) {
      cli::cat_line(cli::style_underline(.y,":\n"))
      cli::cat_line(.x)
    }
  })

  local_deps$local_fp
}

make_msg_list <- function(x, ..., cond = length(x) >= 1) {
  exp <- rlang::enexprs(...)
  if (rlang::eval_bare(rlang::enexpr(cond))) {
    .call <- rlang::call2(glue::glue, "\n", !!!exp, "\n")
    out <- rlang::eval_bare(.call)
  } else
    out <- NULL
  out
}

conditional_download <- Vectorize(function(cond, remote_fp, dest_fp, download_fn = rdrop2::drop_download, overwrite = TRUE) {
  if (cond %|% FALSE) {
    .args <- list(remote_fp, dest_fp)
    if ("overwrite" %in% rlang::fn_fmls_names(download_fn))
      .args$overwrite <- overwrite
    out <- rdrop2::drop_download(remote_fp, dest_fp, overwrite = TRUE)
  } else
    out <- cond
  out
})

#' Update a single file from dropbox
#'
#' @param local_filepath \code{(chr)} path to local file
#' @param remote_fp  \code{(chr)} path to remote file (if different)
#' @param do_update \code{(lgl)} Whether to do this update. If files are all updated on app load see (`create_accessors` argument `update_all`), this function can be muted.
#'
#' @return Nothing
#' @export

update_dropbox <- function(local_filepath, remote_fp = NULL) {

  remote_fp <- remote_fp %||% basename(local_filepath)
  file_exists <- c(local = file.exists(local_filepath),
                   remote = rdrop2::drop_exists(remote_fp))
  if (!any(file_exists))
    UU::gbort("{.path {local_filepath}} does not exist locally or remotely.")

  if (file_exists["local"]) {
    local_updated <- file.info(local_filepath)$mtime
    if (file_exists["remote"]) {
      needs_update <- rdrop2::drop_get_metadata(remote_fp)$client_modified > local_updated
    } else
      needs_update <- FALSE
  } else {
    needs_update <- TRUE
  }
  if (needs_update)
    rdrop2::drop_download(local_filepath, remote_fp, overwrite = TRUE)


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
