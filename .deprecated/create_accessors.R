#' @title Create accessor functions
#'
#' @param path \code{(character)} path to directory where dependencies are saved
#' @param app_nm  \code{(character)} name of this app
#'
#' @export
#'
create_accessors <- function(path = "data", deps, dep_update = dep_update_fn(fn = deps_update), app_nm = pkgload::pkg_name()) {

  files <- clean_null(UU::list.files2(path)) |>
    stringr::str_subset("\\.png^", negate = TRUE)
  db_files <- rdrop2::drop_dir() |>
    dplyr::mutate(client_modified = suppressMessages(lubridate::as_datetime(client_modified, tz = Sys.timezone())))
  # Find the list of dependencies, download & open
  deps_nm <- paste0("deps_", app_nm, ".rds")
  deps <- stringr::str_subset(db_files$path_display, deps_nm)
  if (UU::is_legit(deps)) {
    deps_updated <- file.info(file.path(path, deps_nm))$mtime %|% 0
    deps_needs_update <- deps_updated < db_files$client_modified[db_files$path_display == deps]
    if (UU::is_legit(deps_updated) && deps_needs_update)
      rdrop2::drop_download(deps, file.path(path, deps), overwrite = TRUE)
  }

  deps <- readRDS(file.path(path, deps_nm))
  to_dl <- UU::ext(db_files$name, strip = TRUE) %in% deps
  if (any(to_dl)) {
    db_files <- db_files |>
      dplyr::mutate(
        to_dl = to_dl,
        file_time = file.info(file.path(path, name))$mtime,
        needs_update = (file_time < client_modified) %|% FALSE
      )
    files_to_download <- dplyr::filter(db_files, to_dl & needs_update)
    if (nrow(files_to_download)) {
      UU::mkpath(path)
      apply(files_to_download, 1, rlang::as_function(~rdrop2::drop_download(.x["path_display"], file.path(path, .x["name"]), overwrite = TRUE)))
    }
  }

  purrr::map(UU::list.files2(path), accessor_create)
}
