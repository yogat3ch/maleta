accessor_create <- function(.x) rlang::new_function(args = 
                      rlang::pairlist2(
                        path = rlang::expr(!!.x),
                        ... = ,
                      ),
                    body = base::quote({
                      last_modified <- file.info(path)$mtime %|% 0
                      md <- try(rdrop2::drop_get_metadata(basename(path)), silent = TRUE)
                      if (UU::is_legit(md) && last_modified < lubridate::floor_date(Sys.time(), "day") && lubridate::as_datetime(md$client_modified) > last_modified) {
                        .f <- rdrop2::drop_download(basename(path), overwrite = TRUE)
                        if (.f)
                          fs::file_move(basename(path), path)
                      }
                        
                      UU::file_fn(path)(path, ...)
                      
                    }))

clean_null <- function(files) {
  .rds <- stringr::str_subset(files, "rds$")
  .sizes <- file.size(.rds)
  file.remove(.rds[.sizes == 44])
  files[!files %in% .rds[.sizes == 44]]
}

#' @title Create accessor functions
#'
#' @param path \code{(character)} path to directory where dependencies will be saved
#' @param app_nm  \code{(character)} name of this app
#'
#' @export
#'
#' @include golem_utils_server.R
create_accessors <- function(path = "data", app_nm = "RminorElevated") {
  
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

do_assignment <- function(funs, ns = "RminorElevated") {
  namespace <- rlang::ns_env(ns)
  rlang::env_unlock(namespace)
  purrr::iwalk(funs, ~{
    if (exists(.y, envir = namespace, inherits = FALSE))
      rlang::env_binding_unlock(namespace, .y)
    assign(.y, .x, envir = namespace)
    assignInNamespace(.y, .x, ns, envir = namespace)
    rlang::env_binding_lock(namespace, .y)
  })
}

#' @title Authorize Dropbox
#'
#' @param db_token \code{(character)} path to Dropbox token saved as RDS
#' @export
db_auth <- function(db_token = file.path("inst","auth","db_token.rds")) {
  if (!file.exists(db_token)) {
    token <- rdrop2::drop_auth(key = Sys.getenv("db_key"), secret = Sys.getenv("db_secret"), cache = FALSE)
    if (!dir.exists(dirname(db_token)))
      UU::mkpath(dirname(db_token))
    saveRDS(token, db_token)
  } else {
    rdrop2::drop_auth(rdstoken = db_token)
  }
  
}
