accessor_create <- function(.x) rlang::new_function(args =
                      rlang::pairlist2(
                        path = rlang::expr(!!.x),
                        dep_update = dep_update_fn(fn = dep_update_dropbox),
                        ... = ,
                      ),
                    body = base::quote({
                      dep_update(path)
                      UU::file_fn(path)(path, ...)
                    }))

do_assignment <- function(funs, ns = pkgload::pkg_name()) {
  if (UU::is_legit(try(ns, silent = TRUE))) {
    namespace <- rlang::ns_env(ns)
    rlang::env_unlock(namespace)
    purrr::iwalk(funs, ~{
      if (exists(.y, envir = namespace, inherits = FALSE))
        rlang::env_binding_unlock(namespace, .y)
      assign(.y, .x, envir = namespace)
      assignInNamespace(.y, .x, ns, envir = namespace)
      rlang::env_binding_lock(namespace, .y)
    })
  } else
    funs
}

#' Create accessor functions in the namespace
#'
#' @param dep_dir \code{(chr)} of the directory in which dependencies to be accessed with functions is stored. _Note:_ Either `dep_dir` or `deps` must be supplied.
#' @param deps \code{(chr)} of full filepaths for all dependencies to be accessed with functions.
#' @param dep_update \code{(function)} dependency update from remote location function
#' @param update_all \code{(lgl)} update all dependencies immediately.
#' @return The accessor functions will be available in the package namespace if a `golem` package, otherwise accessor functions will be returned in a list
#' @export


create_accessors <- function(dep_dir = "data", deps = NULL, dep_update = dep_update_fn(fn = dep_update_dropbox), update_all = TRUE) {
  if (is.null(deps))
    deps <- clean_null(UU::list.files2(dep_dir)) |>
      stringr::str_subset("\\.png^", negate = TRUE)
  deps <- fs::path_abs(deps)
  UU::mkpath(dep_dir)
  if (update_all)
    purrr::walk(deps, dep_update)
  accessor_funs <- purrr::map(rlang::set_names(deps, fs::path_ext_remove(basename(deps))), accessor_create)
  do_assignment(accessor_funs)
}

