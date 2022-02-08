
#' Write named credentials to .Renviron file
#' @description Writes key pairs to .Renviron and adds .Renviron to _.gitignore_ if not already there.
#' @param ... named keys to write
#' @inheritParams usethis::edit_r_environ
#' @param overwrite \code{(lgl)} should an existing key be overwritten. **Default: `FALSE`**
#'
#' @return success message if a value is written
#' @export
#'

creds_to_renviron <- function(..., scope = c("user", "project"), overwrite = FALSE) {
  .scope <- UU::match_letters(scope, "user", "project")
  fp <- switch(.scope,
         user = Sys.getenv("R_ENVIRON_USER", "~/.Renviron"),
         project = ".Renviron")
  UU::mkpath(fp, mkfile = TRUE)
  l <- readLines(fp)
  l <- l[nzchar(l)]
  creds <- rlang::dots_list(..., .named = TRUE)
  creds_to_write <- need_write(creds, l, overwrite)

  if (length(creds_to_write)) {
    write(paste0(names(creds_to_write), " = '", creds_to_write,"'"), fp, append = TRUE)
    readRenviron(fp)
    cli::cli_alert_success("{cli::col_green(paste0(names(creds_to_write), collapse = \", \"))} successfully written to {.path {fp}}")
  }
  if (.scope == "project") {
    UU::mkpath(".gitignore", mkfile = TRUE)
    to_ignore <- need_write(".Renviron", readLines(".gitignore"))
    write(to_ignore, file = ".gitignore", append = TRUE)
    if (UU::is_legit(to_ignore))
      cli::cli_alert_info("{.path {'.Renviron'}} added to {.val {'.gitignore'}}")
  }

}

setup_dropbox <- function(key, secret) {
  creds_to_renviron(DROPBOX_KEY = key, DROPBOX_SECRET = secret)
}

need_write <- function(creds, file_lines, overwrite = FALSE) {
  if (is.null(names(creds)))
    creds <- rlang::set_names(creds)
  creds[purrr::imap_lgl(creds, ~{
    cred_exists <- grepl(paste0("^",.y), file_lines, fixed = TRUE)
    if (!any(cred_exists) || overwrite)
      TRUE
    else
      FALSE
  })]
}
