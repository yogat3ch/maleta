
#' Write named credentials to .Renviron file
#'
#' @param ... named keys to write
#' @inheritParams usethis::edit_r_environ
#' @param overwrite \code{(lgl)} should an existing key be overwritten. **Default: `FALSE`**
#'
#' @return success message if a value is written
#' @export
#'
#' @examples
creds_to_renviron <- function(..., scope = c("user", "project"), overwrite = FALSE) {
  fp <- switch(UU::match_letters(scope, "user", "project"),
         user = "~/.Renviron",
         project = ".Renviron")
  UU::mkpath(fp)
  l <- ol <- readLines(fp)
  l <- l[nzchar(l)]
  creds <- rlang::dots_list(..., .named = TRUE)

  purrr::iwalk(creds, ~{
    cred_exists <- grepl(paste0("^",.y), l)
    if (!any(cred_exists) || overwrite) {
      if (any(cred_exists))
        l <- l[!cred_exists]
      l <<- c(l, paste0(.y, " = '", .x,"'"))
    }
  })
  write(l, fp)
  readRenviron(fp)
  .written <- trimws(stringr::str_extract(setdiff(l, ol), "^[^\\=]+"))
  if (length(.written))
    cli::cli_alert_success("{cli::col_green(paste0(.written, collapse = \", \"))} successfully written to {.path {fp}}")
}

setup_dropbox <- function(key, secret) {
  creds_to_renviron(DROPBOX_KEY = key, DROPBOX_SECRET = secret)
}
