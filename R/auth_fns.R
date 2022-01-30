#' @title Authorize Dropbox
#'
#' @param db_token \code{(character)} path to Dropbox token saved as RDS
#' @export

db_auth <- function(db_token = file.path("inst","vault","db_token.rds")) {
  if (!file.exists(db_token) && interactive()) {
    token <- rdrop2::drop_auth(key = Sys.getenv("db_key"), secret = Sys.getenv("db_secret"), cache = FALSE)
    UU::mkpath(dirname(db_token))
    saveRDS(token, db_token)
  } else if (file.exists(db_token)) {
    rdrop2::drop_auth(rdstoken = db_token)
  } else
    UU::gbort("Dropbox authentication credentials missing from {.path {db_token}} and session is non-interactive. Please see ?{cli::col_cyan('rdrop2::drop_auth')}")
}
