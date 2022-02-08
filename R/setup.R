
setup_dropbox <- function(key, secret) {
  UU::creds_to_renviron(DROPBOX_KEY = key, DROPBOX_SECRET = secret)
}
