% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dep_update_fns.R
\name{dep_update_dropbox}
\alias{dep_update_dropbox}
\title{Update a dependency saved in Dropbox (if it's older)}
\usage{
dep_update_dropbox(
  local_filepaths = readRDS(file.path("data", paste0("deps_", pkgload::pkg_name(),
    ".rds"))),
  remote_dir = NULL
)
}
\arguments{
\item{local_filepaths}{\code{(chr)} of the filepath(s)}

\item{remote_dir}{\code{(chr)} directory on the remote server where \code{local_filepath} is saved. The location will be transformed as follows: \code{\link[base]{file.path}(dest_remote_dir, \link[base]{basename}(dep_filepath)}}
}
\value{
dep_filepath contents
}
\description{
Update a dependency saved in Dropbox (if it's older)
}
