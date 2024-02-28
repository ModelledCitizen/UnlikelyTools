#' Read Latest Version of RDS Saved With Initials and Date
#'
#' This function reads the latest version of an RDS file saved by save_RDS.
#'
#' @param filename String. The base name for the file.
#' @param path String. Where to look the file. If not set, checks for "Data" or "_data" in the working directory. Reads from working directory if data folders are not found.
#' @param verbose Logical. Should status messages be displayed?
#' @keywords read, load, RDS, initials, date
#' @export
#' @examples
#' \dontrun{read_RDS(filename = "datafile")}


read_RDS <- function(filename,
                     path = NULL,
                     verbose = FALSE) {
  # Detect path
  if (is.null(path)) {
    if ("Data" %in% list.files()) {
      path <- "Data"
      if (verbose) {
        message("Reading from Data.")
      }
    } else if ("_data" %in% list.files()) {
      path <- "_data"
      if (verbose) {
        message("Reading from _data.")
      }
    } else {
      path <- "."
      if (verbose) {
        message("Data folder not detected, reading from working directory.")
      }
    }
  }
  # Clean path
  if (path != "." & !path %in% list.files()) {
    stop("Path folder not found in working directory.")
  }
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }
  fls <- list.files(path = path)
  fls <- fls[grep(".*\\.RDS", list.files(), ignore.case = T)]
  fls <- fls[grep(filename, fls, fixed = T)]
  fl <-
    fls[which(file.mtime(paste0(path, fls)) == max(file.mtime(paste0(path, fls))))]
  # Read file
  if (verbose) {message("Reading file ", fl, ".")}
  readRDS(paste0(path, fl))
}
