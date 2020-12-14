#' Read Latest Version of CSV Saved With Initials and Date
#'
#' This function reads the latest version of an RDS file saved by save_CSV
#'
#' @param filename String. The base name for the file.
#' @param path String. Where to look the file. If not set, checks for "Tables" or "_tables" in the working directory. Reads from working directory if data folders are not found.
#' @param verbose Logical. Should status messages be displayed?
#' @keywords read, load, CSV, initials, date
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{read_CSV(filename = "datafile")}


read_CSV <- function(filename,
                     path = NULL,
                     verbose = FALSE) {
  # Detect path
  if (is.null(path)) {
    if ("Tables" %in% list.files()) {
      path <- "Tables"
      if (verbose) {
        message("Reading from Tables.")
      }
    } else if ("_tables" %in% list.files()) {
      path <- "_tables"
      if (verbose) {
        message("Reading from _tables.")
      }
    } else {
      path <- ""
      if (verbose) {
        message("Tables folder not detected, reading from working directory.")
      }
    }
  }
  # Clean path
  if (path != "") {
    if (substr(path, nchar(path), nchar(path)) != "/") {
      if (!path %in% list.files()) {
        stop("Path folder not found in working directory.")
      }
      path <- paste0(path, "/")
    } else {
      if (!substr(path, 1, nchar(path) - 1) %in% list.files()) {
        stop("Path folder not found in working directory.")
      }
    }
  }
  # Identify most recent file
  fls <- list.files(path = path)
  fls <- fls[grep("CSV", fls, fixed = T)]
  fls <- fls[grep(filename, fls, fixed = T)]
  fl <-
    fls[which(file.mtime(paste0(path, fls)) == max(file.mtime(paste0(path, fls))))]
  # Read file
  if (verbose) {message("Reading file ", fl, ".")}
  read.csv(paste0(path, fl))
}
