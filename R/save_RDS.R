#' Save RDS With Initials and Date
#'
#' This function appends the user's initials and the current date when saving to RDS.
#'
#' @param object The object in the R environment to save to a file.
#' @param filename String. The base name for the file.
#' @param path String. Where to save the file. If not set, checks for "Data" or "_data" in the working directory. Saves to working directory if data folders are not found.
#' @param initials String, or empty to retrieve from .Renviron.
#' @param verbose Logical. Should status messages be displayed?
#' @keywords save, RDS, initials, date
#' @export
#' @examples
#' \dontrun{save_initials("DA")}
#' \dontrun{save_RDS(object = ls(), filename = "datafile")}

save_RDS <-
  function(object,
           filename,
           path = NULL,
           initials = Sys.getenv("UNLIKELY_INITIALS"),
           verbose = FALSE) {
    # Detect path
    if (is.null(path)) {
      if ("Data" %in% list.files()) {
        path <- "Data"
        if (verbose) {
          message("Saving to Data.")
        }
      } else if ("_data" %in% list.files()) {
        path <- "_data"
        if (verbose) {
          message("Saving to _data.")
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
    # Check initials
    if (initials == "") {
      stop(
        "Initials not saved in .Renviron. Run save_initials() or include argument initials= when calling save_RDS()."
      )
    }
    # Format date
    date <- format(Sys.Date(), "%m-%d-%Y")
    # Save object to path
    loc <- paste0(path, filename, "-", date, "-", initials, ".RDS")
    saveRDS(object = object, file = loc)
  }
