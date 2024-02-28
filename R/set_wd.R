#' Set Cloud-Aware Working Directory
#'
#' This function allows you to set a working directory within a given cloud storage folder in your home directory.
#'
#' @param project The name of the project directory. Must be set.
#' @param cloud The name of the cloud storage provider. If not set, the project is assumed to reside in your user folder.
#' @param verbose Logical. Should status messages be displayed?
#' @keywords working directory, cloud storage, box, dropbox, odrive
#' @export
#' @examples
#' \dontrun{set_wd("Platforms", "Box")}
#' \dontrun{set_wd("Misogyny", "Dropbox")}
#' \dontrun{set_wd("project-harvest")}

set_wd <- function(project, cloud = NULL, verbose = FALSE) {
  switch(Sys.info()[["sysname"]],
         Windows = {
           path <- "C:/Users/"
           if (verbose) {
             message("Starting with Windows root...")
           }
         },
         Darwin = {
           path <- "/Users/"
           if (verbose) {
             message("Starting with macOS root...")
           }
         },
         Linux = {
           path <- "/home/"
           if (verbose) {
             message("Starting with Linux root...")
           }
         })
  path <- paste0(path, Sys.info()[["user"]], "/")
  if (!is.null(cloud)) {
    if (cloud == "iCloud") {
      if (verbose) { message("Using documents folder for iCloud sync...") }
      path <- paste0(path, "Documents/")
    }
    if (cloud != "iCloud") {
      if ("odrive" %in% list.files(path)) {
        if (verbose) { message("Detected odrive folder; adding to path...") }
        path <- paste0(path, "odrive/")
      }
      if (verbose) { message("Adding cloud provider to path...") }
      path <- paste0(path, cloud)
    }
  }
  path <- paste0(path, "/", project)
  setwd(path)
}
