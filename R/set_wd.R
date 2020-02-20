#' Set Cloud-Aware Working Directory
#'
#' This function allows you to set a working directory within a given cloud storage provider in your home directory.
#' @param project The name of the project directory. Must be set.
#' @param cloud The name of the cloud storage provider. If not set, the project is assumed to reside in your user folder.
#' @keywords working directory, cloud storage, box, dropbox, odrive
#' @export
#' @examples
#' set_wd()

set_wd <- function(project, cloud = NULL) {
  path <-
    switch(Sys.info()[['sysname']], Windows = "C:/Users/", Darwin = "/Users/")
  path <- paste0(path, Sys.info()[['user']])
  if ("odrive" %in% list.files(path)) {
    path <- paste0(path, "/odrive/")
  }
  if (exists("cloud")) {
    path <- paste0(path, cloud)
  }
  path <- paste0(path, project)
  setwd(path)
}
