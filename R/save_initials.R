#' Save Initials to .Renviron
#'
#' This function saves the user's initials to the .Renviron.
#'
#' @param initials String, containing users initials.
#' @param overwrite Logical. If initials are already in .Renvrion, should they be replaced?
#' @keywords save, initials, .Renviron
#' @export
#' @examples
#' \dontrun{save_initials("DA")}

save_initials <- function(initials, overwrite = FALSE) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")
  if (file.exists(renv)) {
    file.copy(renv, file.path(home, ".Renviron_backup"))
  }
  if (!file.exists(renv)) {
    file.create(renv)
  }
  else {
    if (isTRUE(overwrite)) {
      message(
        "Your original .Renviron will be backed up and stored in your R HOME directory if needed."
      )
      oldenv <- readLines(renv)
      newenv <- oldenv[-grep("UNLIKELY_INITIALS", oldenv)]
      writeLines(newenv, renv)
    }
    else {
      tv <- readLines(renv)
      if (any(grepl("UNLIKELY_INITIALS", tv))) {
        stop(
          "Initials are already in .Renviron. You can overwrite them with the argument overwrite=TRUE",
          call. = FALSE
        )
      }
    }
  }
  intlscct <- paste0("UNLIKELY_INITIALS = '", initials, "'")
  write(intlscct, renv, sep = "\n", append = TRUE)
  message(
    'Your initials have been stored in .Renviron.  \nTo use now, restart R or run `readRenviron("~/.Renviron")`'
  )
}
