#' Save LaTeX output to a renderable .tex document
#'
#' This function takes LaTeX output from Stargazer, texreg, xtable, etc. and saves it to an output file with the appropriate start and end notes.
#'
#' @param latex Character. The latex output from another function.
#' @param file Character. The (optional) file to save the output to.
#' @param landscape Logical. Should the document be saved as a PDF?
#' @keywords
#' @export
#' @examples
#' data("mtcars", envir = environment())
#' tex <- stargazer::stargazer(mtcars)
#' save_tex(tex)

save_tex <- function(latex,
                     file = NULL,
                     landscape = FALSE) {
  if ("xtable" %in% class(latex)) {
    switch(Sys.info()[["sysname"]],
           Darwin = {
             sink("/dev/null")
           },
           Linux = {
             sink("/dev/null")
           },
           Windows = {
             sink("NUL")
           })
    latex <- print(latex)
    sink()
  }
  start <-
    c(
      "\\documentclass{article}",
      "\\usepackage{dcolumn}",
      if (landscape) {
        "\\usepackage[landscape]{geometry}"
      } else {
        "\\usepackage{geometry}"
      },
      "\\geometry{left=1in,right=1in,top=1in,bottom=1in}",
      "\\begin{document}"
    )
  end <- c("\\end{document}")
  ltx <- c(start, latex, end)
  if (!is.null(file)) {
    writeLines(ltx, con = file(description = file))
    close(file(description = file))
  } else {
    cat(ltx, sep = "\n")
  }
  invisible(ltx)
}

