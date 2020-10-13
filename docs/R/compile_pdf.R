# Compile an Rnw to PDF using the same commands as R Studio
# but offer the options to control the quiet & clean settings

# from: https://community.rstudio.com/t/what-commands-are-run-when-the-compile-pdf-button-is-clicked/6291

compile_pdf <- function(input, output=NULL, quiet=FALSE, clean=FALSE, ...) {
	knitr::knit(input, output, quiet=quiet, ...)
	tools::texi2pdf(output, clean=clean )
	cat("Created", output)
	system(paste('open', output))  # what is the equivalent for Windows?
}
