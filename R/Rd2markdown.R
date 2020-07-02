#' @export
#' @name Rd2markdown
#' @title Rd file to markdown
#' @description This function converts an Rd file into markdown format.
#' @param rdfile Filepath to an .Rd file or an \code{Rd} object to parse.
#' @param outfile Filepath to output file (markdown file).
#' @param append If outfile exists, append to existing content.
#' @param section header tag.
#' @param subsection header tag.
#' @param run.examples logical. should examples be run?
#' @param code.headings logical. topic headings formatted as code? backticks in topic headings.
#' @param topic.section.heading logical. sections within topic formatted as headings?
#' @return Parsed Rd as named list
#' @examples
#' ## give a markdown source file
#' rdfile = "~/git/MyPackage/man/myfun.Rd"
#' ## specify, where markdown shall be stored
#' outfile = "/var/www/html/R_Web_app/md/myfun.md"
#' ## create markdown
#' ## Rd2markdown(rdfile = rdfile, outfile = outfile)
Rd2markdown <- function(rdfile, outfile, append=FALSE, section = "#", subsection = "##", run.examples = FALSE,Rmd=F, code.headings=T, topic.section.heading = T) {
	# VALIDATION
	append <- as.logical(append)
	if (length(append) != 1) stop("Please provide append as single logical value.")
	if (is.character(rdfile)) if ((length(rdfile) != 1)) stop("Please provide rdfile as single character value (file path with extension).")
	outfile <- as.character(outfile)
	if (length(outfile) != 1) stop("Please provide outfile as single character value (file path with extension).")
	if (append) {
		if (!file.exists(outfile)) stop("If append=TRUE, the outfile must exists already.")
	}

	type <- ifelse(inherits(rdfile, "Rd"), "bin", "src")

	# Global definitions for file parsing
	file.ext <- "md"
	section.sep <- "\n\n"
	if (grepl("\\.Rmd$", outfile, perl=T)) Rmd=T


	# Parse rd file
	if (type == "src") {
		rd <- parse_Rd(rdfile)
	} else {
		if (inherits(rdfile, "list"))  {
			rdfile = rdfile[[1]]
		}
		rd <- rdfile
		class(rd) <- "Rd"
	}
	# takes as input an "Rd" object
	results <- parseRd(rd)

	## set subsection heading function
	print.subsection <- function(text, outfile) {
	  if (topic.section.heading) {
	    cat(paste(subsection, text), file=outfile, append=TRUE)
	  } else {
	    cat(paste0("**", text, "**"), file=outfile, append=TRUE)
	  }
	  cat(section.sep, file=outfile, append=TRUE)
	}

	if (all(c("name","title") %in% names(results))) {
		filename <- paste0(results$name, ".", file.ext)
		results$filename <- filename
		results$directory <- dirname(outfile)

		# INIT file if required
		cat("", file=outfile, append=append)

		# HEADING
		if (code.headings) {
		  cat(paste0(section, " `", results$name, "`"), section.sep, file=outfile, append=TRUE, sep="")
		} else {
		  cat(paste(section, results$name), section.sep, file=outfile, append=TRUE, sep="")
		}

		# title as normal text
		cat(results$title, file=outfile, append=TRUE)
		cat(section.sep, file=outfile, append=TRUE)

		if (identical(results$description, results$title)) results$description <- NULL

		for (i in sections.print[!sections.print %in% c("name","title")]) {
			if (i %in% names(results)) {
				if (i == "examples") {
				  print.subsection("Examples", outfile=outfile)
					# cat(paste(subsection, "Examples"), file=outfile, append=TRUE)
					# cat(section.sep, file=outfile, append=TRUE)

				# EXAMPLES
				  if (run.examples) {
				    if (grepl("^## Not run:", results$examples, perl=T)) {
				      cat("```{r, eval=FALSE}\n", file=outfile, append=TRUE)
				    } else {
				      cat("```{r}\n", file=outfile, append=TRUE)
				    }
				  } else if (Rmd) {
					  cat("```{r, eval=FALSE}\n", file=outfile, append=TRUE)
				  } else {
				    cat("```r\n", file=outfile, append=TRUE)
				  }
				  cat(paste0(results$examples), "\n```", "\n\n", file=outfile, append=TRUE, sep="")
				} else if (i %in% c("usage")) {
				  print.subsection(simpleCap(i), outfile)
					# cat(paste(subsection, simpleCap(i)), file=outfile, append=TRUE)
					# cat(section.sep, file=outfile, append=TRUE)
					if (Rmd) {
					  cat("```{r, eval=FALSE}\n",file=outfile, append=TRUE)
					} else {
					  cat("```r\n",file=outfile, append=TRUE)
					}
					cat(paste0(results[[i]]), file=outfile, append=TRUE)
					cat("\n```\n", file=outfile, append=T)
#					cat(section.sep, file=outfile, append=TRUE)
				} else if (i %in% c("arguments")) {
				  print.subsection(simpleCap(i), outfile)
					# cat(paste(subsection, simpleCap(i)), file=outfile, append=TRUE)
					# cat(section.sep, file=outfile, append=TRUE)
					# Prepare table with arguments
					cat("Argument      |Description\n", file=outfile, append=TRUE)
					cat("------------- |----------------\n", file=outfile, append=TRUE)
					cat(paste0("`", names(results[[i]]), "`", "     |     ", results[[i]], collapse="\n"), file=outfile, append=TRUE)

				} else {
				  print.subsection(simpleCap(i), outfile)
				  # cat(paste(subsection, simpleCap(i)), file=outfile, append=TRUE)
				  # cat(section.sep, file=outfile, append=TRUE)
					cat(paste0(results[[i]], collapse="\n"), file=outfile, append=TRUE, sep="\n")
				}
			  cat(section.sep, file=outfile, append=TRUE)
			}
		}
		cat(section.sep, file=outfile, append=TRUE)
	} else {
		warning("name and title are required. Not creating markdown file")
	}

	invisible(results)
}
