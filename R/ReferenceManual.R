#' @export
#' @name ReferenceManual
#' @title Create Reference Manual Markdown
#' @description This is a wrapper to combine the Rd files of a package source or binary
#' into a reference manual in markdown format.
#' @param pkg Full path to package directory. Default value is the working directory.
#' Alternatively, a package name can be passed. If this is the case, \code{\link[base]{find.package}} is applied.
#' @param outdir Output directory where the reference manual markdown shall be written to.
#' @param man_file Character. name of output file.  Default Reference_Manual_pkgname.md
#' @param front.matter String with yaml-style heading of markdown file.
#' @param toc.matter String providing the table of contents. This is not auto-generated.
#' The default value is a HTML comment, used by gitbook plugin
#' \href{https://www.npmjs.com/package/gitbook-plugin-toc}{toc}.
#' @param date.format Date format that shall be written to the beginning of the reference manual.
#' If \code{NULL}, no date is written.
#' Otherwise, provide a valid format (e.g. \code{\%Y-\%m-\%d}), see Details in \link[base]{strptime}.
#' @param verbose If \code{TRUE} all messages and process steps will be printed
#' @param title.level Integer.  Title header level. If including exported sections, section.level will be one
#' more, and subsection level 2 more.  Otherwise, section.level will be the same and subsection.level only one
#' more.
#' @param run.examples Logical. Whether or not to run examples.
#' @param skip.topics Character. Functions, methods, objects, etc to skip.  Should be prefix of
#' .rd file.
#' @param topic.groups Named list of vectors of topics in each topic group.  The group names should be
#'  the names of the list.
#' @param sepxported Logical. Separate exported and internal objects. Sets \code{topic.groups} to be
#' "Exported" and "Internal"
#'
#' @references Murdoch, D. (2010). \href{http://developer.r-project.org/parseRd.pdf}{Parsing Rd files}
#' @seealso Package \href{https://github.com/jbryer/Rd2markdown}{Rd2markdown} by jbryer
#' @examples
#' ## give source directory of your package
#' pkg_dir = "~/git/MyPackage"
#'
#' ## specify, where reference manual shall be stored
#' out_dir = "/var/www/html/R_Web_app/md/"
#'
#' ## create reference manual
#' ## ReferenceManual(pkg = pkg_dir, outdir = out_dir)
#'
#' @importFrom utils capture.output
#' @importFrom desc desc_print
#'
#'
ReferenceManual <- function(pkg = getwd(), outdir = getwd()
                            , man_file = NULL
                            , front.matter = ""
                            , toc.matter = "<!-- toc -->"
                            , date.format = "%B %d, %Y"
                            , verbose = FALSE
                            , title.level = 1
                            , run.examples = FALSE
                            , skip.topics = NULL
                            , topic.groups = NULL
                            , sepexported = FALSE) {
  # VALIDATION
  pkg <- as.character(pkg)
  if (length(pkg) != 1) stop("Please provide only one package at a time.")
  outdir <- as.character(outdir)
  if (length(outdir) != 1) stop("Please provide only one outdir at a time.")
  if (!dir.exists(outdir)) stop("Output directory path does not exist.")
  opts <- options(verbose = verbose)
  on.exit(options(opts))

  # locate package
  pkg_path <- path.expand(pkg)
  pkg_name <- basename(pkg_path)
  type <- "src"
  mandir <- "man"
  if (!dir.exists(pkg_path)) {
    pkg_path <- find.package(pkg_name)
    type <- "bin"
    mandir <- "help"
  }

  if (length(mandir ) != 1) stop("Please provide only one manuals directory.")
  if (!dir.exists(file.path(pkg_path, mandir))) stop("Package manuals path does not exist. Check working directory or given pkg and manuals path!")

  # PARAMS
  section.sep <- "\n\n"
  title.header = paste0(rep("#", title.level), collapse = "")
  if (!sepexported && is.null(topic.groups)) {
    title.level = title.level-1
  }
  section.header = paste0(rep("#", title.level+1), collapse = "")
  subsection.header = paste0(rep("#", title.level+2), collapse = "")


  # Output file for reference manual
  if (is.null(man_file)) {
    man_file <- file.path(outdir, paste0("Reference_Manual_", pkg_name, ".md"))
  } else {
    man_file <- file.path(outdir,man_file)
  }

  # INIT REFERENCE MANUAL .md
  cat(front.matter, file=man_file, append=FALSE) # yaml
  cat(section.sep, file=man_file, append=TRUE)

  # Table of contents
  cat(toc.matter, file=man_file, append=TRUE)
  cat(section.sep, file=man_file, append=TRUE)

  # Date
  if (!is.null(date.format)) {
    cat(format(Sys.Date(), date.format), file=man_file, append=TRUE)
    cat(section.sep, file=man_file, append=TRUE)
  }



  # DESCRIPTION file
  cat(title.header, " DESCRIPTION", file=man_file, append=TRUE)
  cat(section.sep, file=man_file, append=TRUE)
  cat("```\n", file=man_file, append=TRUE)
  DESCRIPTION = capture.output(desc_print((file.path(pkg_path, "DESCRIPTION"))))
  cat(paste0(DESCRIPTION, collapse="\n"), file=man_file, append=TRUE)
  cat("\n```\n", file=man_file, append=TRUE)
  cat(section.sep, file=man_file, append=TRUE)

  # RD files
  results <- list()

  if (type == "src") {
    rd_files <- list.files(file.path(pkg_path, mandir), full.names = TRUE)
    topics <- gsub(".rd","",gsub(".Rd","",basename(rd_files)))
    v <- which(!(topics %in% skip.topics))
    topics <- topics[v]
    rd_files <- rd_files[v]
  } else {
    rd_files <- fetchRdDB(file.path(pkg_path, mandir, pkg_name))
    topics <- names(rd_files)
  }

  ## if package Rd exists put it first
  packagerd <- which(topics == paste0(pkg_name, "-package"))
  if (length(packagerd) == 1) {
    packagerdresults <- Rd2markdown(rdfile=rd_files[packagerd], outfile=man_file, append=TRUE, section = title.header, subsection = subsection.header, run.examples=run.examples)
    topics <- topics[-packagerd]
    rd_files <- rd_files[-packagerd]
  }
  ## Make topic group for exported and internal
  if (sepexported) {
    topic.groups = list()
    ns <- scan(file.path(pkg_path,"NAMESPACE"), sep="\n", what = character())
    topic.groups$Exported <- unique(stringr::str_match(ns, "export.*\\((.*?)[\\,\\)]")[,2])
    topic.groups$Internal <- setdiff(topics, topic.groups$Exported)
  }
  ## Write out topic groups separately
  if (!is.null(topic.groups)) {
    topicnums <- 1:length(topics)
    for (tgroup in names(topic.groups)) {
      cat(title.header, " ", tgroup, file=man_file, append=TRUE)
      cat(section.sep, file=man_file, append=TRUE)
      tgt <- which(topics %in% topic.groups[[tgroup]])

      for(i in tgt) {#i=1
        if(verbose) message(paste0("Writing topic: ", topics[i], "\n"))
        results[[i]] <- Rd2markdown(rdfile=rd_files[i], outfile=man_file, append=TRUE, section = section.header, subsection = subsection.header, run.examples=run.examples)
      }
      topicnums <- setdiff(topicnums, tgt)
    }
    ## Write out topics not found in any topic groups as Other
    if (length(topicnums) > 0) {
      cat(section.sep, file=man_file, append=TRUE)
      cat(title.header, " Other\n", file=man_file, append=TRUE)
      for(i in 1:length(topicnums)) {#i=1
        if(verbose) message(paste0("Writing topic: ", topics[i], "\n"))
        results[[i]] <- Rd2markdown(rdfile=rd_files[i], outfile=man_file, append=TRUE, section = section.header, subsection = subsection.header, run.examples=run.examples)
      }
    }
  ## No groups
  } else {

    # Parse rd files and add to ReferenceManual
    for(i in 1:length(topics)) {
      if(verbose) message(paste0("Writing topic: ", topics[i], "\n"))
      results[[i]] <- Rd2markdown(rdfile=rd_files[i], outfile=man_file, append=TRUE, section = section.header, subsection = subsection.header, run.examples=run.examples)
    }

  }

  invisible(man_file)

}
