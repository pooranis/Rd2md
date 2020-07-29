#' @export
#' @name parseRd
#' @title Parse an Rd object
#' @description This function will parse an Rd object returning a list with each section. The
#' contents of each element of the list will be converted to markdown.
#' @param rd An \code{Rd} object.
#' @return a named list with the parts of the Rd object that will be used for creating
#' a markdown file
#' @examples
#' ## rd source (from parse_Rd function of tools package)
#' rdfile = '~/git/MyPackage/man/myfun.Rd'
#' ## rd = tools::parse_Rd(rdfile)
#' ## parseRd(rd)
parseRd <- function(rd) {
    verbose <- getOption("verbose")
    # VALIDATION
    if (!("Rd" %in% class(rd)))
        stop("Please provide Rd object to parse.")
    tags <- RdTags(rd)
    results <- list()
    if (!("\\name" %in% tags)) {
        return(results)
    }
    for (i in sections) {
        if (i %in% tags) {
            # Handle \argument section separately
            if (i == "\\arguments") {
                args <- rd[[which(tags == "\\arguments")]]
                args.tags <- RdTags(args)
                args <- args[which(args.tags == "\\item")]
                params <- character()
                for (i in seq_along(args)) {
                  param.name <- as.character(args[[i]][[1]])
                  param.desc <- paste(sapply(args[[i]][[2]], FUN = function(x) {
                    parseTag(x)
                  }), collapse = " ")
                  params <- c(params, param.desc)
                  names(params)[length(params)] <- param.name
                }
                results$arguments <- params

                ## Usage
            } else if (i %in% c("\\usage")) {
                results[["usage"]] <- paste0(trim(paste(sapply(rd[[which(tags ==
                  "\\usage")]], FUN = function(x) {
                  if (x[1] == "\n")
                    x[1] <- ""  # exception handling
                  parseTag(x, stripNewline = FALSE, stripWhite = FALSE, stripTab = FALSE)
                }), collapse = "")))

                ## Doctype package first
            } else if (i == "\\docType") {
                dt <- rd[[which(tags == "\\docType")]]
                if (dt == "package") {
                  name <- rd[[which(tags == "\\name")]]
                  if (!grepl("-package$", rd[[which(tags == "\\name")]], perl = T)) {
                    rd[[which(tags == "\\name")]] <- paste0(trimws(name), "-package")
                  }
                }

                ## examples
            } else if (i %in% c("\\examples", "\\example")) {
                key <- substr(i, 2, nchar(i))
                results[[key]] <- trim(paste(sapply(rd[[which(tags == i)[1]]], FUN = function(x) {
                  parseTag(x, stripNewline = FALSE)
                }), collapse = ""))

            } else if (i %in% c("\\references", "\\details")) {
                key <- substr(i, 2, nchar(i))
                results[[key]] <- trim(paste(sapply(rd[[which(tags == i)[1]]], FUN = function(x) {
                  parseTag(x, stripNewline = FALSE, stripWhite = F)
                }), collapse = ""))

                ## process aliases separately, so we can find package name rdfiles from the .rdb
                ## database don't have doctype
            } else if (i %in% c("\\alias")) {
                key <- substr(i, 2, nchar(i))
                j <- which(tags == i)
                my.alias <- trim(sapply(rd[which(tags == i)], FUN = function(x) {
                  parseTag(x)
                }))

                ## put -package in package doc name so it appears first
                if (!grepl("-package$", rd[[which(tags == "\\name")]], perl = T)) {
                  packname <- grep("-package", my.alias)
                  if (length(packname) > 0) {
                    rd[[which(tags == "\\name")]] <- my.alias[packname]
                    results[["name"]] <- my.alias[packname]
                  }
                }
                results[[key]] <- my.alias

                ## the rest of the tags
            } else if (i %in% tags) {
                key <- substr(i, 2, nchar(i))
                results[[key]] <- trim(paste(sapply(rd[[which(tags == i)[1]]], FUN = function(x) {
                  parseTag(x, stripNewline = FALSE)
                }), collapse = " "))
            }
        }
    }
    invisible(results)
}

#' parse unknown rdfile
#'
#' @description \code{\link{ReferenceManual}} looks for actual .rd files, or in the case of already
#' compiled package, will read from .rdb database.  This function checks whether rdfile is
#' a filename or a string from the .rdb database, and parses accordingly.
#'
#' @param rdfile either rd filename or entry in .rdb database (from \code{\link{fetchRdDB}})
#'
#' @return a named list with the parts of the Rd object that will be used for creating
#' a markdown file
#'
#' @keywords internal
#'
parse_unknown_rd <- function(rdfile) {

    verbose <- getOption("verbose")
    if (is.character(rdfile))
        if ((length(rdfile) != 1))
            stop("Please provide rdfile as single character value (file path with extension).")
    type <- ifelse(inherits(rdfile, "character"), "src", "bin")
    # Parse rd file
    if (type == "src") {
        rd <- parse_Rd(rdfile)
    } else {
        if (inherits(rdfile, "list")) {
            rdfile = rdfile[[1]]
        }
        rd <- rdfile
        class(rd) <- "Rd"
    }
    # if (verbose) print(rd) takes as input an 'Rd' object
    results <- parseRd(rd)
    class(results) <- append(class(results), "Rd.list")
    return(results)
}
