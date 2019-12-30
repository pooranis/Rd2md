#' @name parseTag
#' @title Tag from Rd Format to Markdown
#' @description This function will convert an Rd element to markdown format.
#' Note that links are only supported within a markdown document, referenced with #.
#' Use \code{href} to link to external sources.
#' @param x element from an \code{Rd} class.
#' @param pre String to prepend to the parsed tag.
#' @param post String to append to the parsed tag.
#' @param stripNewline Logical indicating whether to strip new line characters.
#' @param stripWhite Logical indicating whether to strip white space.
#' @param stripTab Logical indicating whether to strip tab characters.
#' @param emptyNA Logical indicating whether empty strings should be set to NA.
parseTag <- function(x,
           pre = character(),
           post = character(),
           stripNewline = TRUE,
           stripWhite = TRUE,
           stripTab = TRUE,
           emptyNA = FALSE) {

    verbose <- getOption("verbose")
    rdtag <- attr(x, "Rd_tag")

    if (is.null(rdtag) || rdtag %in% c("TEXT", "RCODE", "VERB")) {
        x <- paste0(pre, as.character(x), post)
        if (stripTab) {
            x <- gsub("\t", "", x)
        }
        if (stripNewline) {
            x <- gsub("\n", "", x)
        }
        if (stripWhite) {
            x <- stripWhite(x)
        }
        if (emptyNA) {
            if (x == "")
                x <- NA
        }

# dontrun in examples
    } else if (rdtag == "\\dontrun") {
        # x <- sapply(x, parseTag)
        x <- trim(paste(sapply(x, FUN = function(x) {
            parseTag(x, stripNewline = FALSE)
        }), collapse = ""))
        x <- paste("## Not run:\n", paste(x, collapse = "\n"), "\n## End(Not run)", collapse = " ")

# verbatim text
    } else if (rdtag == "\\code") {
        pre <- c("`", pre)
        post <- c(post, "`")
        x <- parseTag(x[[1]], pre, post)

# italicized text
    } else if (rdtag == "\\emph" || rdtag == "\\var") {
        pre <- c("*", pre)
        post <- c(post, "*")
        x <- parseTag(x[[1]], pre, post)

# bold text
    } else if (rdtag == "\\strong") {
        pre <- c("**", pre)
        post <- c(post, "**")
        x <- parseTag(x[[1]], pre, post)

# mathjax/latex inline equation
    } else if (rdtag == "\\eqn") {
        pre <- c("$", pre)
        post <- c(post, "$")
        x <- paste0(parseTag(x[[1]], pre, post, stripWhite = TRUE,
                             stripNewline = TRUE, stripTab = TRUE),collapse = "")
# mathjax/latex equation environment
    } else if (rdtag == "\\deqn") {
        message(paste0("Equation used, be sure to include MathJax."))
        pre <- c("\n$$", pre)
        post <- c(post, "$$\n")
        x <- paste0("\n$$", paste0(parseTag(x[[1]]), collapse = ""), "$$\n")

# links to other R documentation
    } else if (rdtag == "\\link") {
        if (attr(x[[1]], "Rd_tag") != "TEXT") {
            warning("\\link is not the inner most tag. All other nested tags will be ignored.")
        }
        # only internal markdown links, referenced with #
        x <- paste0("[", pre, parseTag(x[[1]], stripNewline = stripNewline), post, "](#", gsub(" ",
            "_", gsub("_", "", tolower(parseTag(x[[1]], stripNewline = stripNewline)))), ")")

# web links and urls
    } else if (rdtag == "\\url") {
        x <- paste0("[", pre, as.character(x), post, "](", as.character(x), ")")
    } else if (rdtag == "\\href") {
        x <- paste0("[", as.character(x[[2]]), "](", as.character(x[[1]]), ")")

# items in lists
    } else if (rdtag == "\\item") {
        enum <- get0("enum", ifnotfound = F, inherits = F)
        l <- ifelse(enum, "1.", "*")
        if (length(x) > 0) {
            x1 <- parseTag(x[[1]], pre = paste0("\n", l, "  `"), post = "` ", stripNewline = FALSE)
            x2 <- parseTag(x[[2]], stripNewline = FALSE)
            x <- paste(x1, x2)
        } else {
            x <- paste0("\n\n", l, " ")
        }
    } else if (rdtag == "\\itemize" || rdtag == "\\describe") {
        enum = F
        x <- paste(sapply(x, parseTag), collapse = " ")
    } else if (rdtag == "\\enumerate") {
        warning("enumerate not currently supported. Items will be bulleted instead.")
        enum = T
        x <- paste(sapply(x, parseTag), collapse = " ")

## tables
    } else if (rdtag == "\\tabular") {
        tab <- x[[2]]
        tabheader_n <- 1:grep("\\cr", sapply(tab, attr, "Rd_tag"))[1]
        tabheader <- na.exclude(sapply(tab[tabheader_n], parseTag, emptyNA = T))
        tabheader <- c(tabheader, sapply(tabheader, function(t) ifelse(t %in% c(" | ", "\n"), t, paste0(rep("-",
            nchar(t) + 5), collapse = "")), USE.NAMES = F))
        x <- na.exclude(c(tabheader, sapply(tab[-tabheader_n], parseTag, emptyNA = T)))
        x <- paste(x, collapse = "")
# table column separator
    } else if (rdtag == "\\tab") {
        x <- " | "
# table row separator
    } else if (rdtag == "\\cr") {
        x <- "\n"
    }

    return(x)
}
