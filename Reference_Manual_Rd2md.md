

<!-- toc -->

July 29, 2020

#  DESCRIPTION

```
Package: Rd2md
Title: Markdown Reference Manuals
Version: 0.0.4
Authors@R (parsed):
    * Julian Busch <jb@quants.ch> [aut, cre]
Maintainer: Poorani Subramanian <email@example.com>
Description: The native R functionalities only allow PDF exports of
    reference manuals. This shall be extended by converting the package
    documentation files into markdown files and combining them into a
    markdown version of the package reference manual.
License: GPL
URL: https://github.com/pooranis/Rd2md
Depends:
    R (>= 3.2.3)
Imports:
    knitr,
    tools,
    desc
Suggests:
    testthat,
    rmarkdown,
    devtools
VignetteBuilder:
    knitr
Encoding: UTF-8
LazyData: true
Roxygen: list(old_usage = TRUE)
RoxygenNote: 7.1.1
```


# `Rd2md-package`

Reference Manual in Markdown

## Description

Functions to parse and convert Rd files to markdown format reference manual.


## Seealso

`knitr`


## Author

Julian Busch <jb@quants.ch>




# `fetchRdDB`

Make first letter capital

## Description

Capitalize the first letter of every new word. Very simplistic approach.


## Usage

```r
fetchRdDB(filebase, key = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`filebase`     |     The file path to the database ( `.rdb` file), with no extension
`key`     |     Keys to fetch

## Value

character vector with capitalized first letters




# `parse_unknown_rd`

parse unknown rdfile

## Description

[`ReferenceManual`](#referencemanual) looks for actual .rd files, or in the case of already
 compiled package, will read from .rdb database.  This function checks whether rdfile is
 a filename or a string from the .rdb database, and parses accordingly.


## Usage

```r
parse_unknown_rd(rdfile)
```


## Arguments

Argument      |Description
------------- |----------------
`rdfile`     |     either rd filename or entry in .rdb database (from [`fetchRdDB`](#fetchrddb) )

## Value

a named list with the parts of the Rd object that will be used for creating
 a markdown file




# `parseRd`

Parse an Rd object

## Description

This function will parse an Rd object returning a list with each section. The
 contents of each element of the list will be converted to markdown.


## Usage

```r
parseRd(rd)
```


## Arguments

Argument      |Description
------------- |----------------
`rd`     |     An `Rd` object.

## Value

a named list with the parts of the Rd object that will be used for creating
 a markdown file


## Examples

```r
## rd source (from parse_Rd function of tools package)
rdfile = '~/git/MyPackage/man/myfun.Rd'
## rd = tools::parse_Rd(rdfile)
## parseRd(rd)
```





# `parseTag`

Tag from Rd Format to Markdown

## Description

This function will convert an Rd element to markdown format.
 Note that links are only supported within a markdown document, referenced with #.
 Use `href` to link to external sources.


## Usage

```r
parseTag(x, pre = character(), post = character(), stripNewline = TRUE,
  stripWhite = TRUE, stripTab = TRUE, emptyNA = FALSE)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     element from an `Rd` class.
`pre`     |     String to prepend to the parsed tag.
`post`     |     String to append to the parsed tag.
`stripNewline`     |     Logical indicating whether to strip new line characters.
`stripWhite`     |     Logical indicating whether to strip white space.
`stripTab`     |     Logical indicating whether to strip tab characters.
`emptyNA`     |     Logical indicating whether empty strings should be set to NA.



# `Rd2markdown`

Rd file to markdown

## Description

This function converts an Rd file into markdown format.


## Usage

```r
Rd2markdown(rdfile, outfile, append = FALSE, section = "#",
  subsection = "##", run.examples = FALSE, Rmd = F, code.headings = T,
  topic.section.heading = T)
```


## Arguments

Argument      |Description
------------- |----------------
`rdfile`     |     Filepath to an .Rd file or an `Rd` object to parse or an `Rd.list` object output from [`parse_unknown_rd`](#parseunknownrd)
`outfile`     |     Filepath to output file (markdown file).
`append`     |     If outfile exists, append to existing content.
`section`     |     header tag.
`subsection`     |     header tag.
`run.examples`     |     logical. should examples be run?
`code.headings`     |     logical. topic headings formatted as code? backticks in topic headings.
`topic.section.heading`     |     logical. sections within topic formatted as headings?

## Value

Parsed Rd as named list


## Examples

```r
## give a markdown source file
rdfile = "~/git/MyPackage/man/myfun.Rd"
## specify, where markdown shall be stored
outfile = "/var/www/html/R_Web_app/md/myfun.md"
## create markdown
## Rd2markdown(rdfile = rdfile, outfile = outfile)
```





# `RdTags`

Extract Rd tags

## Description

Extract Rd tags from Rd object


## Usage

```r
RdTags(Rd)
```


## Arguments

Argument      |Description
------------- |----------------
`Rd`     |     Object of class `Rd`

## Value

character vector with Rd tags




# `ReferenceManual`

Create Reference Manual Markdown

## Description

This is a wrapper to combine the Rd files of a package source or binary
 into a reference manual in markdown format.


## Usage

```r
ReferenceManual(pkg = getwd(), outdir = getwd(), man_file = NULL,
  front.matter = "", toc.matter = "<!-- toc -->",
  date.format = "%B %d, %Y", verbose = FALSE, title.level = 1,
  skip.topics = NULL, topic.groups = NULL, sepexported = FALSE, ...)
```


## Arguments

Argument      |Description
------------- |----------------
`pkg`     |     Full path to package directory. Default value is the working directory. Alternatively, a package name can be passed. If this is the case, [`find.package`](#find.package) is applied.
`outdir`     |     Output directory where the reference manual markdown shall be written to.
`man_file`     |     Character. name of output file.  Default Reference_Manual_pkgname.md
`front.matter`     |     String with yaml-style heading of markdown file.
`toc.matter`     |     String providing the table of contents. This is not auto-generated. The default value is a HTML comment, used by gitbook plugin [toc](https://www.npmjs.com/package/gitbook-plugin-toc) .
`date.format`     |     Date format that shall be written to the beginning of the reference manual. If `NULL` , no date is written. Otherwise, provide a valid format (e.g. `%Y-%m-%d` ), see Details in [strptime](#strptime) .
`verbose`     |     If `TRUE` all messages and process steps will be printed
`title.level`     |     Integer.  Title header level. If including exported sections, section.level will be one more, and subsection level 2 more.  Otherwise, section.level will be the same and subsection.level only one more.
`skip.topics`     |     Character. Functions, methods, objects, etc to skip.  Should be prefix of .rd file.
`topic.groups`     |     Named list of vectors of topics in each topic group.  The group names should be the names of the list.
`...`     |     parameters to pass to [Rd2markdown](#rd2markdown)
`sepxported`     |     Logical. Separate exported and internal objects. Sets `topic.groups` to be "Exported" and "Internal"

## Seealso

Package [Rd2markdown](https://github.com/jbryer/Rd2markdown) by jbryer


## References

Murdoch, D. (2010). [Parsing Rd files](http://developer.r-project.org/parseRd.pdf)


## Examples

```r
## give source directory of your package
pkg_dir = "~/git/MyPackage"

## specify, where reference manual shall be stored
out_dir = "/var/www/html/R_Web_app/md/"

## create reference manual
## ReferenceManual(pkg = pkg_dir, outdir = out_dir)
```





# `render_manual_github`

Render manual as a github document

## Description

Render manual file, as produced by [ReferenceManual](#referencemanual) , as a github document.


## Usage

```r
render_manual_github(rmd_man_file, man_file = NULL, outdir = getwd(),
  pkg = getwd(), title = NULL, toc = FALSE, toc_depth = 2,
  author = NULL, toplinks = FALSE, knitr_opts_chunk = list(tidy = TRUE),
  nocodelinks = F, ...)
```


## Arguments

Argument      |Description
------------- |----------------
`rmd_man_file`     |     input .Rmd file - usually output of ReferenceManual.  See Details section.
`man_file`     |     output .md file - final md file formatted as gfm.  If NULL, will be same as `rmd_man_file` with ".md" extension.
`outdir`     |     output directory
`pkg`     |     package name
`title`     |     title of manual.  If NULL, set to "Package 'pkg name'"
`toc`     |     logical. whether or not table of contents should be added.  If `FALSE` , `toc_depth` and `toplinks` will be ignored.
`toc_depth`     |     table of contents heading depth
`author`     |     Author if not already in package help
`toplinks`     |     logical. whether links to the top of the table of contents should be included at each topic header.
`knitr_opts_chunk`     |     options for [`knitr::opts_chunk`](#knitr::optschunk)
`nocodelinks`     |     logical.  if TRUE, will remove code tags from links that have them. improves conversion to .rst
`...`     |     other arguments passed to [`rmarkdown::render`](#rmarkdown::render)

## Details

You will need to have [rmarkdown-package](#rmarkdown-package) installed.  If the `rmd_man_file`
 file contains yaml front matter (as parsed by
[`yaml_front_matter`](#yamlfrontmatter)), then none of the options besides the filenames,
and output directory are used (though you can still pass arguments to `rmarkdown::render`
with `...`).


## Value

value of running `rmarkdown::render`




# `simpleCap`

Make first letter capital

## Description

Capitalize the first letter of every new word. Very simplistic approach.


## Usage

```r
simpleCap(x)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     Character string

## Value

character vector with capitalized first letters




# `stripWhite`

Strip white space

## Description

Strip white space (spaces only) from the beginning and end of a character.


## Usage

```r
stripWhite(x)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     character to strip white space from

## Value

a character with white space stripped




# `trim`

Trim

## Description

Trim whitespaces and newlines before and after


## Usage

```r
trim(x)
```


## Arguments

Argument      |Description
------------- |----------------
`x`     |     String to trim

## Value

character vector with stripped whitespaces




