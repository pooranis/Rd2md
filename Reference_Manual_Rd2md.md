

<!-- toc -->

December 29, 2019

#  DESCRIPTION

```
Package: Rd2md
Title: Markdown Reference Manuals
Version: 0.0.4
Authors@R (parsed):
    * Julian Busch <jb@quants.ch> [aut, cre]
Description: The native R functionalities only allow PDF exports of
    reference manuals. This shall be extended by converting the package
    documentation files into markdown files and combining them into a
    markdown version of the package reference manual.
License: GPL
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
RoxygenNote: 7.0.2
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
rdfile = "~/git/MyPackage/man/myfun.Rd"
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
parseTag(
  x,
  pre = character(),
  post = character(),
  stripNewline = TRUE,
  stripWhite = TRUE,
  stripTab = TRUE,
  emptyNA = FALSE
)
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
Rd2markdown(
  rdfile,
  outfile,
  append = FALSE,
  section = "#",
  subsection = "##",
  run.examples = FALSE,
  Rmd = F
)
```


## Arguments

Argument      |Description
------------- |----------------
`rdfile`     |     Filepath to an .Rd file or an `Rd` object to parse.
`outfile`     |     Filepath to output file (markdown file).
`append`     |     If outfile exists, append to existing content.
`section`     |     header tag.
`subsection`     |     header tag.
`run.examples`     |     logical. should examples be run?
`Rmd`     |     logical. should the output be in Rmarkdown format or regular markdown? default: FALSE, regular markdown

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
ReferenceManual(
  pkg = getwd(),
  outdir = getwd(),
  front.matter = "",
  toc.matter = "<!-- toc -->",
  date.format = "%B %d, %Y",
  verbose = FALSE,
  title.level = 1,
  run.examples = FALSE,
  skip.topics = NULL,
  topic.groups = NULL,
  sepexported = FALSE,
  man_file = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`pkg`     |     Full path to package directory. Default value is the working directory. Alternatively, a package name can be passed. If this is the case, [`find.package`](#find.package) is applied.
`outdir`     |     Output directory where the reference manual markdown shall be written to.
`front.matter`     |     String with yaml-style heading of markdown file.
`toc.matter`     |     String providing the table of contents. This is not auto-generated. The default value is a HTML comment, used by gitbook plugin [toc](https://www.npmjs.com/package/gitbook-plugin-toc) .
`date.format`     |     Date format that shall be written to the beginning of the reference manual. If `NULL` , no date is written. Otherwise, provide a valid format (e.g. `%Y-%m-%d` ), see Details in [strptime](#strptime) .
`verbose`     |     If `TRUE` all messages and process steps will be printed
`title.level`     |     Integer.  Title header level. If including exported sections, section.level will be one more, and subsection level 2 more.  Otherwise, section.level will be the same and subsection.level only one more.
`run.examples`     |     Logical. Whether or not to run examples.
`skip.topics`     |     Character. Functions, methods, objects, etc to skip.  Should be prefix of .rd file.
`topic.groups`     |     Named list of vectors of topics in each topic group.  The group names should be the names of the list.
`man_file`     |     Character. name of output file.  Default Reference_Manual_pkgname.md
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




