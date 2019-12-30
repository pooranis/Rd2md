$if(title)$
$title$
================
$endif$
$if(author)$
$author$
$endif$
$if(date)$
$date$
$endif$

$for(header-includes)$
$header-includes$

$endfor$
$for(include-before)$
$include-before$

$endfor$

$if(toc)$
$if(toc-title)$
$toc-title$
$endif$
$toc$

$endif$
$body$
$for(include-after)$

$include-after$
$endfor$
