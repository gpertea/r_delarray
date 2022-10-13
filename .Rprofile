## for usethis::use_git()
options(usethis.protocol = "ssh")
options(repos=structure(c(CRAN="https://cran.rstudio.com")))
## for developing R packages
#if (interactive()) {
#  suppressMessages(require("devtools"))
#  suppressMessages(require("testthat"))
#  suppressMessages(require("usethis"))
#}
nasum <- function(o) {
  if (grepl("frame", paste(class(o), collapse = ' '), ignore.case = T)) {
    s <- colSums(is.na(o), )
    s[s>0]
  } else {
    sum(is.na(o)) # assuming it's a vector
  }
}

#ht == headtail
# Show the first 6 rows & last 6 rows of a data frame
ht = function(d, n=6) rbind(head(d, n), tail(d, n))
# Show the first 5 rows & first 5 columns of a data frame
hh = function(d, n=8, r=6) {
    if (r>nrow(d)) r=nrow(d)
    if (n>ncol(d)) n=ncol(d)
    d[1:r, 1:n]
 }

tt = function(d, n=8, r=6) {
  m=ncol(d)
  sc <- m-n
  if (sc<1) sc=1
  if (r>nrow(d)) r=nrow(d)
  tail(d, r)[, sc:m]
  }

os = function(o, units="MB") {
  message("mem size: ", format(object.size(o), units=units))
}

oi = function(o) {
  d=dim(o)
  len=""
  if (is.null(d)) {
   len=paste0(" | length: ",length(o))
  } else {
   len=paste0(" | dim: ",paste(d, collapse=" x "))
  }
  message("class: ",paste(class(o), collapse=" "), ", typeof: ", typeof(o), len, " | mem: ",
          format(object.size(o), units="auto") )
}

coltypes=function(o) {
  if (grepl("frame|list", paste(class(o), collapse = ' '), ignore.case = T)) {
    sapply(o, class)
  } else {
    message("Not a list or data.frame.")
  }
}

types <- coltypes #alias
h <- utils::head # shorter head() alias
len <- base::length

options(save.defaults = list(ascii = F, compression = T))
#print(".Rprofile loaded")
