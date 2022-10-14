require('lobstr') ## to get more accurate object memory use

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

os = function(o, bytes=F) {
  v=as.numeric(lobstr::obj_size(o))
  if (bytes) {
    message("mem size: ", v)
  } else {
    message("mem size: ",paste0(format(round(v/(1024*1024),1), big.mark=','), " MB"))
  }
  invisible(v)
}

oi = function(o) {
  d=dim(o)
  len=""
  if (is.null(d)) {
    len=paste0(" | length: ",length(o))
  } else {
    len=paste0(" | dim: ",paste(d, collapse=" x "))
  }
  message("class: ",paste(class(o), collapse=' '), ", typeof: ", typeof(o), len, " | mem: ",
          paste0(format(round(as.numeric(lobstr::obj_size(o))/(1024*1024),1), big.mark=','), " MB"))
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
