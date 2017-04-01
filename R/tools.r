deparse1 = function (call, collapse = "") 
{
    paste0(deparse(call, width = 500), collapse = collapse)
}

#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}


first.non.null = function(...) {
  args = list(...)
  args = args[!sapply(args, is.null)]
  if (length(args)==0) return(NULL)
  args[[1]]
}

# takes a vector of ids
# replaces it by a vector of indices starting from 1
id.to.index = function(id, unique.ids = unique(id)) {
	match(id, unique.ids)
}
