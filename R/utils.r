##http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
## checked for empty list at the bottom
listdepth <- function(this,thisdepth = 0L){
  if(!length(this) > 0L | !is.list(this)){
    if (!length(this) > 0L ) thisdepth <- thisdepth + 1L
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,listdepth,thisdepth=thisdepth + 1L))))    
  }
}

get_names <- function(x) {
  l <- length(x)
  c("x", "y", "z", "m")[seq_len(l)]
}