extract_reference <- function(x){
  labs <- names(x)
  y <- c()
  for(i in 1:length(x)){
    y[[labs[i]]] <- x[[i]][1]
  }
  return(y)
}
