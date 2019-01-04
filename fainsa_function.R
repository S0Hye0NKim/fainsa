
## find H-index

H_index <- function(num_cite) {
  num_cite <- sort(num_cite, decreasing = TRUE) ## integer vector of the number of citation
  ranking <- 1:length(num_cite)
  
  if(num_cite[1] == 0) {
    return(0)
  }
  
  H_index <- which(num_cite == ranking)
  
  if(length(H_index) == 0) {
    H_index <- which(ranking < num_cite) %>% max()
  }
  
  return(H_index)
}