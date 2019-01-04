H_index <- function(num_cite) {
  # num_cite is integer vector of the number of citation
  # returns H-index of a researcher
  num_cite <- sort(num_cite, decreasing = TRUE) 
  ranking <- 1:length(num_cite)
  #check weather H-index is 0
  if(num_cite[1] == 0) {
    return(0)
  }
  
  H_index <- which(num_cite == ranking)
  
  if(length(H_index) == 0) {
    H_index <- which(ranking < num_cite) %>% max()
  }
  
  return(H_index)
}