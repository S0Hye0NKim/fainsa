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


find_IF <- function(JourNm, year, onlyIF = FALSE, only_perc = FALSE) {
  # filter with Journal name and its published year.
  # It needs the reference table 
  # return Impact Factor with minimum Percentage
  env <- .GlobalEnv %>% as.list %>% names
  if(!"JCR_List" %in% env) {
    return("Load the JCR_List, the reference table!")
  }
  JourNm <- JourNm %>% 
    sub("amp;", "", x = .) %>%
    sub("&", "and", x = .) %>%
    gsub("[^[:alnum:]]", "", x = .)
  if (year == 2020) {year <- (year - 2)}
  else {year <- year - 1}
  JourNm <- JourNm %>%
    gsub(pattern = " ", replacement = "") %>%
    toupper()
  target <- filter(JCR_List, Title == JourNm, Year == year) %>%
    filter(Percent == min(Percent))
  if(nrow(target) == 0) {
    target <- tibble(Title = JourNm, IF = NA, Percent = NA, Year = year)
  }
  if(onlyIF == TRUE){
    return(target %>% unique %>% select(IF) %>% as.numeric)
  } else if(only_perc == TRUE) {
    return(target %>% unique %>% select(Percent) %>% as.character)
  } else {
    return(target %>% unique)}
}
    



find_JourClass <- function(JourName, type = "Business") {
  # Find Journal Class in Business and Economics
  env <- .GlobalEnv %>% as.list %>% names
  if(!"Business" %in% env | !"Economics" %in% env) {
    return("Load the reference table for Business&Economics!")
  }
  JourName <- sub("amp;", "", JourName) %>%
    gsub(" ", "", .) %>%
    sub("&", "and", x = .) %>%
    gsub("[^[:alnum:]]", "", x = .) %>%
    toupper
  if(type == "Economics") {
    target <- filter(Economics, Title == JourName)
  } else {
    target <- filter(Business, Title == JourName)
  }
  if(nrow(target) == 0) {
    target <- tibble(Title = JourName, Grade = NA)
  }
  return(target %>% unique %>% select(Grade) %>% as.character)
}


redun_extract <- function(data, target = "Title"){
  # Extract duplicated value from the tibble data
  # Set target option (default is "Title")
  n_occur <- data.frame(table(data[, target]))
  result <- data[(data %>% pull(target)) %in% n_occur$Var1[n_occur$Freq > 1], ]
  return(result)
}
