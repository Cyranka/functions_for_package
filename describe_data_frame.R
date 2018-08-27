describe <- function(data_frame){
  require(dplyr)
  x <- tibble(
    `Variable name` = colnames(data_frame),
    `Variable type` = sapply(data_frame, class),
    `Unique values` = unname(sapply(data_frame, function(i)length(unique(i)))),
    `Total Missing` = unname(sapply(data_frame, function(i)sum(is.na(i))))
  )
  
 return(x) 
}

##Not run
##describe(mtcars)