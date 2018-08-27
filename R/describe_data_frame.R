#' Describe function
#'
#' Inspired by a function with the same name in STATA, it quickly displays information about variables
#' @param data_frame.  A basic data frame or tibble
#' @keywords describe
#' @export
#' @examples
#' describe(mtcars)


describe <- function(data_frame){
  require(dplyr)
  x <- tibble(
    `Variable name` = colnames(data_frame),
    `Variable type` = sapply(data_frame, class),
    `Unique values` = unname(sapply(data_frame, function(i)length(unique(i)))),
    `Total Missing` = unname(sapply(data_frame, function(i)sum(is.na(i)))),
    `% Missing` = unname(sapply(data_frame, function(i)sum(is.na(i))))/nrow(data_frame)
  )
  
 return(x) 
}

##Not run
##describe(mtcars)