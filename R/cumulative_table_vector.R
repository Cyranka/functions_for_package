#' Cumulative Table
#'
#' A function to quickly create cumulative tables
#' @param df, column.  A data frame and a column within that data frame. The column must be numeric
#' @keywords Cumulative, Table
#' @export
#' @examples
#' cumulative_table_vector(mtcars, "carb")



cumulative_table_vector <- function(df, column){
  require(dplyr)
  p <- df %>% group_by_(column) %>% tally() %>%
    mutate(percent = round(n/sum(n)*100,2),
           cumulative = round(cumsum(n)/nrow(df)*100,2))
  return(p)
}