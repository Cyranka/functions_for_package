#' Cut data frame
#'
#' A function that breaks a data frame into a smaller number of data frames of equal number of rows.
#' @model A data frame or a tibble
#' @keywords Break data frame
#' @export
#' @examples
#' x <- as_tibble(1:1000)
#' p <- cut_df(x,2)
#' 

cut_df <- function(df, parts){
  x <- df %>% ungroup() %>%
    mutate(row = row_number()) %>%
    mutate(group = as.numeric(cut_number(row, parts)))
  
  y <- lapply(1:parts, function(i) filter(x, group == i))
  return(y)
}

