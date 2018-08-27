#' Cumulative Table
#'
#' Stata inspired summary function
#' @param vector. A numeric vector
#' @keywords summary, summarise
#' @export
#' @examples
#' summary_function(mtcars$carb)



summary_function <- function(vector){
  x <- tibble(Mean = round(mean(vector),2),
              Median = round(median(vector),2),
              Std_dev = round(sd(vector),2),
              Variance = round(var(vector),2),
              Min = min(vector),
              Max = max(vector),
              Obs = length(vector))
  return(x)
}