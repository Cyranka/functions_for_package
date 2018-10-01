#' Cumulative Table
#'
#' A function to quickly create cumulative tables
#' @model A linear model, class "lm"
#' @keywords Diagnostic plots
#' @export
#' @examples
#' lm_diagnostic_plots(fit_1), where fit_1 = lm(y~..., data = df)


lm_diagnostic_plots <- function(model){
  library(tidyverse);library(MASS)
  
  ##Residuals vs fitted values plot
  residuals_vs_fitted <- tibble(`Fitted values` = model$fitted.values,
                                Residuals = model$residuals) %>%
    ggplot(aes(x = `Fitted values`, y = Residuals)) + geom_point(size = 1) + 
    geom_smooth(col = "red", se = FALSE) + theme_bw() + 
    labs(title = "Residuals vs. fitted values",
         subtitle = "Red line represents loess fit") + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          plot.subtitle = element_text(size = 13),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))
  
  
  possible_outliers <- tibble(`Fitted values` = model$fitted.values,
                              `Studentized Residuals` = MASS::studres(model),
                              observation = names(model$fitted.values)) %>%
    filter(abs(`Studentized Residuals`) >3)
  
  studentized_vs_fitted <- tibble(`Fitted values` = model$fitted.values,
                                  `Studentized Residuals` = MASS::studres(model)) %>%
    ggplot(aes(x = `Fitted values`, y = `Studentized Residuals`)) + geom_point(size = 1) + 
    geom_smooth(col = "red", se = FALSE) + theme_bw() + 
    labs(title = "Studentized residuals vs. fitted values",
         subtitle = "") + 
    geom_hline(yintercept = 3, linetype = 2, col = "red") + 
    geom_hline(yintercept = -3, linetype = 2, col = "red") + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          plot.subtitle = element_text(size = 13),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12)) + 
    scale_y_continuous(breaks = seq(-3,3, by = 1),labels = c("-3","-2","-1","0","1","2","3")) +
    geom_text(aes(y = `Studentized Residuals`, x = `Fitted values`, label = observation), data = possible_outliers,
              nudge_x = 0.6)
  
  return(list(residuals_vs_fitted, studentized_vs_fitted))
}
