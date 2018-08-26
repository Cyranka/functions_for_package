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

cumulative_table_vector <- function(df, column){
    p <- df %>% group_by_(column) %>% tally() %>%
        mutate(percent = round(n/sum(n)*100,2),
               cumulative = round(cumsum(n)/nrow(x)*100,2))
    return(p)
}

poisson_dispersion_statistic <- function(glm_fit){
    pearson_chi2 <- sum(residuals(glm_fit, type = "pearson")**2)
    return(round(pearson_chi2/glm_fit$df.residual,2)) ##Dispersion statistic
}