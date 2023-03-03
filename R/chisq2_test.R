chisq2_test_norm <- function(values_vec, alpha = 0.05, ) {
  
  hist_df <- hist_table(values_vec)
  N <- sum(hist_df$abs_freq)
  g <- nrow(hist_df)
  chi_metric <- 0
  for (i in 1:g) {
    theor <- N * (integrate(dnorm, lower = -Inf, upper = hist_df$high[i], 
                            mean=mean(values_vec), sd = sd(values_vec), 
                            abs.tol = 0)$value - 
                    integrate(dnorm, lower = -Inf, upper = hist_df$low[i], 
                              mean=mean(values_vec), sd = sd(values_vec), 
                              abs.tol = 0)$value)
    chi_metric <- chi_metric + (hist_df$abs_freq[i] - theor)^2 / theor 
  }
  
  return(list("test" = (chi_metric < qchisq(1 - alpha / 2, df = g - 2 - 1)), 
              "chi-squared metric" = chi_metric, 
              "theor" = qchisq(1 - alpha / 2, df = g - 2 - 1)))
}
