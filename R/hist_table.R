library(dplyr)

# table_hist --------------------------------------------------------------

hist_table <- function(X_sample, groups = "Sturges") {
  N <- length(X_sample)
  if (stringr::str_to_lower(groups[1]) == "sturges") {
    groups <- 1 + floor(log2(N))
  }
  
  factor_groups <- cut(X_sample, groups)
  table_cut <- table(factor_groups, exclude = TRUE)
  
  Z <- names(table_cut) %>% 
    stringr::str_replace_all(pattern = "[\\(\\]]", repl = "") %>% 
    stringr::str_split(",") %>% 
    unlist() %>% 
    as.numeric()
  
  table_hist <- data.frame(groupnames = factor(x = names(table_cut), 
                                               levels = levels(factor_groups)),
                           abs_freq = as.numeric(table_cut),
                           rel_freq = as.numeric(table_cut) / N,
                           low = Z[seq(1, length(Z), 2)],
                           high = Z[seq(2, length(Z), 2)],
                           med = (Z[seq(1, length(Z), 2)] + Z[seq(2, length(Z), 2)]) / 2,
                           h = (Z[seq(2, length(Z), 2)] - Z[seq(1, length(Z), 2)]))
  return(table_hist)
}