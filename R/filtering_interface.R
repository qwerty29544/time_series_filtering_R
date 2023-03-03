source("R/sliding_window.R")

# SVD_filtering interface -------------------------------------------------


SVD_filtering <- function(ts, window_width=NA, N_last=NA, damping_ratio=0.85, 
                          k_samples=NA) {
  
  if (is.na(N_last)) {
    N_last <- length(ts)
  }
  
  if ((is.na(window_width)) | (window_width > (N_last %/% 2))) {
    window_width <- N_last %/% 2 - 1
  }
  
  time_series <-ts[(length(ts)- N_last + 1):length(ts)]
  sliding_matrix <- sliding_window_1d(time_series, window_width, 1)
  result_vsu <- svd(sliding_matrix)
  
  if (is.na(k_samples)) {
    k_samples <- which(cumsum(result_vsu$d) / sum(result_vsu$d) > damping_ratio)[1]
  }

  matrix_smoothed <- matrix(0, nrow = nrow(sliding_matrix), ncol = window_width)
  for (i in 1:k_samples) { 
    matrix_smoothed <- matrix_smoothed + 
      (result_vsu$u[, i] %*% t(result_vsu$v[, i])) * result_vsu$d[i]
  }
  
  # ts_smoothed <- 0
  # for (i in 2:(window_width - 1)) {
  #   vec <- c(matrix_smoothed[1, 1:(i-1)], 
  #            matrix_smoothed[, i], 
  #            matrix_smoothed[nrow(matrix_smoothed), (i+1):(window_width)])
  #   ts_smoothed <- ts_smoothed + as.numeric(na.omit(vec))
  # }
  # ts_smoothed <- ts_smoothed/(window_width-2)
  #   
  ts_smoothed <- c(matrix_smoothed[1, ],
                   matrix_smoothed[-1, window_width])
  
  return(list("ts_smoothed" = as.numeric(ts_smoothed), 
              "errors" = ts - ts_smoothed,
              "damping_ratio" = result_vsu$d / sum(result_vsu$d),
              "determination" = 1 - sum((ts - ts_smoothed)^2)/sum((ts - mean(ts))^2)))
}


# Сглаживание и проврка автокорреляции остатков ---------------------------
ts <- as.numeric(AirPassengers)
ts_smooth <- SVD_filtering(ts, k_samples = 1)
errors <- ts - ts_smooth$ts_smoothed
plot(ts, type = "o", pch = 19, col = "blue", cex = I(0.5))
lines(ts_smooth$ts_smoothed, type ="l", col = "red")

plot(errors, type = "o")

Box.test(errors, lag = 2, type = "Ljung-Box")
acf(errors)

# Визуализация ошибок -----------------------------------------------------
mu_err <- mean(errors)
std_err <- sd(errors) 
grid <- seq(mu_err - 4 * std_err, mu_err + 4 * std_err, length.out = 1000)
hist(errors, freq=F)
lines(grid, dnorm(grid, mu_err, std_err), col = "red")
chisq2_test_norm(errors)

# График нахождения тренда ------------------------------------------------
errors_mean <- numeric(40)
for (i in 1:40) {
  ts_smooth <- SVD_filtering(ts, k_samples = i)
  erros <- ts - ts_smooth$ts_smoothed
  errors_mean[i] <- mean(erros)
}
plot(errors_mean, type="o", pch = 19, col = "red")

# График количества информации --------------------------------------------
plot(log(ts_smooth$damping_ratio), type = "o", pch = 19, cex = I(0.5))
abline(v = 17)  



# Синус с шумом -----------------------------------------------------------
n <- 1000
grid <- seq(0, 20, length.out=n)
signal_true <- 12.45 * sin(grid) 
signal <- signal_true + rnorm(n, 0, 40)

plot(signal, type = "o", cex = I(0.5), col = "blue2", pch = 19, lty = 3)
smooth_signal <- SVD_filtering(signal, k_samples = 2)
lines(signal_true, col = "green2", lwd = I(3))
lines(smooth_signal$ts_smoothed, col = "red", lwd = I(2))

plot(log(smooth_signal$damping_ratio), type="o", cex = I(0.5), pch = 19, col = "blue")
abline(v = 4)
