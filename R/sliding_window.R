sliding_window_1d <- function(values_vec, window_width=3, step_size=1) {
  N <- length(values_vec)
  nrows <- (N - window_width) %/% step_size + 1
  new_matrix <- matrix(0, nrow = nrows, ncol = window_width)
  for (i in (1:nrows)) {
    new_matrix[i, ] <- values_vec[((i-1) * step_size + 1):((i-1) * step_size + window_width)]
  }
  return(new_matrix)
}


sliding_window_1d(letters, 3, 1)
