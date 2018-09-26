matrix_data <- function(rows, cols, Beta) {
  a <- matrix (0, nrow = rows, ncol = cols)
  
  #Create Initial Matrix
  initial <- (1 / (n * n))
  for (row in 1:nrow(a)) {
    for (col in 1:ncol(a)) {
      a[row, col] <- initial
    }
  }
  
  b <- matrix (0, rows, cols)
  
  nrows <- 1:nrow(a)
  ncols <- 1:ncol(a)
  
  for (row in 1:nrow(a)) {
    for (col in 1:ncol(a)) {
      a[row, col] <- (a[row, col] * (beta * (row * n) + beta * (col * n)))
    }
  }
  T <- sum(a)
  a <- a / T
  ncols <-  sample(col)
  nrows <- sample(nrow)
  i <- 1
  j <- 1
  for (row in nrows) {
    j <- 1
    for (col in ncols) {
      b[row, col] <- a[i, j]
      j <- j + 1
    }
    i <- i + 1
  }
  b
}