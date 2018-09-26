require(lattice)
require(latticeExtra)


matrix_data <- function(Beta) {
n <- 5
a <- matrix (0, 25, ncol = 1)

types <- c("One", "Two", "Three", "Four", "Five")

#Create Initial Matrix 
initial <- (1/(n*n))
  for (col in 1:nrow(a)) {
    a[col,1] <- initial
  }
print(a)

b <- matrix (0, 25, 3, dimnames = list(NULL, c("Type_1", "Type_2", "Observation")))
print(b)

for (col in 1:nrow(a)) {
      a[col,1] <- (a[col,1] * (Beta * (row*n) + Beta * (col*n)))
}

T <- sum(a)
a <- a/T
  
j <- 1
k <- 1
l <- 1
for (row in 1:nrow(b)) {
    b[row,3] <- as.integer(a[j, 1] * 100)
    b[row, 1] <- types[k]
    b[row, 2] <- types[l]
    j <- j + 1
      
    if (k < 5) {
      k <- k + 1
    } else {
      k <- 1
      l <- l + 1
    }
}

df <- as.data.frame(b)
return (df)
}

df <- matrix_data(3)
print(df)

cloud(Observation~Type_1+Type_2, df, panel.3d.cloud=panel.3dbars, col.facet='blue', 
    xbase=0.2,  ybase=0.2, zlab = NULL, 
    scales=list(arrows=FALSE, col=1), 
    par.box = list(col = NA), lcol=NA)

  
  
  