library(pracma)

sparse <- function(C) {
  
  #C <- exp(- theta * Dmat)
  n <- nrow(C)
  A <- matrix(0, n, n)
  D <- matrix(0, n, n)
  D[1, 1] <- C[1, 1]
  
  # Initial computation of A and D
  for(i in 1:(n - 1)) {
    A[i + 1, 1:i] <- solve(C[1:i, 1:i], C[1:i, i + 1])
    D[i + 1, i + 1] <- C[i + 1,i + 1] - dot(C[i + 1, 1:i], A[i + 1, 1:i])
  }
  
  # Sparse computaton of A and D
  N <- vector(mode = "list", length = n)
  for(i in 1:(n-1)) {
    N[[i + 1]] <- which(sapply(1:i, \(j) (j <= i) & A[i + 1, j] != 0))
    A[i + 1, N[[i + 1]] ] <- solve(C[ N[[i + 1]], N[[i + 1]] ], C[ N[[i + 1]], i + 1])
    D[i + 1, i + 1] <- C[i + 1, i + 1] - dot(C[i + 1, N[[i + 1]] ], A[i + 1, N[[i + 1]] ])
  }
  
  Ctilde <- solve(diag(n) - A) %*% D %*% t(solve(diag(n) - A))
  
  if (!isSymmetric(Ctilde)) {
    Ctilde <- (Ctilde + t(Ctilde)) / 2
  }
  
  #CtildeInv <- t(diag(n) - A) %*% solve(D) %*% (diag(n) - A)
  
  #return(list(Ctilde = Ctilde, CtildeInv = CtildeInv))
  return(Ctilde)
}



