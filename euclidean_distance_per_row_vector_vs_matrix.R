# Distance vector vs matrix
library(pacman)
pacman::p_load(Rfast)

ref <- matrix(1:19, nrow = 1, byrow = T)
rst <- matrix(runif(10*19, min = 1, max = 19), nrow = 10, byrow = T)

dista(xnew = ref, x = rst, type = 'euclidean')

# Test first distance
matrix(c(as.numeric(ref), as.numeric(rst[1,])), ncol = 19, byrow = T) |> dist()
