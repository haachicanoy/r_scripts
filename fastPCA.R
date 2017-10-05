pca_svd <- function(x, modos){
  
  n     <- dim(x)[1]
  x0    <- scale(x) # *(sqrt(n)/sqrt(n-1))
  svd_o <- corpcor::fast.svd(x0)
  comp  <- svd_o$u[,1:modos, drop = F] %*% diag(svd_o$d[1:modos], length(svd_o$d[1:modos]), length(svd_o$d[1:modos])) 
  vect  <- svd_o$v[,1:modos]
  output <- list(comp, vect)
  return(output)
  
}

fpca <- pca_svd(x = mtcars, modos = 5)
tpca <- FactoMineR::PCA(X = mtcars, scale.unit = T, ncp = ncol(mtcars), graph = F)

View(tpca$ind$coord)
View(tpca$var$coord)
View(fpca[[1]])
View(fpca[[2]])
