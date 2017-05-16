# Comparing k-nearest neighbors and linear regression
# Implemented by: H. Achicanoy
# 2017

# Read in the training data
X <- as.matrix(read.table(gzfile("zip.train.gz")))
y2or3 <- which(X[, 1] == 2 | X[, 1] == 3)
X.train <- X[y2or3, -1]
y.train <- X[y2or3, 1] == 3

# Read in the test data
X <- as.matrix(read.table(gzfile("zip.test.gz")))
y2or3 <- which(X[, 1] == 2 | X[, 1] == 3)
X.test <- X[y2or3, -1]
y.test <- X[y2or3, 1] == 3

drawDigit <- function(x) {
  for (i in 1:16) {
    for (j in 1:16) {
      color <- gray(1 - (1 + x[(i - 1) * 16 + j])/2)
      grid.rect(j, 17 - i, 1, 1, default.units = "native",
                gp = gpar(col = color, fill = color))
    }
  }
}

library(grid)
grid.newpage()
pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6)))
for (k in 1:25) {
  pushViewport(viewport(x = (k - 1)%%5 + 1, y = 5 - floor((k - 1)/5), width = 1, 
                        height = 1, xscale = c(0, 17), yscale = c(0, 17),
                        default.units = "native"))
  drawDigit(X.train[k, ])
  popViewport(1)
}
popViewport(1)

# Classification by linear regression
L <- lm(y.train ~ X.train)
yhat <- (cbind(1, X.test) %*% L$coef) >= 0.5
L.error <- mean(yhat != y.test)

# Classification by k-nearest neighbors
library(class)
k <- c(1, 3, 5, 7, 15)
k.error <- rep(NA, length(k))
for (i in 1:length(k)) {
  yhat <- knn(X.train, X.test, y.train, k[i])
  k.error[i] <- mean(yhat != y.test)
}

# Compare results
error <- matrix(c(L.error, k.error), ncol = 1)
colnames(error) <- c("Error Rate")
rownames(error) <- c("Linear Regression", paste("k-NN with k =", k))
error

plot(c(1, 15), c(0, 1.1 * max(error)), type = "n", main = "Comparing Classifiers", 
     ylab = "Error Rate", xlab = "k")
abline(h = L.error, col = 2, lty = 3)
points(k, k.error, col = 4)
lines(k, k.error, col = 4, lty = 2)
