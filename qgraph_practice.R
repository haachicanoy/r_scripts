# qgraph practice
# H. Achicanoy
# CIAT, 2016

library(qgraph)

# Example 1, simple network
input <- matrix(c(0,1,1,
                  0,0,1,
                  0,0,0), 3, 3, byrow = TRUE)
qgraph(input)

# Example 2, a little bit complex network
input2 <- structure(c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                      0, 0, 0, 1, 0), .Dim = c(8L, 8L))
qgraph(input2)

# Example 3, directed network
input <- matrix(1, 3, 3)
qgraph(input)
qgraph(input, directed = TRUE)

input[1,2] <- 0
qgraph(input)

# Example 4, 
input <- matrix(c(0,1,2,
                  0,0,3,
                  0,0,0), 3, 3, byrow = TRUE)
qgraph(input)

input <- matrix(c(0,1,-2,
                  1,0,3,
                  -2,3,0), 3, 3, byrow = TRUE)
qgraph(input)

# Example 5, layout
input <- matrix(1,3,3)
L <- matrix(c(0,1,
              1,1,
              0.5,0), ncol = 2, byrow = TRUE)
qgraph(input,layout=L)

L <- matrix(c(0,1,
              1,1,
              0,0), ncol = 2, byrow = TRUE)
qgraph(input,layout=L)

Q <- qgraph(input)
qgraph(input2, layout=Q$layout)

# Example 6, big5 dataset
data(big5)
str(big5)

qgraph(cor(big5),minimum=0.25)

# Example 7, groups argument
# List:
groups <- list(A = c(1,2,3,4,5),
               B = c(6,7,8,9,10))
# Factor:
groups <- c("A","A","A","A","A",
            "B","B","B","B","B")
# Result:
qgraph(matrix(1,10,10),groups=groups)

# Example 8, groups for big5 dataset
data(big5groups)
big5graph <- qgraph(cor(big5),minimum=0.25,groups=big5groups)
qgraph(big5graph,layout="spring")
