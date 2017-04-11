# Top 10 data mining algorithms in plain R
# Source: https://rayli.net/blog/data/top-10-data-mining-algorithms-in-plain-r/?utm_campaign=Data%20Elixir&utm_medium=email&utm_source=Data_Elixir_41#6_PageRank
# Implemented by: H. Achicanoy
# CIAT, 2017

# 1. C5.0
suppressMessages(library(C50))
suppressMessages(library(printr))

train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces,]
iris.test <- iris[-train.indeces,]

model <- C5.0(Species ~ ., data = iris.train)
results <- predict(object = model, newdata = iris.test, type = "class")
table(results, iris.test$Species)

# 2. k-means
suppressMessages(library(stats))
suppressMessages(library(printr))

model <- kmeans(x = subset(iris, select = -Species), centers = 3)
table(model$cluster, iris$Species)

# 3. Support Vector Machines
suppressMessages(library(e1071))
suppressMessages(library(printr))

train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces,]
iris.test <- iris[-train.indeces,]

model <- svm(Species ~ ., data = iris.train)
results <- predict(object = model, newdata = iris.test, type = "class")
table(results, iris.test$Species)

# 4. Apriori
suppressMessages(library(arules))
suppressMessages(library(printr))
data("Adult")

# SUPPORT: is the percentage of records in the dataset that contain the related items.
# Here you're saying we want at least 40% support.

# CONFIDENCE: is the conditional probability of some item given you have certain other items in your itemset.
# You're using 70% confidence here.

# APPEARANCE: Association rules look like this {United States} => {White, Male}.
# You'd read this as "When I see United States, I will also see White, Male."
# There's a left-hand side (lhs) to the rule and a right-hand side (rhs).

rules <- apriori(Adult,
                 parameter = list(support = 0.4, confidence = 0.7),
                 appearance = list(rhs = c("race=White", "sex=Male"), default = "lhs"))

rules.sorted <- sort(rules, by = "lift")
top5.rules <- head(rules.sorted, 10)
as(top5.rules, "data.frame")

# 5. EM
suppressMessages(library(mclust))
suppressMessages(library(printr))

model <- Mclust(subset(iris, select = -Species))
table(model$classification, iris$Species)
plot(model)

# 6. PageRank
suppressMessages(library(igraph))
suppressMessages(library(dplyr))
suppressMessages(library(printr))

# Generates a random directed graph with 10 objects
g <- random.graph.game(n = 10, p.or.m = 1/4, directed = TRUE)
plot(g)
pr <- page.rank(g)$vector

# This code outputs the PageRank for each object
df <- data.frame(Object = 1:10, PageRank = pr)
arrange(df, desc(PageRank))

# 7. AdaBoost
suppressMessages(library(adabag))
suppressMessages(library(printr))

train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces,]
iris.test <- iris[-train.indeces,]

model <- boosting(Species ~ ., data = iris.train)
results <- predict(object = model, newdata = iris.test, type = "class")
results$confusion

# 8. kNN
suppressMessages(library(class))
suppressMessages(library(printr))

train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces,]
iris.test <- iris[-train.indeces,]

results <- knn(train = subset(iris.train, select = -Species),
               test = subset(iris.test, select = -Species),
               cl = iris.train$Species)
table(results, iris.test$Species)

# 9. Naive Bayes
suppressMessages(library(e1071))
suppressMessages(library(printr))

train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces,]
iris.test <- iris[-train.indeces,]

model <- naiveBayes(x = subset(iris.train, select=-Species), y = iris.train$Species)
results <- predict(object = model, newdata = iris.test, type = "class")
table(results, iris.test$Species)

# 10. CART
suppressMessages(library(rpart))
suppressMessages(library(printr))

train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces,]
iris.test <- iris[-train.indeces,]

model <- rpart(Species ~ ., data = iris.train)
results <- predict(object = model, newdata = iris.test, type = "class")
table(results, iris.test$Species)
