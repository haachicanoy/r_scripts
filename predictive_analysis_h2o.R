# Predictive machine learning methodologies using h2o
# H. Achicanoy, 2017

# R options
options(warn = -1); options(scipen = 999)

# load packages
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(h2o)){install.packages('h2o'); library(h2o)} else {library(h2o)})

localH2O <- h2o.init(ip='127.0.0.1', port=54321)
filePath <- '/Users/haachicanoy/Documents/Online_courses/StatMachLearning/FLbigdataStats-master/bank_customer_data.csv'
market_data <- h2o.uploadFile(filePath,
                              destination_frame = '',
                              parse = TRUE,
                              header = TRUE,
                              sep = ',',
                              na.strings = c('unknown'),
                              progressBar = FALSE,
                              parse_type = 'CSV')

# Step 1: print a summary
market_data

# Step 2: fetch summary statistics
summary(market_data)
View(market_data)
str(market_data)

# Step 3: Explore data
sample_frame <- h2o.splitFrame(market_data, ratio = 0.2)[[1]]
market_data_sample <- as.data.frame(sample_frame)

by_y_job <- market_data_sample %>% group_by(y, job) %>% tally()
by_y_job

ggplot(data = by_y_job, aes(x = job, y = n, fill = y)) + geom_bar(stat = "identity", position = "dodge")

# -------------------------------------------- #
# Predictive analysis: Accept offers
# -------------------------------------------- #

market_dataex1 <- market_data[,-11]

split_data <- h2o.splitFrame(market_dataex1, ratios=0.75)
train_data <- split_data[[1]]
validation_data <- split_data[[2]]

glm_model = h2o.glm(x = 1:19,
                    y = 20,
                    training_frame = train_data,
                    validation_frame = validation_data,
                    max_iterations = 100,
                    solver="L_BFGS",
                    family="binomial",
                    alpha = 1, #L2 regularisation
                    intercept = T)

summary(glm_model)
h2o.varimp(glm_model)

# -------------------------------------------- #
# Predictive analysis: Length of call phone
# -------------------------------------------- #

market_dataex2 <- market_data[,-21]
market_dataex2 <- market_dataex2[,c("housing", "job", "day", "qualification", "month", "location", "online.banking", "checking.account", "prev.sales", "year.contact", "central.interest.rate", "employment.rate", "customer.satisfaction", "price.index", "credit.rating", "age", "staff_total", "confidence.index", "emails.month", "length")]

split_data2 <- h2o.splitFrame(market_dataex2, ratios=0.75)
train_data2 <- split_data2[[1]]
validation_data2 <- split_data2[[2]]

glm_model2 = h2o.glm(x = 1:19,
                     y = 20,
                     training_frame = train_data2,
                     validation_frame = validation_data2,
                     max_iterations = 100,
                     solver = "L_BFGS",
                     family = "gaussian",
                     alpha = 1, #L2 regularisation
                     intercept = T)

summary(glm_model2)
h2o.varimp(glm_model2)

attach(market_data)

plot(market_data$day, market_data$length)

# -------------------------------------------- #
# Dimension reduction
# -------------------------------------------- #

suppressMessages(if(!require(AER)){install.packages('AER'); library(AER)} else {library(AER)})

# Load dataset
data("HousePrices")

# Create a new variable in h2o
housing_data <- as.h2o(HousePrices)

# Response variable
response <- housing_data[,1]

# Covariates
covariates <- housing_data[,-1]

# Build a PCA model
pca_model <- h2o.prcomp(training_frame = covariates,
                        k = 11,
                        max_iterations = 1000,
                        transform = "STANDARDIZE",
                        pca_method = "GramSVD"
) 

summary(pca_model)

pca_model@model

# Comparing the predictive power of dimension reduced data

# Get the PCs in a data frame
pc_data <- h2o.predict(pca_model, covariates)

# Take the first 5
pc_data <- pc_data[,1:5]

# Add the response to the data frame
pc_glm_data <- h2o.cbind(pc_data, response) 

# Split for training and validation
pc_glm_split <- h2o.splitFrame(pc_glm_data, ratios=0.75)
pc_train_data <- pc_glm_split[[1]]
pc_validation_data <- pc_glm_split[[2]]

# Fit the predictive Model: Linerar Regression
glm_pca_model = h2o.glm(x = 1:5,
                        y=6,
                        training_frame = pc_train_data,
                        validation_frame = pc_validation_data,
                        max_iterations = 100,
                        solver = "L_BFGS",
                        family = "gaussian",
                        link = "identity",
                        alpha = 0,
                        lambda = 0,
                        intercept = T)

summary(glm_pca_model)

# -------------------------------------------- #
# Clustering
# -------------------------------------------- #

# Exclude 11th feature and the response variable
cluster_data <- market_data[,-c(11,ncol(market_data))]

# Fit k-means algorithm
varying_k <- function(k){
  cluster_model <- h2o.kmeans(training_frame = cluster_data,
                              k = k,
                              standardize = T,
                              init = "Random"
  )
  # Examine the model
  return(summary(cluster_model))
}
varying_k(k = 3)
varying_k(k = 2)
varying_k(k = 5)
varying_k(k = 10)

# Close the connection
h2o.shutdown(prompt = TRUE)

# Construct a decision tree
# Load packages
suppressMessages(if(!require(rpart)){install.packages('rpart'); library(rpart)} else {library(rpart)})
suppressMessages(if(!require(rpart.plot)){install.packages('rpart.plot'); library(rpart.plot)} else {library(rpart.plot)})

# Load data
data("ptitanic")

# Build a decision tree
first.tree <- rpart(survived ~ pclass + sex + age + sibsp + parch, data = ptitanic)
prp(first.tree)
