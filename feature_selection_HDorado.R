
# Feature selection via filters,wrappers and embedded algorithms
# Hugo Andres Dorado B.
# 2018 30 08

# Load Libraries

library(here)
library(FSelector)
library(lsr)
library(caret)

# Load feature selection funs

source("C:/Users/hadorado/Desktop/Feature_Selection/feature_selection_FUNS.R")

# Load dataset

chiapas_maiz   <- read.csv(here::here('Chiapas_Maiz.csv'),row.names = 1)

# Define the model that will be process with feature selection

performance_test <-
  function(data_set,subset){
    
    inputs  <- data_set[,-ncol(data_set)]
    output  <- data_set[,ncol(data_set)]
    
    
    tr_data <-
      train(x=inputs[subset],y=output,method = 'rf',
            trControl = trainControl(method = 'cv',number = 5))
    
    tr_sults <- tr_data$results[c('RMSE', 'Rsquared')]
    tr_sults[which.max(tr_sults$RMSE),]
    
  }

#----------------------------------FILTERS-------------------------------------#

# Run the filters based in entropy

entIdx_chiapas_maiz <- entropyIndexes(chiapas_maiz)

filters_chiapas_maiz <- evaluateFilters(entIdx_chiapas_maiz,chiapas_maiz)

filts <- c("GI","GR","SU")

# Generate the best solutions

bestSelectionsFilters <- 
do.call(rbind,
lapply(1:length(filters_chiapas_maiz),
       function(x){
         fc <- filters_chiapas_maiz[[x]]
         newfc <- do.call(rbind,fc)
         perform <- data.frame(nvars = 1:nrow(newfc),
                               newfc )
         df <- data.frame(FS=filts[x],perform[ which.min(perform$RMSE),])
         
         data.frame(df,features= paste(entIdx_chiapas_maiz[[x]][,df$FS],collapse=","))
       } 
)
)
#----------------------------------WRAPPER-------------------------------------#

datasetWrapper <- chiapas_maiz



# Define the function based in the performance index that will be optimized in 
# the wraper  

evaluator <- function(subset)
{
  pt <- performance_test(datasetWrapper,subset)
  -pt$RMSE # By defaul always try to maximice
}

# Forwad

set.seed(123)

forwardSearch_chiapas_maiz <- forward.search(names(datasetWrapper)[-ncol(datasetWrapper)],evaluator)

# Backward

set.seed(123)

backward.search_chiapas_maiz <- backward.search(names(datasetWrapper)[-ncol(datasetWrapper)],evaluator)


# Hill climbing

set.seed(123)

hill.climbing.search_chiapas_maiz <- hill.climbing.search(names(datasetWrapper)[-ncol(datasetWrapper)],evaluator)

#---------------------------------EMBEDDED-------------------------------------#

# Preprocess

dummies <- dummyVars(rendimiento_final ~ ., data = dataset) # Dummy vars
dataset_bin <- predict(dummies, newdata = dataset) 

inputs  <- dataset_bin[,-ncol(dataset_bin)]
output  <- dataset_bin[,ncol(dataset_bin)]

inputs <- inputs[,-nearZeroVar(inputs)] # Remove near zero variables

# Train the model least absolute shrinkage and selection operator

set.seed(123)

tr_data_lasso <-
  train(x=inputs,y=output,method = 'lasso',
        trControl = trainControl(method = 'cv',number = 5),preProcess = "range")

lasso_chiapas  <-
  data.frame(tr_data_lasso$results[which.max(tr_data_lasso$results$RMSE),][2:3],
             numvars = ncol(inputs),selector='lasso')

# Compare the performances, and choose the best according your
# objectives:  the best good of fitness, less variables,...

# Proceed with the REGRESSION METHOD with the new subset of variables selected



