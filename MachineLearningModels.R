########################################################################################
########### ***** 
##                  Training the Identification Models
##                               
###
####

rm(list = ls())

libs = c("snowfall", "caret", "nnet", "SDMTools", "stringr", "raster", "ff")
lapply(libs, require, character.only = T)

# library(e1071)
# library(mlbench)

####### <>>>>>>>> Loading the functions
SVMRadialForestModel = function(x){
  
  output <- ncol(normMat)
  setseed <- .Random.seed[1:n.ite]
  nOutPut <- ncol(normMat)
  
  head(normMat)
  
  inTrain <- createDataPartition(y = normMat[,output], p = 0.7, list = F)
  training <- normMat[inTrain,]
  testing <- normMat[-inTrain,]
  
  ctrl <- trainControl(method = "repeatedcv", # 10 fold cross validation
                       repeats = 5,           # Do 5 repititions of cv
                       # summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                       classProbs = F)
  
  svm.tune <- train(x = training[,-ncol(training)],
                    y = training[,output],
                    method = "svmRadial",           # Radial kernel
                    tuneLength = 9,				 	        # 9 values of the cost function
                    # preProc = c("center", "scale"), # Center and scale data
                    trControl = ctrl,
                    importance = T)
  varImportance = varImp(svm.tune)
  
  VarImportance = row.names(varImportance$importance[order(varImportance$importance[,1], decreasing = T),])[1:20]
  
  CM = table(testing[,output], predict(svm.tune, testing[,-ncol(training)]))
  predictNum = predict(svm.tune, testing[,-ncol(testing)])
  ValidatedNum = testing[,output]
  levels(ValidatedNum) = levels(predictNum) = 1:length(levels(training[,ncol(training)]))
  ValidatedNum = as.numeric(as.character(ValidatedNum))
  predictNum = as.numeric(as.character(predictNum))
  rsquare <- R2(predictNum, ValidatedNum) * 100
 
  return(list(svm.tune, rsquare, training, testing, VarImportance))
  
}


RandomForestModel = function(x){
  
  output <- ncol(normMat)
  setseed <- .Random.seed[1:n.ite]
  nOutPut <- ncol(normMat)
  
  #  pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)    
  
  head(normMat)
  #  info <- sprintf("%d%% done", round((i/(nb.it)*100)))
  # setWinProgressBar(pb, paste(i/(nb.it)*100), label=info)
  
  inTrain <- createDataPartition(y = normMat[,output], p = 0.7, list = F)
  training <- normMat[inTrain,]
  testing <- normMat[-inTrain,]
  
  table(training$Class)
  
  grid <- expand.grid(mtry = round((ncol(training)-1)/3))
  
  modelRF <- train(training[,-ncol(training)], training[,output],
                   method = "rf", tuneGrid = grid, importance = TRUE, ntree = 2000)
  
  CM = table(testing[,output], predict(modelRF, testing[,-ncol(training)]))
  predictNum = predict(modelRF, testing[,-ncol(testing)])
  ValidatedNum = testing[,output]
  levels(ValidatedNum) = levels(predictNum) = 1:length(levels(training$Class))
  ValidatedNum = as.numeric(as.character(ValidatedNum))
  predictNum = as.numeric(as.character(predictNum))
  rsquare <- R2(predictNum, ValidatedNum) * 100
  
  return(list(modelRF, rsquare, training, testing, modelRF$results[3]))
  
}

####### ********> Reading the events file

server = Sys.info()[1]

serverPath = switch(server, "Linux" = "/mnt", "Windows" = "//dapadfs")
FolPrincipal = paste0(serverPath, "/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/FEDEARROZ/REMOTE_SENSING/Sentinel_Saldanna")
setwd(FolPrincipal)
#dataModel=read.csv("DataModel/PhenStages_TS10_L8_NDVI_500_2Months20151221.csv",row.names=2)
dataModel = read.csv("DataModel/Phen_Identification/PhenStages_TS_S2L8L7_BSI_LSWI_NDVI_96days_2015122120160110_V1_Red.csv", row.names = 1)
#dataModel=dataModel[,-1]
head(dataModel)
dim(dataModel)
#### **** 
table(dataModel$stage)
normMat = dataModel
n.ite = 20
ncores = 20
sfInit(parallel = T, cpus = ncores)
sfLibrary(caret)
sfLibrary(nnet)
sfLibrary(SDMTools)
sfExport("SVMRadialForestModel")
sfExport("normMat")
sfExport("RandomForestModel")
sfExport("n.ite")
cat("Starting model process: \n")
Sys.time()->start

allModelsAndRMSE.RF <- sfLapply(1:n.ite, SVMRadialForestModel)

setwd(FolPrincipal)
save(allModelsAndRMSE.RF, file = "RData_OutputModels/Phen_Identification/SVM_20Models_2015122120160110_TS_BSI_LSWI_NDVI_Red.RData")
allModelsAndRMSE.RF <- sfLapply(1:n.ite, RandomForestModel)
save(allModelsAndRMSE.RF, file="RData_OutputModels/Phen_Identification/RF_20Models_2015122120160110_TS_BSI_LSWI_NDVI_Red.RData")

sfStop()
print(Sys.time() - start)
load(file = "RData_OutputModels/Phen_Identification/RF_20Models_2015122120160110_TS_BSI_LSWI_NDVI_Red.RData")

allRMSE.RF   <- unlist(lapply(allModelsAndRMSE.RF, function(x){x[[2]]}))    
bestModels.RF<- order(allRMSE.RF, decreasing = T)

BestModel.RF <- lapply(allModelsAndRMSE.RF, function(x){x[[1]]})[bestModels.RF[15]][[1]]
modelRF = BestModel.RF

testing = lapply(allModelsAndRMSE.RF, function(x){x[[4]]})[bestModels.RF[15]][[1]]
CM = table(testing[,ncol(testing)],predict(modelRF, testing[,-ncol(testing)]))

vaRelevance = varImp(modelRF, scale = T)$importance
MLM = allModelsAndRMSE.RF[[1]]
listVarImportance = lapply(allModelsAndRMSE.RF, function(MLM){
  vaRelevance = varImp(MLM[[1]], scale = T)$importance
  vaRelevance = vaRelevance[order(vaRelevance[,2],decreasing = T),][1:15,]
  return(row.names(vaRelevance))
})

unique(unlist(listVarImportance))[order(unique(unlist(listVarImportance)))]
normMat = normMat[,c(which(names(normMat)%in%unique(unlist(listVarImportance))), ncol(normMat))]
write.csv(normMat, "DataModel/Phen_Identification/PhenStages_TS_S2L8L7_BSI_LSWI_NDVI_96days_2015122120160110_V1_Red.csv")
bestModels.RF
Finding_BestImportanceVariables()
