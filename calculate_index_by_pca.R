# Calculate index using principal components
# Implemented by: H. Achicanoy
# 2016

# Load packages
# options(warn=-1); options(scipen = 999)
# suppressMessages(if(!require(FactoMineR)){install.packages('FactoMineR'); library(FactoMineR)} else {library(FactoMineR)})

# Load data
# df # Data matrix
# analysis <- PCA(df, scale.unit = T, graph = F)

# Calculate evaluation index
index_cal  <- function(analysis){
  
  analysis <- analysis
  a_class  <- class(analysis)[1]
  
  if(a_class=="MFA"){
    
    w <- analysis$eig[,1] # Extraccion de valores propios
    q <- w/sum(w)         # Porcentaje de varianza explicada
    i <- q[1]*analysis$quanti.var$coord[,1] + q[2]*analysis$quanti.var$coord[,2] # Ponderaciones de variables
    l <- i/sum(i)         # Estandarizacion
    Ind0 <- scale(analysis$call$XTDC)%*%l   # Ponderacion individuos
    Ind1 <- (exp(Ind0))/(1+(exp(Ind0)))*100 # Indice final
    return(Ind1)
    
  } else {
    
    if(a_class=="PCA"){
      
      w <- analysis$eig[,1] # Extraccion de valores propios
      q <- w/sum(w)         # Porcentaje de varianza explicada
      i <- q[1]*analysis$var$coord[,1] + q[2]*analysis$var$coord[,2] # Ponderaciones de variables
      l <- i/sum(i)         # Estandarizacion
      Ind0 <- scale(analysis$call$X)%*%l      # Ponderacion individuos
      Ind1 <- (exp(Ind0))/(1+(exp(Ind0)))*100 # Indice final
      return(Ind1)
      
    } else {
      
      cat("The crop tal hasn't appropiated data for index construction \n")
      
    }
    
  }
  
}

# index_values <- index_cal(analysis)
