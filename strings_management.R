
library(tidyverse)
library(readr)
library(xlsx)
library(readxl)
library(zoo)

df <- read_excel("D:/CC/_bd/_cocoa/_brazil/_tables/_ibge/Copy of mesomicromunic_pam2002_permpmun.xls", sheet = 1)

head(df)
class(df)

df2 <- df
df2 <- as.data.frame(df2)

countSpaces <- function(x){
  counter <- 0
  coll <- numeric()
  vec <- strsplit(x," ")[[1]]
  for(i in 1:length(vec)){
    if (vec[i]==""){
      counter <- counter+1
    }
    else{
      if (counter!=0) coll <- c(coll,counter)
      counter <- 1
    }
  }
  coll
}

countSpaces_vec <- Vectorize(FUN = countSpaces, vectorize.args = "x")

countList <- countSpaces_vec(x = df2$`Mesorregiões, microrregiões e os municípios`)
countList <- unlist(lapply(1:length(countList), function(i){
  x <- countList[[i]][1]
  return(x)
}))

df2$count <- countList; rm(countList)
View(df2[which(is.na(df2$count)),])
df2$count[which(is.na(df2$count))] <- -99
barplot(table(df2$count))

names(df2)[1] <- "mpio"

df2$crop   <- NA
df2$region <- NA

df2$crop[which(df2$count >= -99 & df2$count <= 2)] <- df2$mpio[which(df2$count >= -99 & df2$count <= 2)]
df2$region[which(df2$count == 5)] <- df2$mpio[which(df2$count == 5)]

df2$crop   <- na.locf(zoo(df2$crop))
df2        <- df2[-which(df2$count >= -99 & df2$count <= 2),]; rownames(df2) <- 1:nrow(df2)
df2$region <- na.locf(zoo(df2$region))
df2        <- df2[-which(df2$count == 5),]; rownames(df2) <- 1:nrow(df2)

df2$mpio   <- gsub(pattern = " $", replacement = "", x = gsub(pattern = "^ ", replacement = "", x = gsub(pattern = "\\s+", replacement = " ", x = df2$mpio)))
df2$crop   <- gsub(pattern = " $", replacement = "", x = gsub(pattern = "^ ", replacement = "", x = gsub(pattern = "\\s+", replacement = " ", x = df2$crop)))
df2$region <- gsub(pattern = " $", replacement = "", x = gsub(pattern = "^ ", replacement = "", x = gsub(pattern = "\\s+", replacement = " ", x = df2$region)))

df2$mpio   <- toupper(df2$mpio)

write.csv(df2, 'D:/CC/_bd/_cocoa/_brazil/_tables/_ibge/mesomicromunic_2002_order.csv')

