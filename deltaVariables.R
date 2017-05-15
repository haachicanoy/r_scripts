
require(raster)
require(rgdal)
require(tidyverse)
require(rgeos)
require(gtools)

path <- 'D:/article_arroz'
files_cur <- list.files(paste0(path, '/_raster/_variables/Current_Variables_Ok'), full.names = T, pattern = '.asc$')%>%
              grep('bio_', ., value = T) %>%
              mixedsort()
lyr_cur <- lapply(files_cur, FUN = raster)

models   <- list.files(paste0(path, '/_raster/_variables/2050_Ok'), full.names = F)
toMatch  <- unlist(strsplit(basename(files_cur), '.asc'))
namesVar <- toMatch

# Create folders

for(i in 1:length(models)){
  
  dir.create(paste0(path, '/_raster/_variables/_deltas/', models[i]))
  
}


# Deltas raster

dif     <- list()

list.files2 <- Vectorize(FUN = list.files, vectorize.args = "path")
lyr_fut <- list.files2(paste0(path, '/_raster/_variables/2050_ok/', models), full.names = T) %>%
  grep(paste0(toMatch, collapse = '|'), ., value = T) %>%
  mixedsort()

grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
lyr_id <- as.data.frame(grep2(pattern = namesVar, x = lyr_fut))

lyr_fut2 <- lapply(1:ncol(lyr_id), function(i){
  test <- lyr_fut[as.numeric(lyr_id[,i])]
  test <- lapply(test, raster)
  return(test)
})


rec.list <- function(len){
  if(length(len) == 1){
    vector("list", len)
  } else {
    lapply(1:len[1], function(...) rec.list(len[-1]))
  }
}

dif2 <- rec.list(c(5, 32))

for(i in 1:length(namesVar)){
  
  for(j in 1:length(models)){
    
    dif2[[i]][[j]] <- lyr_fut2[[i]][[j]] - lyr_cur[[i]]
    
  }
  
}

dif3 <- lapply(dif2, raster::stack)
dif4 <- lapply(dif3, mean, na.rm = T)

plot(stack(dif4))

Map('writeRaster', x = dif4, filename = paste0(path, '/_raster/_variables/_deltas_mean/', namesVar, '.asc'))


# for(i in 1:length(models)){ 
#   
#   print(models[i])
#   
#   lyr_fut <- list.files(paste0(path, '/_raster/_variables/2050_ok/', models[i]), full.names = T) %>%
#                       grep(paste0(toMatch, collapse = '|'), ., value = T) %>%
#                       mixedsort() %>%
#                       lapply(raster)
#   
#   print('Differences...')
#   
#   for(j in 1:length(lyr_fut)){
#     
#     dif[[j]] <- lyr_fut[[j]] - lyr_cur[[j]]
#     
#   }
#   
#   for(m in 1:length(dif)){
#    
#     writeRaster(dif[[m]], paste0(path, '/_raster/_variables/_deltas/', models[i], '/', namesVar[m], '.asc'))
#    
#   }
#   
# }



