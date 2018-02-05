
# ISRIC soils rasters

dir <- "D:/ccsosa/ISRIC/chosen/chosen_NA"

library(raster)

rasterOptions(tmpdir="D:/ccsosa/temp")
soilList <- list.files(dir, pattern=".tif$")
varList <- strsplit(soilList,split="_")
varList <- unlist(lapply(varList, function(x){x[1]}))
varList <- sort(unique(varList))

depth_var <- strsplit(soilList,split="_")
depth_var <- unlist(lapply(depth_var, function(x){x[2]}))
depth_var <- sort(unique(depth_var))

weighted_mean_raster <- function(var,...){
  library(raster)
  var_raster <- soilList[grep(pattern=var,x=soilList)]
  var_raster <- lapply(var_raster[-length(var_raster)], function(x){raster(paste(dir,"/",x,sep=""))})
  var_raster <- stack(var_raster)*10
  w <- c(5/100, 10/100, 15/100, 30/100, 40/100)
  wmean <- calc(x=var_raster,fun=function(x){weighted.mean(x=x,w=w,na.rm=T)})
  writeRaster(x=wmean, filename=paste("D:/ccsosa/ISRIC/wMean_calculated/csv",var,"_02_apr_2014.tif",sep=""),format="GTiff",overwrite=F)
  return("Done!")
}



layers <- c("BLD","CEC","CLYPPT","ORCDRC","PHIHOX","SLTPPT","SNDPPT")


for(var in layers){
  
 out<-weighted_mean_raster(var) 
}


w_meanProcess <- function(ncpu){
  
  #layers <- c("BLD","CEC","CLYPPT","ORCDRC","PHIHOX","SLTPPT","SNDPPT")
  
  weight_wrapper <- function(i)
  {
    layer <- layers[i]
    out <- weighted_mean_raster(var=layer)
    return("Done!")
  }
  
  library(snowfall)
  sfInit(parallel=T, cpus=ncpu)
  
  sfExport("dir")
  sfExport("varList")
  sfExport("soilList")
  sfExport("layers")
  sfExport("weighted_mean_raster")
  sfExport("weight_wrapper")
  
  # Run the control function
  system.time(sfSapply(as.vector(1:length(layers)), weight_wrapper))
  
  # Stop the cluster
  sfStop()
  
  return("Done!!!")
  
}
w_meanProcess(ncpu=3)
