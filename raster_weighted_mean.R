
# ISRIC soils rasters

dir <- "D:/ccsosa/Mikey_request/soils/IDW"

library(raster)

soilList <- list.files(dir, pattern=".tif$")
#soilList<-sub("_sd6_m_02_apr_2014.tif","",soilList)
soilList<-sub("bld_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("cec_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("clyppt_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("crfvol_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("ocstha_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("orcdrc_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("phihox_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("sltppt_sd6_m_02_apr_2014.tif",NA,soilList)
soilList<-sub("sndppt_sd6_m_02_apr_2014.tif",NA,soilList)

soilList<-soilList[which(!is.na(soilList))]




varList <- strsplit(soilList,split="_")
varList <- unlist(lapply(varList, function(x){x[1]}))
varList <- sort(unique(varList))


depth_var <- strsplit(soilList,split="_")
depth_var <- unlist(lapply(depth_var, function(x){x[2]}))
depth_var <- sort(unique(depth_var))


#temp_dir <- "D:/tmp"

weighted_mean_raster <- function(var,...){
  library(raster)
  #temp_dir <- "D:/tmp"
  #rasterOptions(tmpdir=temp_dir)
  var_raster <- soilList[grep(pattern=var,x=soilList)]
  #var_raster <- lapply(var_raster[-length(var_raster)], function(x){raster(paste(dir,"/",x,sep=""))})
  var_raster <- lapply(var_raster, function(x){raster(paste(dir,"/",x,sep=""))})
  
  #for(i in 1:length(var_raster)){
  #  var_raster[[i]][which(var_raster[[i]][]>=100)] <- NA
  #}; rm(i)
  var_raster <- stack(var_raster)
  w <- c(5/100, 10/100, 15/100, 30/100, 40/100)
  wmean <- calc(x=var_raster,fun=function(x){weighted.mean(x=x, w=w, na.rm=TRUE)})
  writeRaster(x=wmean, filename=paste("D:/ccsosa/Mikey_request/soils/wmean_calculated/",var,"_02_apr_2014.tif",sep=""), overwrite=F)
  return("Done!")
}
w_meanProcess <- function(ncpu){
  
  layers <- c("bld","cec","clyppt","crfvol","ocstha","orcdrc","phihox","sltppt","sndppt")
  
  weight_wrapper <- function(i)
  {
    layer <- layers[i]
    out <- weighted_mean_raster(var=layer)
    return("Done!")
  }
  
  library(snowfall)
  sfInit(parallel=T, cpus=ncpu)
  
  #sfExport("temp_dir")
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
w_meanProcess(ncpu=4)


# out_dir <- "D:/ccsosa/ISRIC/worldwide_wmean_calculated"
# 
# v1 <- raster(paste(out_dir,"/BLD_02_apr_2014.tif",sep=""))
# plot(v1)
# 
# hist(na.omit(v1[]))
# summary(na.omit(v1[]))
# 
# v2 <- raster(paste(out_dir,"/SNDPPT_02_apr_2014.tif",sep=""))
# plot(v2)
# v3 <- raster(paste(out_dir,"/CEC_02_apr_2014.tif",sep=""))
# plot(v3)
# v4 <- raster(paste(out_dir,"/SLTPPT_02_apr_2014.tif",sep=""))
# plot(v4)





