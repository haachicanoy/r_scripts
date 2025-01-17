options(warn = -1, scipen = 999)
library(pacman)
pacman::p_load(raster,stringr,terra,tidyverse,ncdf4,sp,sf,geodata,furrr,future)

# # Cargar archivos DEM, shapefile y de referencia
# # para realizar los recortes del area de interes
# ind <- geodata::gadm('IND', level = 1, path = tempfile())
# bihar <- ind[grepl("Bihar", ind$NAME_1)]
# terra::writeVector(bihar, "//Catalogue/cicap/1.Data/IND_initiative/_GLOBAL/_shps/Bihar.shp")
# elev <-  terra::rast("C:/Users/acmendez/Downloads/elevation_30s.tif")
# elev_cropped <- terra::crop(elev, terra::ext(bihar))
# elev_cropped <- terra::mask(elev_cropped, bihar)
# msk <- terra::rast(ruta_referencia_5km)
# msk_cropped <- terra::crop(msk, bihar)
# msk_cropped <- terra::mask(msk_cropped, bihar)
# msk_cropped[!is.na(msk_cropped)] <- 1
# writeRaster(msk_cropped, "//Catalogue/cicap/1.Data/IND_initiative/_GLOBAL/mask_5km.tif")
# elev_5km <- terra::resample(elev_cropped, msk_cropped, method = "bilinear")
# terra::writeRaster(elev_5km, "//Catalogue/cicap/1.Data/IND_initiative/_GLOBAL/STRM_elevation_5km2.tif", overwrite = T)
# #####################################################
# 
# get_dates_ERA5 <- function(file_name){
#   dt <- stringr::str_extract(file_name, "[0-9]{8}")
#   year <- substring(dt, 1, 4)
#   mnt  <- substring(dt, 5, 6)
#   dy   <- substring(dt, 7, 8)
#   to_ret <- paste(year, mnt, dy, sep = "-")
#   return(to_ret)
# }
# get_dates_CHIRPS <- function(file_name){
#   dt <- stringr::str_extract(file_name, "[0-9]{4}.[0-9]{2}.[0-9]{2}")
#   dt <- stringr::str_replace_all(dt, "\\.", "")
#   year <- substring(dt, 1, 4)
#   mnt  <- substring(dt, 5, 6)
#   dy   <- substring(dt, 7, 8)
#   to_ret <- paste(year, mnt, dy, sep = "-")
#   return(to_ret)
# }
# DEM_ruta = "//Catalogue/cicap/1.Data/IND_initiative/_GLOBAL/STRM_elevation_5km.tif"
# DEM =  terra::rast(DEM_ruta)
# # Cargar shapefile con los departamentos de Colombia
# ruta_shapefile <- "//Catalogue/cicap/1.Data/IND_initiative/_GLOBAL/_shps/Bihar.shp"
# shp <- terra::wrap(terra::vect(ruta_shapefile))
# # Cargar archivo de referencia con resolucion a 5 km
# ruta_referencia_5km <- "//Catalogue/cicap/1.Data/IND_initiative/_GLOBAL/mask_5km.tif"
# reference_5km <- terra::wrap(terra::rast(ruta_referencia_5km))
# root <- "//alliancedfs.alliance.cgiar.org/data_cluster17/observed/gridded_products/AgERA5_V1.1"
# out_dir <- "//catalogue/cicap/1.Data/IND_initiative/SPEI/00_ERA5"
# vars <- c('2m_Maximum Temperature', '2m_Minimum Temperature', '10m_Wind Speed', 'Relative Humidity', "Solar Radiation")
# av_yrs <- lapply(vars, function(vr){
#   yrs <- list.dirs(file.path(root, vr), recursive = F, full.names = F)
#   stopifnot("Na found" = any(!is.na(yrs)))
#   return(as.numeric(yrs))
# })
# names(av_yrs) <- vars
# stopifnot("different years " = length(unique(sapply(av_yrs, length))) == 1 )
# yrs <- unique(unlist(av_yrs))
# 
# plan(multisession, workers = 44)
# for(vrs in vars){
#   cat(">>> Resampling, cropping and masikng: ", vrs, "\n")
#   aa = furrr::future_map(.x = yrs, .f = function(.x){
#     #cat(.x, "\n")
#     shp_u <- terra::unwrap(shp)
#     msk_u <- terra::unwrap(reference_5km)
#     av_fls <- list.files(file.path(root, vrs, .x), pattern = ".nc$", full.names = T)
#     #deal with invalid files
#     if(.x == 2006 & vrs == 'Relative Humidity'){
#       pos = which(basename(av_fls) == "Relative-Humidity-2m-12h_C3S-glob-agric_AgERA5_20060814_final-v1.1.nc")
#       av_fls[pos] = av_fls[ifelse(pos > 1, pos-1, pos +1)]
#     } else if(.x == 1981 & vrs == 'Solar Radiation'){
#       pos = which(basename(av_fls) == "Solar-Radiation-Flux_C3S-glob-agric_AgERA5_19810407_final-v1.1.nc")
#       av_fls[pos] = av_fls[ifelse(pos > 1, pos-1, pos +1)]
#     }
#     stk <- terra::rast(av_fls)
#     stk <- terra::crop(stk, shp_u)
#     stk <- terra::mask(stk, shp_u)
#     stk <- terra::resample(stk, msk_u, method = 'bilinear')
#     if(.x == 2006 & vrs == 'Relative Humidity'){
#       av_fls[pos] = "Relative-Humidity-2m-12h_C3S-glob-agric_AgERA5_20060814_final-v1.1.nc"
#     } else if(.x == 1981 & vrs == 'Solar Radiation'){
#       av_fls[pos] = "Solar-Radiation-Flux_C3S-glob-agric_AgERA5_19810407_final-v1.1.nc"
#     }
#     dts <- get_dates_ERA5(av_fls)
#     fl_nms <- paste0(vrs, "_", dts)
#     names(stk) <- fl_nms
#     stopifnot("different extents " = terra::ext(stk) == terra::ext(msk_u))
#     out <- file.path(out_dir, vrs)
#     dir.create(out, showWarnings = F)
#     terra::writeRaster(stk, file.path(out, paste0(vrs, "_", .x, ".tif")), overwrite = T)
#     return(length(av_fls))
#   }, .progress = T)}
# plan(sequential)

shp <- terra::wrap(terra::vect(ruta_shapefile))
reference_5km <- terra::wrap(terra::rast(ruta_referencia_5km))

plan(multisession, workers = 44)
for(vrs in vars){
  cat(">>> Resampling, cropping and masikng: ", vrs, "\n")
  aa = furrr::future_map(.x = yrs, .f = function(.x){
    #cat(.x, "\n")
    shp_u <- terra::unwrap(shp)
    msk_u <- terra::unwrap(reference_5km)
    av_fls <- list.files ...
    stk <- terra::rast(av_fls)
    stk <- terra::crop(stk, shp_u)
    stk <- terra::mask(stk, shp_u)
    stk <- terra::resample(stk, msk_u, method = 'bilinear')
    out <- file.path(out_dir, vrs)
    dir.create(out, showWarnings = F)
    terra::writeRaster(stk, file.path(out, paste0(vrs, "_", .x, ".tif")), overwrite = T)
    return(length(av_fls))
  }, .progress = T)}
plan(sequential)