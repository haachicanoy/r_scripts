# Get monthly CHIRPS for one country
# By: H. Achicanoy, 2022

# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, geodata, terra, imager, lubridate))

# Get spatial coordinates
iso <- 'COL'
shp <- geodata::gadm(country = iso, level = 0, path = base::tempdir())
crd <- terra::centroids(x = shp) %>% terra::crds() %>% base::as.data.frame()

# Time frame
ini <- as.Date('1981-01-01')
end <- as.Date('2021-12-31')
dts <- seq(from = ini, to = end, by = 'month'); rm(ini, end)
dts <- as.character(dts)
dts <- substr(x = dts, start = 1, stop = 7)

# Output directory
Out  <- 'D:/download_victor'; if(!dir.exists(Out)){dir.create(Out,F,T)}

# Main function
get_chirps <- function(date = dts[1]){
  # CHIRPS base URL
  chrps <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/'
  # Target file
  tfile <- paste0(chrps,'chirps-v2.0.',gsub('-','.',date,fixed=T),'.tif.gz')
  # Destination file
  dfile <- paste0(Out,'/',basename(tfile))
  # Raster file
  rfile <- gsub('.gz','',dfile,fixed = T)
  
  if(!file.exists(rfile)){
    
    # Downloading
    if(!file.exists(dfile)){
      tryCatch(expr = {
        utils::download.file(url = tfile, destfile = dfile)
      },
      error = function(e){
        cat(paste0(basename(tfile),' failed.\n'))
      })
    }
    
    # Unzip
    R.utils::gunzip(dfile)
    
    # Identify 512 x 512 pixels region
    r <- terra::rast(x = rfile)
    r <- r %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
    r[r == -9999] <- NA
    terra::writeRaster(x = r, filename = rfile, overwrite = T)
    return(cat(paste0('Raster ',basename(rfile),' processed correctly!!!\n')))
  } else {
    return(cat(paste0('Raster ',basename(rfile),' already exists!\n')))
  }
  
}

# Loop through the dates
dts %>% purrr::map(.f = get_chirps)