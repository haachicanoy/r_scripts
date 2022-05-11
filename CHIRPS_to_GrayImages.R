# CHIRPS to gray scale images centered on Colombia
# By: H. Achicanoy, 2022

# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
.rs.restartR()                      # Restart R session
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
dts <- seq(from = ini, to = end, by = 'day'); rm(ini, end)

# Output directory
Out  <- 'D:/download_test'; if(!dir.exists(Out)){dir.create(Out,F,T)}

# Main function
chirps2gray <- function(date = dts[1]){
  # CHIRPS base URL
  chrps <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05'
  # Get day and year
  Day  <- date
  Year <- lubridate::year(Day)
  # Target file
  tfile <- paste0(chrps,'/',Year,'/chirps-v2.0.',gsub('-','.',Day,fixed=T),'.tif.gz')
  # Destination file
  dfile <- paste0(Out,'/',basename(tfile))
  # Raster file
  rfile <- gsub('.gz','',dfile,fixed = T)
  # Image file
  ifile <- gsub('.tif','.png',rfile,fixed = T)
  
  if(!file.exists(ifile)){
    
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
    r <- raster::raster(x = rfile)
    # Centroid
    x_pos <- raster::colFromX(object = r, x = crd$x)
    y_pos <- raster::rowFromY(object = r, y = crd$y)
    # Associated extent
    ext <- raster::extent(r, y_pos-256, y_pos+255, x_pos-256, x_pos+255)
    # Cropping the raster
    r <- raster::crop(x = r, y = ext)
    # Assigning NAs
    r[r[] == -9999] <- NA
    sts <- range(r[], na.rm = T) # Important to use in any moment
    # Normalizing raster values between 0 and 1
    r_nrm <- r/max(r[], na.rm = T)
    
    # Read auxiliary image
    aux <- system.file('extdata/parrots.png',package='imager')
    img <- imager::load.image(aux) %>% imager::resize(512,512) %>% imager::grayscale(); rm(aux)
    # Assigning raster values to gray image format
    img[,,1,1] <- raster::as.matrix(r_nrm)
    
    # Save the result
    save.Image <- function(im = img, file = ifile){
      im <- imager::imrotate(im, 90) %>% imager::mirror("x")
      dim(im) <- dim(im)[-3]
      im %>% png::writePNG(file)
    }
    save.Image(im = img, file = ifile)
    file.remove(rfile)
    return(cat(paste0('Image ',basename(ifile),' processed correctly!!!\n')))
  } else {
    return(cat(paste0('Image ',basename(ifile),' already exists!\n')))
  }
  
}

# Loop through the dates
dts %>% purrr::map(.f = chirps2gray)
