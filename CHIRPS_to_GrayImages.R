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

# Download CHIRPS
chrps <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05'
Day  <- dts[1]
Year <- lubridate::year(Day)
file <- paste0(chrps,'/',Year,'/chirps-v2.0.',gsub('-','.',Day,fixed=T),'.tif.gz')
Out  <- 'D:/download_test'; if(!dir.exists(Out)){dir.create(Out,F,T)}
dfile <- paste0(Out,'/',basename(file))

if(!file.exists(dfile)){
  tryCatch(expr = {
    utils::download.file(url = file, destfile = dfile)
  },
  error = function(e){
    cat(paste0(basename(file),' failed.\n'))
  })
}

R.utils::gunzip(dfile)

rfile <- gsub('.gz','',dfile,fixed = T)
r <- raster::raster(x = rfile)
x_pos <- raster::colFromX(object = r, x = crd$x)
y_pos <- raster::rowFromY(object = r, y = crd$y)
# Define 512 x 512 px region
ext <- raster::extent(r, y_pos-256, y_pos+255, x_pos-256, x_pos+255)
r <- raster::crop(x = r, y = ext)
r[r[] == -9999] <- NA
sts <- range(r[], na.rm = T)
r_nrm <- r/max(r[], na.rm = T)


aux <- system.file('extdata/parrots.png',package='imager')
img <- imager::load.image(aux) %>% imager::resize(512,512) %>% imager::grayscale(); rm(aux)
img[,,1,1] <- raster::as.matrix(r_nrm)

ifile <- gsub('.tif','.png',rfile,fixed = T)
save.Image <- function(im = img, file = ifile){
  im <- imager::imrotate(im, 90) %>% imager::mirror("x")
  dim(im) <- dim(im)[-3]
  im %>% png::writePNG(file)
}
save.Image(im = img, file = ifile)

file.remove(rfile)
