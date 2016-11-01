# Hyperspectral imaging analysis
# H. Achicanoy, 2016

library(hyperSpec)
library(fBasics)
library(RColorBrewer)

# Data import
chondro <- scan.txt.Renishaw ("C:/Users/haachicanoy/Downloads/rawdata/chondro.txt", data = "xyspc")
chondro

plot(chondro, "spcprctl5")
plotmap(chondro, func.args = list (na.rm = TRUE), col.regions = brewer.pal(20, "Greens"))

# Spectral smoothing
chondro <- spc.loess (chondro, seq (602, 1800, 4))
chondro
spectra.to.save <- chondro

# Baseline correction
baselines <- spc.fit.poly.below (chondro)
chondro <- chondro - baselines

# Normalization
chondro <- sweep (chondro, 1, apply (chondro, 1, mean), "/")
plot (chondro, "spcprctl5")

# Subtracting the Overall Composition
chondro <- sweep (chondro, 2, apply (chondro, 2, quantile, 0.05), "-")
plot (chondro, "spcprctl5")

# Outlier Removal by Principal Component Analysis (PCA)
pca <- prcomp (~ spc, data = chondro$., center = TRUE)
scores <- decomposition (chondro, pca$x, label.wavelength = "PC", label.spc = "score / a.u.")
loadings <- decomposition (chondro, t(pca$rotation), scores = FALSE, label.spc = "loading I / a.u.")

pairs (scores [[,,1:20]], pch = 19, cex = 0.5)

out <- map.identify (scores [,,5])
out <- c (out, map.identify (scores [,,6]))
out <- c (out, map.identify (scores [,,7])) ##

# ============================================================================================================= #
# Data source:  6-band Landsat 7 image (path 7 row 57) taken in 2000
# ============================================================================================================= #

# R options
options(warn = -1)
options(scipen = 999)

# load packages
suppressMessages(library(raster))
suppressMessages(library(rgdal))
suppressMessages(library(RSAGA))
suppressMessages(library(RStoolbox))

# set the working directory
setwd("D:/Harold/_maps/landsat/images/2000")

# get absolute meta data files
mtlList <- list.files(path = './', pattern = '_MTL.txt$', full.names = T, recursive = T)
rsNames <- list.dirs(path = './', full.names = F, recursive = F)
rsNames <- rsNames[grep(pattern = '^LE', rsNames)]

# preprocessing: reflectance correction
lapply(1:length(mtlList), function(i){
  
  mtlFile  <- mtlList[i]
  metaData <- RStoolbox::readMeta(mtlFile) # read meta data file
  lsat     <- RStoolbox::stackMeta(mtlFile) # load all bands
  lsat_ref <- RStoolbox::radCor(img = lsat, metaData = metaData, method = "apref") # reflectance correction
  lsat_ref <- raster::unstack(lsat_ref)
  lapply(1:length(lsat_ref), function(j){ # save individual rasters
    raster::writeRaster(lsat_ref[[j]], filename = paste('./', rsNames[i], '/', names(lsat)[j], '_ref.tif', sep = ''), format = "GTiff", overwrite = TRUE)
  })
  
})

# ============================================================================================================= #
# Image classification with Random Forest
# ============================================================================================================= #

# R options
options(warn = -1)
options(scipen = 999)

# load packages
suppressMessages(library(rgdal))
suppressMessages(library(raster))
suppressMessages(library(caret))

imgList <- list.files(path = 'D:/Harold/_maps/landsat/images/2000/LE70070572000044EDC00', full.names = T)
grep2 <- Vectorize(FUN = grep, vectorize.args = 'pattern')
imgList <- imgList[grep2(pattern = paste("/B", c(1:5, 7), '_dn_ref.tif', sep = ''), x = imgList)]

img <- raster::stack(imgList); rm(imgList)
names(img) <- c(paste0("B", 1:5, coll = ""), "B7")

plotRGB(img * (img >= 0), r = 4, g = 5, b = 3, scale = 10000)

