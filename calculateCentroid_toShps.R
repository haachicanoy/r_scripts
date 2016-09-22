
######################################################################################################################
#### Septiembre 2016 - Calcular el centroide a un listado de archivos shapefile
######################################################################################################################

library("rgdal"); library("mapdata")
library("raster"); library("plotKML")
library("maptools"); library("stringr")
library("randomForest"); library("rasterVis")
library("dismo"); library("rgeos")
library("SDMTools")
library("maps"); library("compare")

install.packages('spatstat')
install.packages('rgeos')
install.packages('rms')
install.packages('yaImpute')
install.packages('SpatialPack')

install.packages("spatialEco")
library(spatialEco)

path_shp <- "D:/CC/_points/_somes/Ghana/_shp/_polygons_geo"
listado  <- list.files(path_shp, full.names = T, pattern = ".shp$")
shapes   <- lapply(listado, FUN = shapefile)

#path_output <- ""

plot(shapes[[1]])

dim_mod <- function(x){ #funcion para observar las dimensiones del listdo de shapefile
  
  x <- x@data
  return(dim(x))
}

dim_shps <-do.call('rbind', lapply(shapes, function(x){dim_mod(x)})) #observar las dimensiones de cada shpe dentro del listado
class(dim_shps)
dim_shps <- as.data.frame(dim_shps)
names(dim_shps) <- c("rows", "columns")
head(dim_shps)

#centroids <- getSpPPolygonsLabptSlots(shp_co)
#points(centroids, pch = 3, col = "Red")

centroids       <- list()
tablas_centroids <- list()

for (i in 1:length(shapes)){
  
  centroids[[i]]               <- getSpPPolygonsLabptSlots(shapes[[i]])
  tablas_centroids[[i]]        <- as.data.frame(centroids[[i]])
  names(tablas_centroids[[i]]) <- c("Long", "Lat")
  
}

plot(tablas_centroids[[1]])

tablas_union <-do.call('rbind', tablas_centroids)
dim(tablas_union)
names(tablas_union)

tablas_union[, "Species"] <- "Cocoa"
tablas_order <- tablas_union[, c(3, 1, 2)] #cambio en el orden de las columnas

head(tablas_union)
head(tablas_order)

dupvector <- duplicated(tablas_order[,c("Long", "Lat")])

unique(dupvector) #si hay puntos repetidas

tablas_order_rmDup <- tablas_order[!dupvector,]

dim(tablas_order)
dim(tablas_order_rmDup)

write.csv(tablas_order, "D:/CC/_points/_somes/Ghana/presences_shpPolygons_rmDups.csv")


#----------------------------------------------------------------------------------------------------------------------------------------
# Union con tablas csv de la misma base de datos
#----------------------------------------------------------------------------------------------------------------------------------------

require(raster); require(rgdal)

alt <- raster("C:/Workspace/_raster/_altitude/srtm_v41_30s")
plot(alt)

tabla_centroids <- read.csv("D:/CC/_points/_somes/Ghana/presences_shpPolygons_rmDups.csv")
tabla_1         <- read.csv2("D:/CC/_points/_somes/Ghana/cdi_data.csv") #uso del read.csv 2 por la manera en la que estan los datos (. y ,)
tabla_2         <- read.csv2("D:/CC/_points/_somes/Ghana/Issaka_sites.csv") #uso del read.csv 2 por la manera en la que estan los datos (. y ,)

head(tabla_centroids)
head(tabla_1)
head(tabla_2)

# Tabla 1

tabla_1[, "Species"] <- "Cocoa" 
names(tabla_1)       <- c("ID", "Long", "Lat", "Species" )
tabla_1              <- tabla_1[,c(1, 4, 2, 3)]
summary(tabla_1$Long)
summary(tabla_1$Lat)

# Tabla 2

head(tabla_2)
tabla_2[, "Species"] <- "Cocoa" 
tabla_2              <- tabla_2[,c(1, 12, 2, 3)]
names(tabla_2)       <- c("ID", "Species", "Long", "Lat")

tabla_2$Long         <- as.numeric(as.character(tabla_2$Long))
tabla_2$Lat          <- as.numeric(as.character(tabla_2$Lat))
#str(tabla_2)  #mirar la clase de cada columna del data frame

# Union ambas tablas

tablas_join          <- rbind(tabla_1, tabla_2)
head(tablas_join)
dim(tabla_1); dim(tabla_2); dim(tablas_join)
class(tablas_join)
summary(tablas_join$Long); summary(tablas_join$Lat)

# Tabla Centroids 

head(tabla_centroids)
head(tablas_join)

names(tabla_centroids) <- c("ID", "Species", "Long", "Lat")
str(tablas_join)
str(tabla_centroids)

tabla_centroids$Species <- as.character(tabla_centroids)

# Union tablas join y tablas centroids

dim(tablas_join)
dim(tabla_centroids)

tabla_global          <- rbind(tablas_join, tabla_centroids) #other form: tabla_global <- do.call("rbind", c(tablas_join, tabla_centroids))
dim(tabla_global)
head(tabla_global)
str(tabla_global)

# Remove duplicated to table

dupvector_table        <- duplicated(tabla_global[,c("Long", "Lat")])
unique(dupvector_table)    #si hay puntos repetidas
tabla_global_rmDup     <- tabla_global[!dupvector_table,]

dim(tabla_global); dim(tabla_global_rmDup) #hay 618 presencias duplicados
head(tabla_global_rmDup)

# Extract altitude to table

Elevation            <- extract(alt, tabla_global_rmDup[, 3:4])
tabla_global_rmDupOk <- cbind(tabla_global_rmDup, Elevation)

head(tabla_global_rmDupOk)
dim(tabla_global_rmDupOk)

write.csv(tabla_global_rmDupOk, "D:/CC/_points/centroids.csv")

# Cortar las presencias a los países del shp countries

countries                         <- shapefile("C:/Workspace/_shp/_countries/all_countries.shp")
plot(countries)

shp_presences                     <- tabla_global_rmDupOk
coordinates(shp_presences)        <- ~ Long + Lat  
plot(shp_presences)

out <- gIntersection(shp_presences, countries, byid=TRUE)


shp_presences_cut                 <-crop(shp_presences, countries)
plot(countries)
plot(shp_presences_cut, add = T)

dim(shp_presences@data)
dim(shp_presences_cut@data)

# Identificar el país en el que se encuentran las presencias

gIntersects(pts, ply, byid=TRUE), 2, any)


writeOGR(shp_presences, dsn = "D:/CC/_points/_somes/Ghana/_shp/_pointsMerge", layer = "centroids_points", driver = "ESRI Shapefile")

proj4string(shp_presences)        <- CRS("+proj=longlat +datum=WGS84")
proj4string(countries)            <- CRS("+proj=longlat +datum=WGS84") 

head(shp_presences@data)
head(countries@data)

#shp_presences_2                   <- merge(shp_presences@data, coordenadas, by = c("Long", "Lat"))
#class(shp_presences_2)


new_shape <- point.in.poly(shp_presences, countries)



over1 <- over(shp_presences, countries)
over2 <- as.data.frame(over1)
head(over2)

class(over)


head(overlay_2)
class(overlay_2)

summary(shp_presences@data$ID)
summary(overlay$point.ID)

dim(shp_presences@data)
dim(overlay)
dim(overlay_2)


head(overlay)
head(shp_presences_2)
join_table <- merge(x = points, y = overlay, by.x = "POINTID", by.y = "point.ID")


head(overlay)
dim(tablas_join_ok)
class(shp_presences)


