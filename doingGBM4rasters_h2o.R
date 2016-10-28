# Prepare inputs layers
# H. Achicanoy
# CIAT, 2016

# set working directory
setwd('D:/Harold/_maps')

# load packages
suppressMessages(library(raster))
suppressMessages(library(rgdal))
suppressMessages(library(foreign))
suppressMessages(library(randomForest))

# load country's shapefile
countries <- shapefile('./world_shape/all_countries.shp')
#plot(countries)
#plot(countries[countries@data$COUNTRY=='Colombia',])
colombia <- countries[countries@data$COUNTRY=='United Kingdom',] # Colombia

# load population density rasters files
pop2010 <- raster('./world_popDensity2010/GL_E_ATOTPOPBT_2010_DENS.tif')
pop2010 <- raster::crop(pop2010, extent(colombia))
pop2010 <- raster::mask(x = pop2010, mask = colombia)

pop2020 <- raster('./world_popDensity2020/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2020.tif')
pop2020 <- raster::crop(pop2020, extent(colombia))
pop2020 <- raster::mask(x = pop2020, mask = colombia)

# calculate difference's raster
diff_pop <- pop2020 - pop2010
hist(diff_pop[]) # population will increase

# load city access raster file
access <- raster('./world_cityAccess/acc_50k')
access <- raster::crop(access, extent(colombia))
access <- raster::mask(x = access, mask = colombia)

# load urban presence raster file
urban <- raster('./world_cities/world_cities1km.tif')
urban <- raster::crop(urban, extent(colombia))
urban <- raster::mask(x = urban, mask = colombia)

# create Colombia rasterized file
col_ras  <- rasterize(x = colombia, y = urban)

all_data <- cbind(data.frame(cellID = 1:ncell(urban)), xyFromCell(object = urban, cell = 1:ncell(urban)))
all_data <- cbind(all_data,
                  extract(x = urban,   y = all_data[,c('x', 'y')]),
                  extract(x = pop2010, y = all_data[,c('x', 'y')]),
                  extract(x = access,  y = all_data[,c('x', 'y')]),
                  extract(x = col_ras, y = all_data[,c('x', 'y')]))
names(all_data)[4:7] <- c('urban', 'pop2010', 'access', 'firm_land')
all_data <- all_data[which(all_data$firm_land==1),]
all_data$urban[which(is.na(all_data$urban))] <- 0
all_data <- all_data[complete.cases(all_data),]
rownames(all_data) <- 1:nrow(all_data)

# train random forest model
all_data2 <- all_data
table(all_data2$urban)

set.seed(1235)
all_data2 <- all_data2[c(which(all_data$urban==1), sample(x = which(all_data$urban==0), size = 1*sum(all_data$urban==1), replace = FALSE)),]
rownames(all_data2) <- 1:nrow(all_data2)

set.seed(1235)
train <- sample(x = 1:nrow(all_data2), size = .8*nrow(all_data2), replace = FALSE)
test  <- setdiff(as.numeric(rownames(all_data2)), train)

##########################

all_data2$urban <- as.factor(all_data2$urban)

rf_urban <- randomForest(x = all_data2[train, c("pop2010", "access")],
                         y = all_data2[train, c("urban")],
                         xtest = all_data2[test, c("pop2010", "access")],
                         ytest = all_data2[test, c("urban")],
                         data = all_data2, importance = TRUE, keep.forest=TRUE)
prob <- predict(rf_urban, all_data2[train, c("pop2010", "access")], type="prob")

plot(all_data2[train, c("pop2010")], prob[,2], pch=20, xlab='Population density at 2010', ylab='Probability of being urban area')
plot(all_data2[train, c("access")], prob[,2], pch=20, xlab='Access to population', ylab='Probability of being urban area')

scatter.smooth(all_data2[train, c("pop2010")], prob[,2], col=2, pch=20, xlab='Population density at 2010', ylab='Probability of being urban area')
scatter.smooth(all_data2[train, c("access")], prob[,2], col=2, pch=20, xlab='Access to population', ylab='Probability of being urban area')

cor(all_data2[train, c("pop2010")], prob[,2], method = 'spearman')
cor(all_data2[train, c("access")], prob[,2], method = 'spearman')

library(ggplot2)
qplot(all_data2[train, c("pop2010")], prob[,2], geom='smooth', span = 0.5)
qplot(all_data2[train, c("access")], prob[,2], geom='smooth', span = 0.5)

urban_2020 <- col_ras
urban_2020[all_data2020$cellID] <- prob2020[,2]

plot(urban_2020, xlim=c(-10, 5), ylim=c(49, 61))
plot(colombia, add=TRUE)

urban_2020Bin <- urban_2020
urban_2020Bin[which(urban_2020Bin[] > .6)] <- 2
urban_2020Bin[which(urban_2020Bin[] <= .6)] <- 0

urbChange <- urban_2020Bin - urban
# Urban area 2020 - urban area 2010
# 2 - 0 = 2; means new urban area predicted
# 2 - 1 = 1; means actual urban area is preserved
# 0 - 0 = 0; non-urban area is preserved
# 0 - 1 = -1; means error of clasification

library(rasterVis)
levelplot(urbChange, col.regions=c('forestgreen', 'red', 'blue', 'gold'))

plot(urbChange)

plot(col_ras)

require(Deducer)
rocplot(fit_12)

#######

library(deepnet)
all_data2[,c("urban")] <- as.numeric(as.character(all_data2[,c("urban")]))
nn <- nn.train(x = as.matrix(all_data2[train, c("pop2010", "access")]), y = as.matrix(all_data2[train, c("urban")]), hidden = c(5))

##########################
### 2020

all_data2020 <- cbind(data.frame(cellID = 1:ncell(urban)), xyFromCell(object = urban, cell = 1:ncell(urban)))
all_data2020 <- cbind(all_data2020,
                      extract(x = urban,   y = all_data2020[,c('x', 'y')]),
                      extract(x = pop2010, y = all_data2020[,c('x', 'y')]),
                      extract(x = access,  y = all_data2020[,c('x', 'y')]),
                      extract(x = col_ras, y = all_data2020[,c('x', 'y')]))
names(all_data2020)[4:7] <- c('urban', 'pop2010', 'access', 'firm_land')
all_data2020 <- all_data2020[which(all_data2020$firm_land==1),]
all_data2020$urban[which(is.na(all_data2020$urban))] <- 0
all_data2020 <- all_data2020[complete.cases(all_data2020),]
rownames(all_data2020) <- 1:nrow(all_data2020)

library(h2o)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '4g')
h2o.clusterInfo()

all_data2$urban <- as.factor(all_data2$urban)
urbanArea.r <- all_data2
urbanArea.r$urban <-  as.factor(urbanArea.r$urban)
urbanArea.h2o <- as.h2o(urbanArea.r, destination_frame = "urbanArea.h2o")
class(urbanArea.h2o)

independent <- c("x", "y","pop2010", "access")
dependent   <- "urban"
model = h2o.gbm(y = dependent, x = independent, ntrees = 500, training_frame = urbanArea.h2o, distribution="bernoulli")
summary(model)
plot(model)
plot(model@model$training_metrics@metrics$max_criteria_and_metric_scores$threshold)
thresholds <- model@model$training_metrics@metrics$max_criteria_and_metric_scores$threshold
median(thresholds)

all_data2020$urban <- as.factor(all_data2020$urban)
urbanArea2020.r   <- all_data2020
urbanArea2020.r$urban <- NULL
urbanArea2020.h2o <- as.h2o(urbanArea2020.r, destination_frame = "urbanArea2020.h2o")

pred <- h2o.predict(model, urbanArea2020.h2o)
pred.r <- as.data.frame(pred)

urban_2020 <- col_ras
urban_2020[all_data2020$cellID] <- pred.r$p1

plot(urban_2020, xlim=c(-10, 5), ylim=c(49, 62))
plot(colombia, add=TRUE)

urban_2020Bin <- urban_2020
urban_2020Bin[which(urban_2020Bin[] > 0.05)] <- 2
urban_2020Bin[which(urban_2020Bin[] <= 0.05)] <- 0

plot(urban_2020Bin, xlim=c(-10, 5), ylim=c(49, 62))
plot(colombia, add=TRUE)

urbChange <- urban_2020Bin - urban
plot(urbChange, xlim=c(-10, 5), ylim=c(49, 62))
plot(colombia, add=TRUE)

writeRaster(urbChange, filename='uk_urban2020.tif', format="GTiff", overwrite=TRUE)
# Urban area 2020 - urban area 2010
# 2 - 0 = 2; means new urban area predicted
# 2 - 1 = 1; means actual urban area is preserved
# 0 - 0 = 0; non-urban area is preserved
# 0 - 1 = -1; means error of clasification
