# Twitter analytics
# H. Achicanoy
# 2016

# load packages
library("twitteR")
library("wordcloud")
library("tm")

# setting twitter access
setup_twitter_oauth(consumer_key = '1yqEL959BLtogmhd3PoEcvqTn', consumer_secret = 'Sw06ImukaiVzEjKaIWBTjQ3Uv3yoH4gUTdRavPqtu3dO5OZbtl', access_token = '3353669056-RX5UIWVKoLmLNZS3XN5GEwbuGFdlFB2iMpmt7zE', access_secret = 'K6qx1ymJQfgGwK50DxVk8fU1pOYGliZvQdjEHFQoMDy7F')

# looking for hashtag of interest
r_brexit <- searchTwitter("#Brexit", n=3000) # cainfo="cacert.pem"

#### =============================================================== ###
#### Text analysis
#### =============================================================== ####

# save text
r_brexit_text <- sapply(r_brexit, function(x) x$getText())

# create corpus
r_brexit_text_corpus <- Corpus(VectorSource(r_brexit_text))

# clean up
r_brexit_text_corpus <- tm_map(r_brexit_text_corpus, removePunctuation)
r_brexit_text_corpus <- tm_map(r_brexit_text_corpus, content_transformer(tolower)) 
r_brexit_text_corpus <- tm_map(r_brexit_text_corpus, function(x) removeWords(x, stopwords()))

# plotting more frequent words
wordcloud(r_brexit_text_corpus)

#### ----

#### =============================================================== ###
#### Spatial analysis
#### =============================================================== ####

lucaspuente <- getUser("lucaspuente")
location(lucaspuente)

lucaspuente_follower_IDs<-lucaspuente$getFollowers(retryOnRateLimit=180)
length(lucaspuente_follower_IDs)


if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}
lucaspuente_followers_df = rbindlist(lapply(lucaspuente_follower_IDs,as.data.frame))
lucaspuente_followers_df<-subset(lucaspuente_followers_df, location!="")

lucaspuente_followers_df$location<-gsub("%", " ",lucaspuente_followers_df$location)

# if(source == "google"){
#   url_string <- paste("https://maps.googleapis.com/maps/api/geocode/json?address=", posturl, "&key=", api_key, sep = "")
# }

# Geocode Followers' Locations
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")

geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="[INSERT YOUR GOOGLE API KEY HERE]")
}

geocode_results<-sapply(lucaspuente_followers_df$location, geocode_apply, simplify = F)

length(geocode_results)

# cleaning geocode results
condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results<-geocode_results[condition_a]

condition_b <- lapply(geocode_results, lapply, length)
condition_b2<-sapply(condition_b, function(x) x["results"]=="1")
geocode_results<-geocode_results[condition_b2]
length(geocode_results)

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")

#### ----






fseason <- first_season[["P_95"]]; fseason$Season <- "First"
sseason <- second_season[["P_95"]]; sseason$Season <- "Second"
seasons <- rbind(fseason, sseason); rm(fseason, sseason)

library(dplyr)
seasons <- seasons %>% gather("Year", "P_95", 4:28) %>% arrange(cellID, Year)

library(ggplot2)

g1 <- ggplot(data = seasons[seasons$Season=="First",], aes(x=as.numeric(as.character(Year)), y=P_95, fill=as.factor(cellID))) + geom_line() # colour=as.factor(Season), 
g1 <- g1 + stat_summary(aes(colour="mean", shape="mean", group=1), fun.y=mean, geom="line", size=1.1)
g1 <- g1 + xlab("Years") + ylab("95 percentile of daily precipitation (mm/day)") + ggtitle("Embu - First season")
g1 <- g1 + theme_bw() + guides(stat_summary=FALSE)
g1

g2 <- ggplot(data = seasons[seasons$Season=="Second",], aes(x=as.numeric(as.character(Year)), y=P_95, fill=as.factor(cellID))) + geom_line() # colour=as.factor(Season), 
g2 <- g2 + stat_summary(aes(colour="mean", shape="mean", group=1), fun.y=mean, geom="line", size=1.1)
g2 <- g2 + xlab("Years") + ylab("95 percentile of daily precipitation (mm/day)") + ggtitle("Embu - Second season")
g2 <- g2 + theme_bw()
g2

library(data.table)

load('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Embu/prec/prec.RData')
all_prec <- chirps_year; rm(chirps_year)

year <- lapply(1:length(all_prec), function(i){
  
  prec_year <- as.data.frame(all_prec[[i]])
  x <- as.data.frame(t(colMeans(prec_year[,4:ncol(prec_year)])))
  x <- x[,1:365]
  return(x)
  
})

year_all <- do.call(rbind, year)

plot(as.numeric(colMeans(year_all)), ty='l')

all_prec

y1995 <- as.data.frame(all_prec[["y2006"]])
y1995$lon <- y1995$lat <- NULL

library(tidyr)
library(dplyr)

y1995 <- y1995 %>% gather(Date, Value, d1:d365)
y1995$cellID <- as.factor(y1995$cellID)

library(ggplot2)
library(viridis)
gg <- ggplot(y1995, aes(x=Date, y=cellID, fill=Value))
gg <- gg + geom_tile() # color=Value, size=10
gg <- gg + scale_fill_viridis(name="Precipitation")
gg <- gg + coord_equal(ratio = 1/5)
gg <- gg + labs(x=NULL, y=NULL, title="Precipitation for 2006")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0, size=17))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=2, angle=90))
gg <- gg + theme(axis.text.y=element_text(size=2))
gg <- gg + theme(legend.title=element_text(size=11, face='bold'))
gg <- gg + theme(legend.text=element_text(size=10))
gg <- gg + geom_vline(xintercept=182, colour=2)

load('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Embu/indx_fs_wet_days.RData')
wd_fseason <- indexs_wet_days; rm(indexs_wet_days)
wd_fseason1995 <- wd_fseason[["y2006"]]
mean(wd_fseason1995$d1); mean(wd_fseason1995$d100)

#gg <- gg + geom_vline(xintercept=min(wd_fseason1995$d1, na.rm = T), colour=5)
#gg <- gg + geom_vline(xintercept=max(wd_fseason1995$d100, na.rm = T), colour=5)

gg <- gg + geom_vline(xintercept=median(wd_fseason1995$d1, na.rm = T), colour=5)
gg <- gg + geom_vline(xintercept=median(wd_fseason1995$d100, na.rm = T), colour=5)

load('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Embu/indx_ss_wet_days.RData')
wd_sseason <- indexs_wet_days; rm(indexs_wet_days)
wd_sseason1995 <- wd_sseason[["y2006"]]

par(mfrow=c(1,2))
hist(wd_sseason1995$d1); hist(wd_sseason1995$d100)

mean(wd_sseason1995$d1); mean(wd_sseason1995$d100)

#gg <- gg + geom_vline(xintercept=min(wd_sseason1995$d1, na.rm = T), colour=5)
#gg <- gg + geom_vline(xintercept=max(wd_sseason1995$d100, na.rm = T), colour=5)

gg <- gg + geom_vline(xintercept=median(wd_sseason1995$d1, na.rm = T), colour=5)
gg <- gg + geom_vline(xintercept=median(wd_sseason1995$d100, na.rm = T), colour=5)

ggsave(filename='/home/hachicanoy/prec2006.pdf', plot=gg, width=15, height=5, units='in')



load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Embu/prec/prec_ss_wet_days.RData')
prec_year <- as.data.frame(chirps_wet_days[['y1996']])
prec_pixel <- unlist(lapply(1:nrow(prec_year), function(j){
  x <- as.numeric(quantile(as.numeric(prec_year[j, 4:ncol(prec_year)]), probs = 0.99, na.rm=TRUE))
  return(x)
}))
hist(prec_pixel, main = '99 percentile distribution by pixels in 1996', xlab = 'Precipitation (mm/day)', probability = TRUE, col='forestgreen')



rain <- county[county$Index=='TOTRAIN',]
rain[rain$RCP!="Historical",] %>% group_by(Season) %>% summarise(mean(Mean))
(165.8205 - 158.3221)/165.8205

# Makueni moisture stress

load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/future/first_season/bcc_csm1_1/2021_2045/rcp26/Makueni_first_season.RData')
first_season <- clim_indexes; rm(clim_indexes)

load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/future/second_season/bcc_csm1_1/2021_2045/rcp26/Makueni_second_season.RData')
second_season <- clim_indexes; rm(clim_indexes)

fseason <- first_season[["NDWS"]]; fseason$Season <- "First"
sseason <- second_season[["NDWS"]]; sseason$Season <- "Second"
seasons <- rbind(fseason, sseason); rm(fseason, sseason)

library(dplyr)
library(tidyr)
seasons <- seasons %>% gather("Year", "NDWS", 4:28) %>% arrange(cellID, Year)

library(ggplot2)

g1 <- ggplot(data = seasons[seasons$Season=="First",], aes(x=as.numeric(as.character(Year)), y=NDWS, fill=as.factor(cellID))) + geom_line() # colour=as.factor(Season), 
g1 <- g1 + stat_summary(aes(colour="mean", shape="mean", group=1), fun.y=mean, geom="line", size=1.1)
g1 <- g1 + xlab("Years") + ylab("Maximum number of moisture stress days") + ggtitle("Makueni - First season")
g1 <- g1 + theme_bw() + guides(stat_summary=FALSE)
g1

g2 <- ggplot(data = seasons[seasons$Season=="Second",], aes(x=as.numeric(as.character(Year)), y=NDWS, fill=as.factor(cellID))) + geom_line() # colour=as.factor(Season), 
g2 <- g2 + stat_summary(aes(colour="mean", shape="mean", group=1), fun.y=mean, geom="line", size=1.1)
g2 <- g2 + xlab("Years") + ylab("Maximum number of moisture stress days") + ggtitle("Makueni - Second season")
g2 <- g2 + theme_bw()
g2
