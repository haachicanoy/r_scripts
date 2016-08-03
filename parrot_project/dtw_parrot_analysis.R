# DTW practice with real data
# H. Achicanoy
# Home, 2016

setwd('C:/Users/haachicanoy/Desktop')
parrot <- read.csv('ParrotData.csv')

str(parrot)

parrot$Date_fixed <- as.POSIXct(parrot$Date)

library(ggplot2)
p <- ggplot(data=parrot[parrot$Bandeja!='Bandeja 4',], aes(x=Date_fixed, y=Temperature, colour=Bandeja)) + geom_line()
p + coord_cartesian(xlim=c(parrot$Date_fixed[1], parrot$Date_fixed[50]), ylim=c(20.5, 23.5)) # Zoom on lower bound
p + coord_cartesian(xlim=c(parrot$Date_fixed[50], parrot$Date_fixed[150]), ylim=c(22.5, 25)) # Zoom on lower bound
p + coord_cartesian(xlim=c(parrot$Date_fixed[1], parrot$Date_fixed[100]), ylim=c(30, 50)) # Zoom on upper bound

library(dplyr)
library(tidyr)

parrot2 <- parrot %>% spread(Date_fixed, Temperature)

# Así funciona file.copy
file.copy(from='C:/Users/haachicanoy/Documents/CIAT/Scripts_clima_old', to='C:/Users/haachicanoy/Desktop', overwrite=TRUE, recursive=TRUE, copy.mode=TRUE)
