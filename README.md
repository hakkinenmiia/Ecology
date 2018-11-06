# Ecology
Tidal zone project

#####ECOLOGY#####

setwd("C:/R files/DATA")

periwinkles.random <- read.csv("Ecology_random.csv", sep = ";", dec = ".")

periwinkles.transect <- read.csv("Ecology_transect.csv", sep = ";", dec = ".")
#clean the table from unnecessary columns 
periwinkles.transect$X <- NULL
periwinkles.transect$X.1 <- NULL
periwinkles.transect$X.2 <- NULL
periwinkles.transect$X.3 <- NULL
periwinkles.transect$X.4 <- NULL

r.upper <- subset(periwinkles.random, subset=periwinkles.random$Tidal.zone == "Upper")
r.middle <- subset(periwinkles.random, subset=periwinkles.random$Tidal.zone == "Middle")
r.low <- subset(periwinkles.random, subset=periwinkles.random$Tidal.zone == "Low")

mean(upper$Periwinkles)
mean(middle$Periwinkles)
mean(low$Periwinkles)

t.upper <- subset(periwinkles.transect, subset=periwinkles.transect$Tidal.zone == "Upper")
t.middle <- subset(periwinkles.transect, subset=periwinkles.transect$Tidal.zone == "Middle")
t.low <- subset(periwinkles.transect, subset=periwinkles.transect$Tidal.zone == "Low")
