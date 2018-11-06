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

mean(r.upper$Periwinkles)
mean(r.middle$Periwinkles)
mean(r.low$Periwinkles)

mean(periwinkles.random$Periwinkles)

t.upper <- subset(periwinkles.transect, subset=periwinkles.transect$Tidal.zone == "Upper")
t.middle <- subset(periwinkles.transect, subset=periwinkles.transect$Tidal.zone == "Middle")
t.low <- subset(periwinkles.transect, subset=periwinkles.transect$Tidal.zone == "Low")

plot(periwinkles.random$Periwinkles, periwinkles.random$Tidal.zone)
plot(periwinkles.transect$Periwinkles, periwinkles.transect$Tidal.zone)



#Test to see if there is a difference between tidal zones: 
#random
m1 <- glm(periwinkles.random$Periwinkles ~ periwinkles.random$Tidal.zone)
shapiro.test(resid(m1)) 
#p-value = 0.001231 -> nonparametric
hist(periwinkles.random$Periwinkles)

#transect 
m2 <- glm(periwinkles.transect$Periwinkles ~ periwinkles.transect$Tidal.zone)
shapiro.test(resid(m2))  
#p-value = 0.03188 -> nonparametric
hist(periwinkles.transect$Periwinkles) 


poisson.test
qpois(periwinkles.random$Periwinkles, lambda = "7", lower.tail = TRUE, log.p = FALSE)


#Test for trend: there are more periwinkles in higher tidal zone than lower

#Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical 
#without assuming them to follow the normal distribution.
kruskal.test(data=periwinkles.random, periwinkles.random$Periwinkles ~ periwinkles.random$Tidal.zone)
#Kruskal-Wallis chi-squared = 5.6609, df = 2, p-value = 0.05899



#The frequency distribution of a data variable is a summary of 
#the data occurrence in a collection of non-overlapping categories.
range(periwinkles.random$Periwinkles) #gives: 0 39
breaks = seq(0, 39, by=5)
periwinkles.cut = cut(periwinkles.random$Periwinkles, breaks, right=FALSE)
periwinkles.cut
periwinkles.freq = table(periwinkles.cut)
periwinkles.freq
cbind(periwinkles.freq)

