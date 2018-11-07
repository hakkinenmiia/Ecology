#####ECOLOGY#####

setwd("C:/R files/DATA")
library(stats)
library(effects)
library(doBy)

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
#rename variables - easier 
random.counts <- periwinkles.random$Periwinkles
transect.counts <- periwinkles.transect$Periwinkles
zones.random <- periwinkles.random$Tidal.zone
zones.transect <- periwinkles.transect$Tidal.zone

par(mfrow=c(1,2))
plot(random.counts, zones.random, xlim = c(0,50), ylim = c(0,5))
plot(transect.counts, zones.transect, xlim = c(0,50), ylim = c(0,5))



#Test to see if there is a difference between tidal zones: 
#random
m1 <- glm(random.counts ~ zones.random)
shapiro.test(resid(m1))
#p-value = 0.001231 -> nonparametric
hist(random.counts, xlab = "Count of periwinkles - random")

#transect 
m2 <- glm(transect.counts ~ zones.transect)
shapiro.test(resid(m2))
#p-value = 0.03188 -> nonparametric
hist(transect.counts, xlab="Count of periwinkles - transect") 


poisson.test
qpois(periwinkles.random$Periwinkles, lambda = "7", lower.tail = TRUE, log.p = FALSE)


#Test for trend: there are more periwinkles in higher tidal zone than lower

#Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical 
#without assuming them to follow the normal distribution.

kruskal.test(data=periwinkles.random, random.counts ~ zones.random)

#Kruskal-Wallis chi-squared = 5.6609, df = 2, p-value = 0.05899


#The frequency distribution of a data variable is a summary of 
#the data occurrence in a collection of non-overlapping categories.
range(periwinkles.random$Periwinkles) #gives: 0 39
breaks = seq(0, 39, by=5)
periwinkles.cut = cut(random.counts, breaks, right=FALSE)
periwinkles.cut
periwinkles.freq = table(periwinkles.cut)
periwinkles.freq
cbind(periwinkles.freq)

#simple linear regression
par(mfrow=c(1,2))
#random sampling
fit1=lm(random.counts ~ zones.random, data=periwinkles.random)
summary(fit1) 

plot(effect(fit1,term="zones.random",confidence.level = 0.95,partial.residuals=TRUE),band.colors="grey3", rug=F,residuals.color=adjustcolor("blue",alpha.f=0.2),residuals.pch=16,smooth.residuals=FALSE)

#transect lines
fit2=lm(transect.counts~zones.transect, data=periwinkles.transect)
summary(fit2)
plot(effect(fit2,term="zones.transect",confidence.level = 0.95,partial.residuals=TRUE),band.colors="grey3", rug=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2),residuals.pch=16,smooth.residuals=FALSE)

#ANOVA analysis
summaryBy(Periwinkles~Tidal.zone, data=periwinkles.random, FUN=c(mean,sd,length))
boxplot(Periwinkles~Tidal.zone, data=periwinkles.random, ylab="Periwinkles count")

fit3=aov(Periwinkles~Tidal.zone, data=periwinkles.random)
summary.lm(fit3) 
summary(fit3)
