## Set WD

setwd("C:/Users/jaume/Desktop/PhD/Courses/Stats course")

## Load data

dat = read.csv("butterflies.csv")


## Load library in case of ggplot use

library(tidyverse)


## Making boxplots to estimate a normal distribution

dat|>
ggplot(mapping=aes(x=LarvalHost, y=AdultWeight, colour = MaternalHost))+
  geom_boxplot()+
  labs(x="Larval host")+
  labs(y="Adult weight (g)")+
  theme_classic()


## Calculating mean and standard error for all groups

dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)

ses = tapply(dat$AdultWeight, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))

## Dot/line plot of the means to show approximate effect of host on weight

plot(c(0.97, 1.03), means[,1], ylim=c(40, 80), xlim=c(0.8, 2.2),
     xlab="Larval host", 
     ylab="Adult weight (g)",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c(expression(paste(italic(Barbarea))),
                      expression(paste(italic(Berteroa)))))

arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03), 
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03), 
       means[,2]+ses[,2], length=0.05, angle=90, code=3)

segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])

points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")

legend("topright", c("Maternal host", 
                    expression(paste(italic(Barbarea))),
                    expression(paste(italic(Berteroa)))), 
       bty="n", pch=c(NA,21,16))

## Anova analysis 

m = lm(AdultWeight~MaternalHost*LarvalHost, data=dat)
anova(m)

summary(m)

colMeans(means)
rowMeans(means)

