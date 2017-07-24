install.packages("ElemStatLearn")
library(ElemStatLearn)

# help(package='ElemStatLearn')

data(prostate)
head(prostate)
# Q: why the lbph are the same? A: add 0.25 to the scales to be the default value.
exp(-1.386294)#=0.25

fix(prostate)
summary(prostate)

# plot variables one at a time
par(mfrow=c(3,3))
for(i in 1:(ncol(prostate)-1)){
  hist(prostate[,i], main = colnames(prostate)[i])
}

# scatterplots of pairs of variables
plot(prostate)

# representing more than 2 variables at a time
install.packages("rgl")   # won't work if OpenGL isn't installed
library(rgl)
attach(prostate)
plot3d( lpsa, gleason, pgg45, col = "blue")

install.packages("lattice")
library(lattice)
print(cloud( pgg45 ~ lpsa * gleason, data = prostate, groups=svi ))

 print(cloud( pgg45 ~ lpsa * gleason, data = prostate, groups=svi,
    screen = list(z=20, x=-70, y=0)))

print(cloud( pgg45 ~ lpsa * gleason, data = prostate, groups=svi,
    screen = list(z=20, x=-90, y=60)))

print(cloud( pgg45 ~ lpsa * gleason, data = prostate, groups=svi,
    screen = list(z=20, x=-90, y=-20)))

plot(age,pgg45)
plot(age,svi)
plot(age,lcp)
# conclusion, age may not be affective.

# explore a larger dataset
# http://stat-computing.org/dataexpo/2009/

a2008 <- read.csv("http://homepage.stat.uiowa.edu/~kcowles/Datasets/2008s.csv")
str(a2008)
summary(a2008)

attach(a2008)
plot(Month, ArrDelay)
boxplot(ArrDelay ~ Month)

# working with a random sample from the data

set.seed(15)
indices <- sample( nrow(a2008), 500, replace = FALSE)
small2008 <- a2008[indices, ]
plot(small2008)


