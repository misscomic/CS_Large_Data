install.packages("ElemStatLearn")
library("ElemStatLearn")
data(prostate)

# split dataset into training and testing portions so as to do cross-validation
#    later

prostateTrn <- prostate[ prostate$train, ] #with train = TRUE
str(prostateTrn)

prostateTst <- prostate[ !prostate$train, ] #with train = FALSE
str(prostateTst)


################################
###  add logical column    ###
################################
help(sample)
myTrain <- sample( c(TRUE,FALSE), 97, TRUE, prob = c(2/3, 1/3))
table(myTrain)
prostate$newTrain <- myTrain
str(prostate)


# why we are going to begin with  lcavol as our response variable

attach(prostateTrn)

hist(pgg45) #right scaled
sort(pgg45) #lots of zeros

hist(lcp)
sort(lcp) #-1.3863's are default values

hist(lcavol) #look better for linear regression
sort(lcavol)

# develop linear regression model for lacavol using training data

# first look at relationship between lcavol and each possible predictor

plot( lpsa, lcavol )

# add a  nonparametric smooth line
lines( lowess( lcavol ~ lpsa) )

# now fit the linear regression
lmLpsa <- lm( lcavol ~ lpsa )
summary(lmLpsa)

#####################
###     summary   ###
#####################
# b = 0.7543 > 0, positive relationship
# p-value =1.73e-12, strong evidence
# r-square = 53.75%, how much is explained by predictors


plot( lpsa, lcavol )
abline(lmLpsa) #a lot of noise around -> small R-square value
plot(lmLpsa)

# get a confidence interval for the population slope
confint(lmLpsa)


# prediction for new observations, including prediction intervals
# for single new observation with specified value of lpsa

newobs <- data.frame( lpsa = 2.0 )
predict( lmLpsa, newobs, interval = "predict" )

# for all the observations in the test dataset
predLpsa <- predict(lmLpsa, prostateTst, interval = "predict" ) 
predLpsa
cbind(prostateTst[,c("lpsa", "lcavol")], predLpsa)
     
# compare out-of-sample predictive ability with in-sample
# in order to compare with the R-square we got before
# 54.38% vs 53.7%(previous)
cor( predLpsa[,1], prostateTst$lcavol ) ^2


# now the same procedure for age as a predictor
plot( age, lcavol )
lines( lowess( lcavol ~ age) )

##  an outlier in the plot ( aged 40 w/ extremely high lcavol)


lmAge <- lm( lcavol ~ age )
summary(lmAge)
plot( lcavol, age)
abline(lmAge)
plot(lmAge)
confint(lmAge)


plot( age[-66], lcavol[-66] )
lmAgeNo66 <- lm( lcavol[-66] ~ age[-66] )
plot(lmAgeNo66)
confint(lmAgeNo66)

# and now gleason

plot( gleason, lcavol )
lines( lowess( lcavol ~ gleason) )
lmGleason <- lm( lcavol ~ gleason )
summary(lmGleason)
plot(lmGleason)
confint(lmGleason)


# multiple regression with all 3 predictors

lmAll <- lm( lcavol ~ age + lpsa + gleason )
summary(lmAll)
plot(lmAll)

# simplify model (parsimony!)

lmLpsaGleason <- lm(lcavol ~ lpsa + gleason)
summary(lmLpsaGleason)
plot(lmLpsaGleason)
confint(lmLpsaGleason)

predLpsaGleason <- predict(lmLpsaGleason, prostateTst, interval = "predict")
plot( predLpsaGleason[,1], prostateTst$lcavol)

# Linux demo of regression on larger dataset

a2008 <- read.csv("http://homepage.stat.uiowa.edu/~kcowles/Datasets/2008s.csv")

attach(a2008)
#plot( Distance, ActualElapsedTime)
lmTimeDist <- lm( ActualElapsedTime ~ Distance)
summary(lmTimeDist)
lmDepMonthDay <- lm(DepDelay ~ as.factor(Month) + as.factor(DayOfWeek)) # calculate each month seperately
summary(lmDepMonthDay)

# the r-sqare is only 0.014, very little statistical meaning.


