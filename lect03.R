library(MASS)
library(ElemStatLearn)
data(prostate)
library(class)
library(e1071)

# how to randomly split a dataset

split <- function( df, seed, fractrain )
{
  #df -- data frame to split
  #seed -- random number seed for splitting
  #fractrain -- proportion of rows desired in training dataset
  # returns logical vector "newtrain" with TRUE for training dataset 
        #and FALSE for test
  numrows <- nrow( df )
  numtrain <- round( fractrain * numrows, 0)
  set.seed(seed)
  index <- sample( numrows, numtrain, replace = FALSE)
  newtrain <- rep(TRUE, numrows)
  newtrain[ -index] <- FALSE
  newtrain
}  
            
# split dataset randomly into 2/3 training and 1/3 testing
mysplit <- split( prostate, 1013, 2/3)
mysplit

prostate$mysplit <- mysplit
# we wish to predict "svi" -- seminal vesicle invasion 
# binary outcome variable coded
#            0 -- no svi
#            1 -- cancer has spread from prostate into seminal vesicle

# classification problem 
#        logistic regression
#        K nearest neighbors
#        naive Bayes

prostateTrn <- prostate[ prostate$train, ]
prostateTst <- prostate[ !prostate$train, ]

# logistic regression
# generalize linear model

glmout <- glm( svi ~ age + gleason + lpsa, data = prostateTrn, 
     family = binomial(link="logit"))

summary(glmout)


##### only gleason and lpsa
glmout2 <- glm( svi ~  gleason + lpsa, data = prostateTrn, 
     family = binomial(link="logit"))

summary(glmout2)
confint(glmout2)
## gleason: [0.102, 2.637]
## lpsa: [1.366, 4.775]
## wide intervals

# odds ratios

exp(cbind( glmout2$coefficients, confint(glmout2)))

### prediction
predictGlmIn <- predict( glmout2, prostateTrn, type = "response" )

# how well do we predict in-sample if we use 0.5 as cutoff?
table( predictGlmIn > 0.5, prostateTrn$svi )

# could adjust cutoff if false positives were worse than false negatives
#        or vice versa

# how about out-of-sample prediction?
predictGlmOut <- predict( glmout2, prostateTst, type="response")

table( predictGlmOut > 0.7, prostateTst$svi )

### 0.5 is uaually a cut-off if the false positive is worse than false negative

(47+9)/(47+6+9+5)
#### = 83.58% prcision

table(prostateTrn$svi)
###   0  1
###   52 15
52/67


# k nearest neighbors
# using knn function in package "class"

# must standardize predictor variables so that "distance" means the same
#    thing for each of them
standardize <- function(v) { (v - mean(v)) / sd(v) }
Xtrn <- apply( prostateTrn[ ,c("age", "gleason", "lpsa")]  , 2, standardize)
Xtst <- apply( prostateTst[ ,c("age", "gleason", "lpsa")]  , 2, standardize)

preds <- knn( Xtrn, Xtst, prostateTrn$svi, k = 5)   ## how many: 5
table(preds, prostateTst$svi)

for( k in 1:10) {
   preds <- knn( Xtrn, Xtst, prostateTrn$svi, k = k)
   print(table(preds, prostateTst$svi))
}

# naive Bayes

### call factor
naiveBayesOut <- naiveBayes( factor(svi) ~ age + gleason + lpsa, data = prostateTrn)

predsNB <- predict( naiveBayesOut, prostateTst, type = "class")

predsNB
## give us the predictions
table(predsNB, prostateTst$svi)


predsNB2 <- predict( naiveBayesOut, prostateTst, type = "raw")
predsNB2

# another example dataset
# description at http://www.stat.uiowa.edu/~kcowles/Datasets/income.info

income <-
  read.table("http://www.stat.uiowa.edu/~kcowles/Datasets/income.dat",
  header = TRUE)

str(income)

# tell R that SEX and RACE are nominal variables
income$SEX <- factor(income$SEX)
income$RACE <- factor(income$RACE)
levels(income$RACE) <- c("white", "black", "Asian")

#
income$EDUCATION <- ordered(income$EDUCATION)

 # suppose we wanted to try to predict EDUCATION level using other available
#         variables

# let's split dataset into training and testing

mysplit <- split(income, 27, 1/16)

income$MYSPLIT <- mysplit
income$logINCOME <- log(income$INCOME+1)

# ordinal logistic regression usig "polr" function in MASS package
polrout <- polr( EDUCATION ~ SEX + logINCOME + RACE + AGE, data = 
       income, subset = MYSPLIT, method = "logistic")

newpreds <- predict( polrout, income[1:10,])









