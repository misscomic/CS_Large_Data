davis2 <- read.table("http://www.stat.uiowa.edu/~kcowles/Datasets/Davis2.txt",header=TRUE)
str(davis2)

davis2$gender[davis2$sex=="M"] <- 1
davis2$gender[davis2$sex=="F"] <- 2

# frequenct plots
par(mfrow=c(2,3))
for(i in 3:(ncol(davis2))){ hist(davis2[,i], main = colnames(davis2)[i]) }

# scatterplots of pairs of variables
plot(davis2)

# fix
fix(davis2)
