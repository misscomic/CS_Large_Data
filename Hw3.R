install.packages("ggplot2")
library(ggplot2)
dat <- read.csv("H:/4720 LDA/Rgraphics/Rgraphics/dataSets/EconomistData.csv")
head(dat)

##############################################################
####################  exercise 1  ############################
#
#   Create a scatter plot with CPI on the x axis and HDI on the y axis.
#   Color the points blue.
#   Map the color of the the points to Region.
#   Make the points bigger by setting size to 2
#   Map the size of the points to HDI.Rank
#
###############################################################
###############################################################

p1 <- ggplot(dat, aes(x = CPI, y = HDI)) 
p1 + geom_point()

p1 + geom_point(color="blue")

p1 + geom_point(aes(color = Region))

p1 + geom_point(size = 2)

p1 + geom_point(aes(size = HDI.Rank))


##############################################################
####################  exercise 2  ############################
#
# Re-create a scatter plot with CPI on the x axis and HDI on the y axis (as you did in the previous exercise).
# Overlay a smoothing line on top of the scatter plot using geom_smooth.
# Overlay a smoothing line on top of the scatter plot using geom_smooth, but use a linear model for the predictions. Hint: see ?stat_smooth.
# Overlay a smoothing line on top of the scatter plot using geom_line. Hint: change the statistical transformation.
# BONUS: Overlay a smoothing line on top of the scatter plot using the default loess method, but make it less smooth. Hint: see ?loess.
#
###############################################################
###############################################################

p2 <- ggplot(dat, aes(x = CPI, y = HDI)) 
p2 + geom_point()

p2 + geom_point() + geom_smooth()

p2 + geom_point() + stat_smooth(method = "lm")

dat$pred <- predict(lm(HDI ~ CPI, data = dat))
p2 + geom_point() + geom_line(aes(y = pred))

p2 + geom_point() + geom_smooth(span = 0.5)
