
# read in the fort collins snow dataset.
# the paths assume that our working directory is the
# directory that contains this sript (lec01_simple_linear_models.R)
# We go up one directory (..), then down into the datasets directory (/datasets)
# and then get the file we want (ftcollinssnow.csv)
snow <- read.csv("../datasets/ftcollinssnow.csv")

# look at the data
snow

# simple plot
plot( snow$Early, snow$Late )
# doesn't look like there's much going on

# let's calculate some statistics "by hand"
n <- nrow(snow)
xbar <- mean( snow$Early )  
ybar <- mean( snow$Late )
xbar
ybar

sxx <- sum( ( snow$Early - xbar )^2 )
syy <- sum( ( snow$Late - ybar )^2 )
sxy <- sum( ( snow$Early - xbar )*( snow$Late - ybar ) )

# what's going on here?
snow$Early
xbar
snow$Early - xbar 
snow$Late - ybar
( snow$Early - xbar )*( snow$Late - ybar )
sum( ( snow$Early - xbar )*( snow$Late - ybar ) )
sxy

b1hat <- sxy/sxx
b0hat <- ybar - b1hat*xbar
b1hat
b0hat

plot( snow$Early, snow$Late )
abline( b0hat, b1hat, col = "magenta" )
points( xbar, ybar, col = "blue", cex = 2, pch = 4 )

# let's make a nicer plot
plot( 
    snow$Early, snow$Late, 
    xlab = "Early Season Snow Total (inches)", 
    ylab = "Late Season Snow Total (inches)",
    main = "Fort Collins Snow"
)
abline( b0hat, b1hat, col = "magenta" )
points( xbar, ybar, col = "blue", cex = 2, pch = 4 )

# estimate the standard deviation
fitted_values <- b0hat + b1hat*snow$Early
residuals <- snow$Late - fitted_values
sigmahat <- sqrt( 1/(n-2)*sum( residuals^2 ) )
sigmahat

# calculate t statistics for testing H_0: b_j = 0
seb0 <- sigmahat*sqrt( 1/n + xbar^2/sxx )
seb1 <- sigmahat/sqrt(sxx)
t0 <- (b0hat - 0)/seb0
t1 <- (b1hat - 0)/seb1

# perform hypothesis test by calculating two-sided p-values
# P( T_{n-2} < -|t0| ) + P( T_{n-2} > |t0| )

p0 <- pt( -abs(t0), n-2 ) + (1 - pt( abs(t0), n-2 ) )
p0

p1 <- pt( -abs(t1), n-2 ) + (1 - pt( abs(t1), n-2 ) )
p1

# get confidence intervals
width0 <- seb0*c( qt(0.025, n-2), qt(0.975, n-2) )
ci0 <- b0hat + width0

width1 <- seb1*c( qt(0.025, n-2), qt(0.975, n-2) )
ci1 <- b1hat + width1


# now do all this with the lm function
fit <- lm( Late ~ Early, data = snow )

fit
b0hat
b1hat

# fit is a special kind of object called a list
names(fit)
fit$coefficients
fit$residuals

# use the summary function, with fit as argument, to print useful information
summary(fit)
seb0
seb1
t0
t1
p0
p1
sigmahat
n-2

# how do you get sigmahat?
names(fit)
summary_fit <- summary(fit)
names(summary_fit)
summary_fit$sigma
