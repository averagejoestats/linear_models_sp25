
# make sure R starts with no objects in environment
ls()
# Tools --> Global Options --> General 
# uncheck "Restore .RData into workspace at startup"
# set "Save workspace to .RData on exit" to "Never"

# install.packages("alr4")

# load the alr4 package
library("alr4")
ls()

# calling library silently loads all alr4 functions and datasets
head( BGSgirls )
ls()
BGSgirls$WT2
ls()
# we can access the data, but it's not listed in ls()

# If you want it listed in ls(), need to call data function
data("BGSgirls", package = "alr4")
ls()

# Datasets in R packages have documentation
?BGSgirls   

#####################################################################


# Multiple linear regression

# suppose we want to predict height at age 18 from
# the data available at ages 2 and 9

# set up a panel of plots with 1 row and 3 columns
par(mfrow=c(1,3))

# plot height age 18 against 3 predictors
plot( BGSgirls$HT2, BGSgirls$HT18, 
      xlab="Height Age 2", ylab="Height Age 18" )
plot( BGSgirls$HT9, BGSgirls$HT18, 
    xlab="Height Age 9", ylab="Height Age 18" )
plot( BGSgirls$LG9, BGSgirls$HT18, 
    xlab="Leg circ Age 9", ylab="Height Age 18" )

# first analysis using height at age 2

# reset graphical parameter to 1 x 1 plot
par(mfrow=c(1,1))         
# plot the data
plot(BGSgirls$HT2, BGSgirls$HT18 )
m1 <- lm(HT18 ~ HT2, data = BGSgirls )
summary(m1)

# what is R^2 and adjusted R^2?
sd( BGSgirls$HT18 )
summary(m1)$sigma
summary(m1)$sigma^2/sd(BGSgirls$HT18)^2
1 - summary(m1)$sigma^2/sd(BGSgirls$HT18)^2

rss <- sum( m1$residuals^2 )
syy <- sum( ( BGSgirls$HT18 - mean(BGSgirls$HT18) )^2 )
rss
syy
rss/syy
1 - rss/syy

# analysis using height at age 9
plot(BGSgirls$HT9, BGSgirls$HT18 )
m2 <- lm(HT18 ~ HT9, data = BGSgirls )
summary(m2)

# which is better?
summary(m1)$sigma
summary(m2)$sigma

# what if we include both?

# before we fit the model, 
# think about what you expect the results to be

m3 <- lm(HT18 ~ HT2 + HT9, data = BGSgirls)
summary(m3)

# is HT2 1/3 as "important" as HT9 ( 0.2682/0.7568 is about 1/3 )
sd( BGSgirls$HT2 )
sd( BGSgirls$HT9 )
sd( BGSgirls$HT2 )*m3$coefficients[2]
sd( BGSgirls$HT9 )*m3$coefficients[3]

# That's a good rough measure, but the full story is a bit more subtle

# why is HT2 no longer significant?
# HT2 does not explain the variation not explained by HT9.
# The variation not explained by HT9 is m2$residuals.
plot( BGSgirls$HT2, m2$residuals )

# HT9 does explain some variation not explained by HT2
# The variation not explained by HT2 is m1$residuals.
plot( BGSgirls$HT9, m1$residuals )
summary( lm(m1$residuals ~ BGSgirls$HT9 ) )


# what about leg circumference?
summary( lm( HT18 ~ LG9, data = BGSgirls ) )  # maybe something there

m4 <- lm(HT18 ~ HT9 + LG9, data = BGSgirls )
summary(m4)

summary(m2)$sigma   # age 9 height only
summary(m3)$sigma   # age 9 height + age 2 height
summary(m4)$sigma   # age 9 height + age 9 leg circ

# For discussion:

# Can you think of a biological reason why HT2 coefficient is
# positive (though not stat. sig. different from 0)

# Can you think of a biological reason why LG9 coefficient is
# negative?

# You answer must grapple with the fact that the interpretation
# of the coefficient is the effect of one variable after
# controlling for the other (holding it constant).


# How do get design matrix, if you need it
X <- model.matrix( ~ HT9 + LG9, data = BGSgirls )
dim(X)
head(X)

# regression calculations "by hand"
y <- BGSgirls$HT18
bhat <- solve( t(X) %*% X ) %*% t(X) %*% y
m4$coefficients
bhat

# matrix inverse times another matrix should be done like this
bhat <- solve( t(X) %*% X, t(X) %*% y )

# projection matrix
P <- X %*% solve( t(X) %*% X ) %*% t(X)
dim(P)

# fitted values
yhat <- X %*% bhat
range( yhat - P %*% y )
range( yhat - m4$fitted.values )


# residuals
resids <- y - yhat

# sigmahat
sigmahat <- sqrt( 1/( nrow(X)-ncol(X) )*sum( resids^2 ) )

# covariance matrix for Bhat
var_Bhat <- sigmahat^2*solve( t(X) %*% X )
var_Bhat
diag(var_Bhat)
se_Bhat <- sqrt( diag( var_Bhat ) )
se_Bhat

summary(m4)

# there is a function for extracting covariance matrix from model
vcov(m4)
var_Bhat

y[37]

var_Bhat[2, 2]

var_Bhat[ "HT9", "HT9" ] + 
var_Bhat[ "LG9", "LG9" ] + 
-2*var_Bhat[ "HT9", "LG9" ]



# ithaca temperature data
ith <- read.csv("../datasets/ithaca_weather_2017_2019.csv")
n <- nrow(ith)

# draw a plot
plot( ith$maxtemp, type="l", col = "red", axes = FALSE, 
    ylim = c(-10,100), xlab = "", ylab = "Temperature (F)" )
lines( ith$mintemp, col = "blue" )
little_ticks <- seq(0, n, by = 365/12)
big_ticks <- seq(0, n, by = 365 )
axis(1,at=little_ticks, labels = rep("",length(little_ticks)))
axis(1,at=big_ticks, labels = rep("",length(big_ticks)), tcl = -1)
axis(1,at=seq(0.5,2.5,by=1)*365, tcl = 0, labels = c("2017","2018","2019"))
axis(2,at=seq(-10,100,by=10))
box()

# we are going to work with the avg temp
plot( ith$avgtemp, type="l",  axes = FALSE, ylim = c(-10,100))
axis(1,at=little_ticks, labels = rep("",length(little_ticks)))
axis(2,at=seq(-10,100,by=10))
box()

# There's clearly a pattern over time, but regressing on time won't help
head(ith)
class(ith$date)
ith$date <- as.Date( ith$date )
head(ith)
ith$day_count <- as.numeric( ith$date - min(ith$date) )
head(ith)

m0 <- lm( avgtemp ~ day_count, data = ith ) 
summary(m0)
abline(m0)


# first analysis - sine and cosine
y <- ith$avgtemp
ss <- sin( (ith$day_count)*2*pi/365.25 )
cc <- cos( (ith$day_count)*2*pi/365.25 )
m1 <- lm( y ~ ss + cc )
lines( m1$fitted.values, col = "magenta", lwd = 2 ) # hey not bad!
summary(m1)
summary(m1)$sigma
# what assumption is violated?

# let's look at the departure from average
sd( ith$avgtempdiff )
plot( ith$avgtempdiff, type="l", 
    axes = FALSE, ylim = c(-30,30))
axis(1,at=seq(0,n,by=365/12), labels = rep("",25))
axis(2,at=seq(-30,30,by=5))
abline(0,0)
box()

# "auto"-regression uses past values to predict future
# need to "line up" the data to do this type of regression
df_auto <- data.frame( 
     y = ith$avgtempdiff[5:( nrow(ith) - 0 ) ], 
    x1 = ith$avgtempdiff[4:( nrow(ith) - 1 ) ],
    x2 = ith$avgtempdiff[3:( nrow(ith) - 2 ) ],
    x3 = ith$avgtempdiff[2:( nrow(ith) - 3 ) ],
    x4 = ith$avgtempdiff[1:( nrow(ith) - 4 ) ]
)

par(mfrow=c(2,2))
plot( df_auto$x1, df_auto$y )
plot( df_auto$x2, df_auto$y )
plot( df_auto$x3, df_auto$y )
plot( df_auto$x4, df_auto$y )

# marginal analyses first
m0 <- lm( y ~ 1, data = df_auto )
m1 <- lm( y ~ x1, data = df_auto )
m2 <- lm( y ~ x2, data = df_auto )
m3 <- lm( y ~ x3, data = df_auto )
m4 <- lm( y ~ x4, data = df_auto )

summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

# multiple analysis
m1 <- lm(y ~ x1, data = df_auto )
m12 <- lm(y ~ x1 + x2, data = df_auto )
m123 <- lm(y ~ x1 + x2 + x3, data = df_auto )
m1234 <- lm(y ~ x1 + x2 + x3 + x4, data = df_auto )

summary(m1)
summary(m12)
summary(m123)
summary(m1234)

# For discussion: why is b2 negative?



