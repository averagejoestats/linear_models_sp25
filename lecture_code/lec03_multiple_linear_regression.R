

# read the temperature data
weather <- read.csv("../datasets/daily_uscrn_weather.csv")
n <- nrow(weather)


# First thing always: try to look at the data
head( weather )
tail( weather )

# should probably remove the last row
ii <- weather$date == "2025-02-19"
which(ii)
weather$salem[ii] <- NA
weather$ithaca[ii] <- NA
weather$champaign[ii] <- NA

tail( weather )

par(mfrow=c(1,3))
hist( weather$salem, breaks = 100 )
hist( weather$champaign, breaks = 100 )
hist( weather$ithaca, breaks = 100 )

# throws an error
plot( 
    weather$date, weather$salem, 
    type="l",  axes = FALSE, ylim = c(-20,90)
)
class( weather$date )

# change to date class
weather$date <- as.Date( weather$date, "%Y-%m-%d" )

par(mfrow=c(1,1))
plot( weather$date, weather$salem, type="l",  
      ylim = c(-20,90))
lines( weather$date, weather$champaign, col = "magenta" )
lines( weather$date, weather$ithaca, col = "blue" )

# scatterplots
par(mfrow=c(1,3))
plot( weather$salem, weather$champaign )
abline(0,1,col="magenta",lwd=2)
plot( weather$salem, weather$ithaca )
abline(0,1,col="magenta",lwd=2)
plot( weather$champaign, weather$ithaca )
abline(0,1,col="magenta",lwd=2)

# for convenience, let's shorten the time frame
weather <- weather[ weather$date >= "2020-07-01", ]

par(mfrow=c(1,1))
plot( weather$date, weather$salem )

# There's clearly a pattern over time, but regressing on time won't help
weather$day_count <- 
    as.numeric( weather$date - min(weather$date) )
head(weather)

m0 <- lm( salem ~ day_count, data = weather ) 
summary(m0)

plot( weather$day_count, weather$salem )
abline(m0)
# what do you think of this?


# sine and cosine
weather$sin1 <- sin( (weather$day_count)*2*pi/365.25 )
weather$cos1 <- cos( (weather$day_count)*2*pi/365.25 )

m1 <- lm( salem ~ sin1 + cos1, data = weather )
summary(m1)

m2 <- lm( salem ~ sin1 + cos1 + day_count, data = weather )
summary(m2)


summary(m0)$sigma
summary(m1)$sigma
summary(m2)$sigma

# make a plot of data and fitted values
plot( weather$salem )
lines( m1$fitted.values, col = "magenta", lwd = 2 ) # hey not bad!

# this isn't quite right
length( weather$salem )
length( m1$fitted.values )
sum( is.na( weather$salem ) )
length( weather$salem ) - length( m1$fitted.values )

# this is an annoying thing about R but important: a quick way fix
plot( m1$fitted.values + m1$residuals )
lines( m1$fitted.values, col = "magenta", lwd = 2 )

# even better, make a data frame with the fitted values
head( weather )
weather$m1_fitted_values <- NA
weather[names(m1$fitted.values), "m1_fitted_values"] <- m1$fitted.values


plot( weather$date, weather$salem )
lines( weather$date, weather$m1_fitted_values, col = "magenta", lwd = 2 )

# we should also look at the residuals
plot( weather$date, weather$salem - weather$m1_fitted_values )

# let's do the same analysis for champaign and ithaca
m2 <- lm( champaign ~ sin1 + cos1, data = weather )
summary(m2)

m3 <- lm( ithaca ~ sin1 + cos1, data = weather )
summary(m3)

summary(m1)$sigma
summary(m2)$sigma
summary(m3)$sigma

# add the fitted values to the data frame
weather$m2_fitted_values <- NA
weather[ names( m2$fitted.values ), "m2_fitted_values" ] <- m2$fitted.values

weather$m3_fitted_values <- NA
weather[ names( m3$fitted.values ), "m3_fitted_values" ] <- m3$fitted.values

head( weather )

# plot them
plot( weather$date, weather$m1_fitted_values, col = "black", type = "l", ylim = c(20,80) )
lines( weather$date, weather$m2_fitted_values, col = "magenta" )
lines( weather$date, weather$m3_fitted_values, col = "blue" )
legend(
    "bottomleft", legend = c("salem","champaign","ithaca"),
    lwd = 1, col = c("black","magenta","blue")
)

# compare aspects of the models

# vertical shift: (Intercept)
m1$coefficients
m2$coefficients
m3$coefficients

# amplitude
sqrt( m1$coefficients[2]^2 + m1$coefficients[3]^2 )
sqrt( m2$coefficients[2]^2 + m2$coefficients[3]^2 )
sqrt( m3$coefficients[2]^2 + m3$coefficients[3]^2 )

# coldest day?
atan( m1$coefficients[2]/m1$coefficients[3] )*365.25/(2*pi) 
atan( m2$coefficients[2]/m2$coefficients[3] )*365.25/(2*pi) 
atan( m3$coefficients[2]/m3$coefficients[3] )*365.25/(2*pi) 

weather[ which.min( weather$m1_fitted_values ), ]
weather[ which.min( weather$m2_fitted_values ), ]
weather[ which.min( weather$m3_fitted_values ), ]



# "auto"-regression uses past values to predict future
# need to "line up" the data to do this type of regression
# let's use the residuals for this analysis
weather$salem_resids <- weather$salem - weather$m1_fitted_values

weather$salem_resids1 <- NA
weather$salem_resids2 <- NA
weather$salem_resids3 <- NA
weather$salem_resids4 <- NA

n <- nrow(weather)
weather$salem_resids1[ 2:n ] <- weather$salem_resids[ 1:(n-1) ]
weather$salem_resids2[ 3:n ] <- weather$salem_resids[ 1:(n-2) ]
weather$salem_resids3[ 4:n ] <- weather$salem_resids[ 1:(n-3) ]
weather$salem_resids4[ 5:n ] <- weather$salem_resids[ 1:(n-4) ]

tail( weather[, c("salem_resids","salem_resids1","salem_resids2","salem_resids3","salem_resids4") ] )

plot( weather$date, weather$salem_resids )

par(mfrow=c(2,2))
plot( weather$salem_resids1, weather$salem_resids )
plot( weather$salem_resids2, weather$salem_resids )
plot( weather$salem_resids3, weather$salem_resids )
plot( weather$salem_resids4, weather$salem_resids )

# marginal analyses first
m0 <- lm( salem_resids ~ 1, data = weather )
m1 <- lm( salem_resids ~ salem_resids1, data = weather )
m2 <- lm( salem_resids ~ salem_resids2, data = weather )
m3 <- lm( salem_resids ~ salem_resids3, data = weather )
m4 <- lm( salem_resids ~ salem_resids4, data = weather )

summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

# multiple analysis
m1 <- lm(salem_resids ~ salem_resids1, data = weather )
m12 <- lm(salem_resids ~ salem_resids1 + salem_resids2, data = weather )
m123 <- lm(salem_resids ~ salem_resids1 + salem_resids2 + salem_resids3, data = weather )
m1234 <- lm(salem_resids ~ salem_resids1 + salem_resids2 + salem_resids3 + salem_resids4, data = weather )

summary(m1)
summary(m12)
summary(m123)
summary(m1234)

# For discussion: why is b2 negative?



