
# load the data. This dataset is saved as a .RData file
load("../datasets/stlcounty_sales.RData")
ls()

# fix an outlier
dat[ dat$price_per_sq_ft < 50, ] # this one was a tear-down
dat[ "9708", c("sale_price", "price_per_sq_ft")] <- NA

head(dat)
class( dat$zip_code )
dat$zip_code <- as.character( dat$zip_code )

# zip code analysis
m1 <- lm( sale_price ~ zip_code, data = dat )
summary(m1)

# before, we controlled for size of the house by analyzing
# the price per square foot.

# here, we just put the size of the house into the model
m2 <- lm( sale_price ~ zip_code + total_living_area, data = dat )
summary(m2)

plot( dat$total_living_area, dat$sale_price )

# make a good log plot using the "log" argument
plot( dat$total_living_area, dat$sale_price, log = "xy" )

# looks pretty good on the log scale, so let's do that regression
m3 <- lm( log(sale_price) ~ zip_code + log(total_living_area), data = dat)
summary(m3)

# make a log plot "by hand"
plot( log(dat$total_living_area), log(dat$sale_price) )
abline( 
    m3$coefficients["(Intercept)"], 
    m3$coefficients["log(total_living_area)"], col = "magenta", lwd = 3
)
abline( 
    m3$coefficients["(Intercept)"] + m3$coefficients["zip_code63130"], 
    m3$coefficients["log(total_living_area)"], col = "blue", lwd = 3
)
abline( 
    m3$coefficients["(Intercept)"] + m3$coefficients["zip_code63122"], 
    m3$coefficients["log(total_living_area)"], col = "green", lwd = 3
)
legend( 
    "topleft", legend = c("63105","63130","63122"), 
    lwd = 1, col = c("magenta","blue","green") 
)

# what is the slope when you don't include zip code?
m3a <- lm( log(sale_price) ~ log(total_living_area), data = dat )
summary(m3)
summary(m3a)
# why is it larger?

# add the line
abline( 
    m3a$coefficients["(Intercept)"], 
    m3a$coefficients["log(total_living_area)"], col = "red", lwd = 3
)


# digression: beware of normalization effect
# create a random predictor
dat$random <- 2000 + 100*rnorm(nrow(dat))

plot( dat$random, dat$sale_price )
summary( lm( sale_price ~ random, data = dat ) )

# normalize by square feet
dat$random_per_sq_ft <- dat$random/dat$total_living_area 
m_random <-  lm( price_per_sq_ft ~ random_per_sq_ft, data = dat ) 
summary(m_random)


plot( dat$random_per_sq_ft, dat$price_per_sq_ft )
abline(m_random, col = "magenta")
# what is going on here?

# There is still a relationship between price per sq ft and 
# total living area (sq ft), so if you regression price per sq ft
# on some function of total living area, you will be a significant result, 
# even for functions total living area that contain randomness
plot( dat$total_living_area, dat$price_per_sq_ft )


# bedroom analysis 
summary( 
    lm( log(sale_price) ~ log(total_living_area) + bedrooms, data = dat )
)


# let's do a two-factor model
m4 <- lm( log(sale_price) ~ zip_code + basement, data = dat)
summary(m4)
m4$xlevels

# get the table of estimated expected values
cj <- c(0, m4$coefficients[2:12])
dk <- c(0,m4$coefficients[13:15])

cj
dk
m4$coefficients[1]

# fix the names
names(cj)[1] <- "zip_code63105"
names(dk)[1] <- "basementCrawl"
cj
dk

tab <- m4$coefficients[1] + outer( cj, dk, FUN = "+" )
tab

# compare against the averages
tab2 <- tapply( 
    log(dat$sale_price), list(dat$zip_code, dat$basement), mean, na.rm = TRUE 
)

tab
tab2
# not quite the same

# let's put some more stuff in the model
m5 <- lm( 
    log(sale_price) ~ 
    zip_code + log(total_living_area) + basement +
    total_acres + log(total_acres) + 
    bedrooms + total_rooms + full_baths +
    half_baths, data = dat
)
summary(m5)

# residual plot
plot( m4$fitted.values, m4$residuals )

# build in model plotting function
plot(m5)

# let's take a look at leverage
lev <- hatvalues(m5)
hist(lev, breaks = 100)

ii <- which( lev > 0.10 )
rows <- names(ii)

dat[rows,]


# predicted values and lognormal distribution
head(dat)
X <- model.matrix( m5 )
x4 <- X["4",]

fitval <- sum( x4*m5$coefficients )
fitval
crossprod( x4, m5$coefficients )
m5$fitted.values["4"]
# all the same

# quartiles
q25 <- qnorm( 0.25, mean = fitval, sd = sqrt(predvar) )
q75 <- qnorm( 0.75, mean = fitval, sd = sqrt(predvar) )

predvar <- summary(m5)$sigma^2 + t(x4) %*% vcov(m5) %*% x4

# plot the density and the quartiles
xx <- seq( 13.5, 15.5, by = 0.01 )
yy <- dnorm( xx, mean = fitval, sd = sqrt(predvar) )
plot(xx,yy,type="l")
abline(v = fitval)
abline(v = q25 )
abline(v = q75 )

# expectation of log sale price
sum( xx*yy*0.01 )

# expectation of sale price = exp(log sale price)
sum( exp(xx)*yy*0.01 )
exp( fitval )
exp( fitval + predvar/2 ) 

# plot of quartiles of sale price = exp(log sale price)
plot(exp(xx),yy,type="l")
abline(v = exp(fitval))
abline(v = exp(q25) )
abline(v = exp(q75) )
abline(v = exp(fitval+predvar/2),col = "magenta" )

