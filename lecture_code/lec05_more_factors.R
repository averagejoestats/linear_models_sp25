
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
    m3$coefficients["log(total_living_area)"], col = "magenta"
)
abline( 
    m3$coefficients["(Intercept)"] + m3$coefficients["zip_code63130"], 
    m3$coefficients["log(total_living_area)"], col = "blue"
)
legend( 
    "topleft", legend = c("63105","63130"), 
    lwd = 1, col = c("magenta","blue") 
)

# digression: beware of normalization effect
# create a random predictor
dat$random <- 2000 + 100*rnorm(nrow(dat))

plot( dat$random, dat$sale_price )
summary( lm( sale_price ~ random, data = dat ) )

# normalize by square feet
dat$random_per_sq_ft <- dat$random/dat$total_living_area 
summary( lm( price_per_sq_ft ~ random_per_sq_ft, data = dat ) )

plot( dat$random_per_sq_ft, dat$price_per_sq_ft )
# what is going on here?


# bedroom analysis
summary( 
    lm( log(sale_price) ~ log(total_living_area) + bedrooms, data = dat )
)


# let's do a two-factor model
m4 <- lm( log(sale_price) ~ zip_code + basement, data = dat)
summary(m4)
m4$xlevels

# let's put some more stuff in the model
m4 <- lm( 
    log(sale_price) ~ 
    zip_code + log(total_living_area) + basement +
    total_acres + log(total_acres) + 
    bedrooms + total_rooms + full_baths +
    half_baths, data = dat
)
summary(m4)

# residual plot
plot( m4$fitted.values, m4$residuals )




