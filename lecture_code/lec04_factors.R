
# load the data. This dataset is saved as a .RData file
load("../datasets/stlcounty_sales.RData")
ls()

head(dat)
nrow(dat)
length( unique( dat$locator_number ) )

# table function allows us to look at counts of factors
table( dat$zip_code )
table( dat$exterior_wall_type )
table( dat$sale_validity )
table( dat$architectural_style )

# locator number
table( dat$locator_number )
table( table( dat$locator_number ) )
tab <- table( dat$locator_number )
tab[ tab == 2 ]
names( tab[ tab == 2 ] )
dat[ dat$locator_number %in% names( tab[ tab == 2 ] ), ]

# make a plot to look at the data
plot( dat$total_living_area, dat$price_per_sq_ft )
plot( log10(dat$total_living_area), dat$price_per_sq_ft )

# look at the outliers
dat[ dat$price_per_sq_ft < 50, ] # this one is probably an error
dat[ "9708", c("sale_price", "price_per_sq_ft")] <- NA

dat[ dat$total_living_area > 15000, ] # this one is probably real

dat[ dat$price_per_sq_ft > 600, ] # couple of them are suspect

# zip code analysis
tapply( dat$sale_price, dat$zip_code, mean )
cbind( tapply( dat$sale_price, dat$zip_code, mean ) )

library("dplyr")
dat |> group_by(zip_code) |> summarize( avg = mean(sale_price) )
dat |> group_by(zip_code) |> summarize( avg = mean(price_per_sq_ft) )

# factor model
m1 <- lm( price_per_sq_ft ~ zip_code, data = dat )
summary(m1)
# not quite right

m2 <- lm( price_per_sq_ft ~ as.factor(zip_code), data = dat )
summary(m2)
# kind of messy. 

# We can also convert to factor
dat$zip_code <- as.factor( dat$zip_code )
dat$zip_code[1:10]
levels( dat$zip_code )

m2 <- lm( price_per_sq_ft ~ zip_code, data = dat )
summary(m2)

# We can alternatively convert zip code to character
dat$zip_code <- as.character( dat$zip_code )
table( dat$zip_code )
head(dat)
dat$zip_code[ 1:10 ]

m2 <- lm( price_per_sq_ft ~ zip_code, data = dat )
summary(m2)

# look at design matrix
X <- model.matrix( ~ zip_code, data = dat )
dim(X)
head(X)

# remove intercept if we add -1
X <- model.matrix( ~ zip_code -1, data = dat )
dim(X)
head(X)

# fit the model without intercept
m3 <- lm( price_per_sq_ft ~ zip_code - 1, data = dat )
summary(m3)
summary(m2)

# same model! Except interpretation of coefficients changes
range( m2$fitted.values - m3$fitted.values )

# get standard errors for differences
d <- rep(0, length(m2$coefficients) )
d[3] <- 1
d[2] <- -1
cbind( m2$coefficients, d )

# estimate of difference between 63119 and 63117
est <- t(d) %*% m2$coefficients
est

# estimated standard error
se <- sqrt( t(d) %*% vcov(m2) %*% d )
se

# you can also relevel the factor
typeof( dat$zip_code )
dat$zip_code <- as.factor( dat$zip_code )
dat$zip_code[1:10]
dat$zip_code <- relevel( dat$zip_code, ref = "63117" )
dat$zip_code[1:10]

m4 <- lm( price_per_sq_ft ~ zip_code, data = dat)
summary(m4)

# do an F test "manually"
m0 <- lm( price_per_sq_ft ~ 1, data = dat )
rss0 <- sum( m0$residuals^2 )
rss1 <- sum( m2$residuals^2 )
df0 <- m0$df.residual
df1 <- m2$df.residual
Fstat <- (rss0 - rss1)/(df0 - df1)/( rss1/df1 )
Fstat

# can also use the anova function
anova(m0,m2)
