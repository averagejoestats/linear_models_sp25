
# read in the data
temps <- read.csv("../datasets/salem_7a_3p.csv")
head(temps)
dim(temps)

# make the date column be a date
class( temps$date )
temps$date <- as.Date( temps$date, "%Y-%m-%d" )
class( temps$date )

# plots
lims <- range(c(temps$temp7a, temps$temp3p), na.rm=TRUE)
plot(temps$temp7a, temps$temp3p, xlim=lims, ylim=lims, cex=0.3, pch=16)
abline(0,1,col="magenta",lwd=2)

plot(temps$doy, temps$temp3p, cex = 0.3, pch = 16 )

# install.packages("fields")
ii <- !( is.na( temps$temp3p ) | is.na( temps$temp7a ) )
fields::quilt.plot( 
    temps$doy[ii], temps$temp7a[ii], temps$temp3p[ii],
    xlab = "7am temp (C)", ylab = "Day of Year",
    main = "3pm Temp, by 7am Temp and Day of Year"
)

# fit some models
m1 <- lm( temp3p ~ temp7a, data = temps[ii,] )
summary(m1)
# sigmahat = 5.532

# linear in day of year
m2 <- lm( temp3p ~ doy, data = temps[ii,] )
summary(m2)
# sigmahat = 10.36

# quadratic in day of year
m3 <- lm( temp3p ~ doy + I(doy^2), data = temps[ii,] )
summary(m3)
# 6.306

# sin and cosine
temps$sin_day <- sin( temps$doy*2*pi/365.25 )
temps$cos_day <- cos( temps$doy*2*pi/365.25 )
temps$sin_day2 <- sin( temps$doy*4*pi/365.25 )
temps$cos_day2 <- cos( temps$doy*4*pi/365.25 )
m4 <- lm( temp3p ~ doy + sin_day + cos_day, data=temps[ii,] )
summary(m4)
# 6.017
m4a <- lm( temp3p ~ doy + sin_day + cos_day + sin_day2 + cos_day2, data=temps[ii,] )
summary(m4a)

# sin and cosine and 7am temp
temps$sin_day <- sin( temps$doy*2*pi/365.25 )
temps$cos_day <- cos( temps$doy*2*pi/365.25 )
m5 <- lm( temp3p ~ doy + sin_day + cos_day + temp7a, data=temps[ii,] )
summary(m5)
# 5.154

# compare the importance of 7am vs day of year

# importance of day of year
anova(m1, m5)

# importance of 7am temp
anova(m4, m5)

# let's try some interactions

# factor-numeric models with 7am and month
temps$month <- format( temps$date, "%m-%b" )
summary( lm( temp3p ~ temp7a + month, data = temps ) )
summary( lm( temp3p ~ temp7a + month - 1, data = temps ) )
summary( lm( temp3p ~ temp7a * month, data = temps ) )

# investigate May vs Nov in a plot
xlims <- c(-20,30)
ylims <- c(-10,40)
iiMay <- temps$month == "05-May"
iiNov <- temps$month == "11-Nov"
plot( 
    temps$temp7a[iiMay], temps$temp3p[iiMay], 
    xlim = xlims, ylim = ylims,
    xlab = "7am temp (C)", ylab = "3pm temp (C)", cex=0.5, pch=16
)
points( 
  temps$temp7a[iiNov], temps$temp3p[iiNov], col="magenta",
  cex=0.5, pch=16
)
legend(
    "topleft", legend = c("May","Nov"), 
    col = c("black","magenta"), pch = 16
)

# what's the difference between fitting this
# factor-numeric interaction, versus fitting
# separate linear models for each month?
summary( lm( temp3p ~ temp7a, data = temps[iiMay,] ) )
summary( lm( temp3p ~ temp7a, data = temps[iiNov,] ) )


# what model would you fit to estimate long term trends?


# try a full quadratic model in doy and 7am temp
m6 <- lm( temp3p ~ temp7a*doy + I(temp7a^2) + I(doy^2), data = temps )
summary(m6)


# plot the fitted values
ngrid <- 50
temp7a_grid <- seq(0,25,length.out=ngrid)
doy_grid <- seq(1,366, length.out=ngrid)
grid <- expand.grid( doy = doy_grid, temp7a = temp7a_grid )
grid$sin_day <- sin(2*pi*grid$doy/365.25)
grid$cos_day <- cos(2*pi*grid$doy/365.25)

m1$preds <- predict(m1, grid)
m2$preds <- predict(m2, grid)
m3$preds <- predict(m3, grid)
m4$preds <- predict(m4, grid)
m5$preds <- predict(m5, grid)
m6$preds <- predict(m6, grid)

par(mfrow=c(2,3))
for( mm in list(m1,m2,m3,m4,m5,m6) ){
    print(mm)
    z <- matrix(mm$preds,ngrid,ngrid)
    fields::image.plot(doy_grid, temp7a_grid, z)
    points(temps$doy, temps$temp7a, col = "gray", cex = 0.2, pch = 16 )
    mtext(mm$call,3,line=1)
}

# sin day cos day interaction model?
m7 <- lm( 
    temp3p ~ temp7a*sin_day + temp7a*cos_day + 
        I(temp7a^2*sin_day) + I(temp7a^2*cos_day),
    data = temps 
)
summary(m7)

m7$preds <- predict(m7, grid)

par(mfrow=c(2,3))
for( mm in list(m1,m2,m4,m5,m6,m7) ){
    print(mm)
    z <- matrix(mm$preds,ngrid,ngrid)
    fields::image.plot(doy_grid, temp7a_grid, z)
    points(temps$doy, temps$temp7a, col = "gray", cex = 0.2, pch = 16 )
    mtext(mm$call,3,line=1)
}



