
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
    temps$temp7a[ii], temps$doy[ii], temps$temp3p[ii],
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
m4 <- lm( temp3p ~ doy + sin_day + cos_day, data=temps[ii,] )
summary(m4)
# 6.017

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



# 
# # plot the fitted models
# ngrid <- 50
# temp_grid <- seq(0,25,length.out=ngrid)
# day_grid <- seq(120,270,length.out=ngrid)
# temp_day_grid <- expand.grid(temp_grid, day_grid)
# temp_day_grid[,3] <- temp_day_grid[,1]*temp_day_grid[,2]
# temp_day_grid[,4] <- temp_day_grid[,1]*temp_day_grid[,1]
# temp_day_grid[,5] <- temp_day_grid[,2]*temp_day_grid[,2]
# onevec <- rep(1,nrow(temp_day_grid))
# zlims <- c(10,30)
# 
# par(mfrow=c(1,1))
# # temp only
# df <- as.matrix(cbind(onevec, temp_day_grid[,c(1)]))
# mod <- m1
# fit3p <- c( df %*% mod$coefficients )
# fields::image.plot( temp_grid, day_grid, matrix(fit3p, ngrid, ngrid),
#     zlim = zlims)
# mtext("linear temp",side=3,line=1)
# 
# # doy only
# df <- as.matrix(cbind(onevec, temp_day_grid[,c(2)]))
# mod <- m2
# fit3p <- c( df %*% mod$coefficients )
# fields::image.plot( temp_grid, day_grid, matrix(fit3p, ngrid, ngrid),
#     zlim = zlims)
# mtext("linear doy",side=3,line=1)
# 
# # additive 
# df <- as.matrix(cbind(onevec, temp_day_grid[,c(1,2)]))
# mod <- m3
# fit3p <- c( df %*% mod$coefficients )
# fields::image.plot( temp_grid, day_grid, matrix(fit3p, ngrid, ngrid),
#     zlim = zlims)
# mtext("additive",side=3,line=1)
# 
# # interaction
# df <- as.matrix(cbind(onevec, temp_day_grid[,c(1,2,3)]))
# mod <- m4
# fit3p <- c( df %*% mod$coefficients )
# fields::image.plot( temp_grid, day_grid, matrix(fit3p, ngrid, ngrid),
#     zlim = zlims)
# mtext("interaction",side=3,line=0.5)
# 
# # additive quadratic
# df <- as.matrix(cbind(onevec, temp_day_grid[,c(1,2,4,5)]))
# mod <- m5
# fit3p <- c( df %*% mod$coefficients )
# fields::image.plot( temp_grid, day_grid, matrix(fit3p, ngrid, ngrid),
#     zlim = zlims)
# mtext("additive quadratic",side=3,line=0.5)
# 
# # full quadratic
# df <- as.matrix(cbind(onevec, temp_day_grid[,c(1,2,4,5,3)]))
# mod <- m6
# fit3p <- c( df %*% mod$coefficients )
# fields::image.plot( temp_grid, day_grid, matrix(fit3p, ngrid, ngrid),
#     zlim = zlims)
# mtext("full quad",side=3,line=0.5)
# 
# 
