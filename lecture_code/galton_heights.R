
data("GaltonFamilies", package = "HistData")
dat <- GaltonFamilies

head( dat, n = 30 )


par(mfrow=c(2,2))
plot( dat$father, dat$childHeight )
plot( dat$mother, dat$childHeight )
plot( dat$midparentHeight, dat$childHeight )

# midparent height = 0.5*father + 0.54*mother
m0 <- lm( midparentHeight ~ father + mother, data = dat )
summary(m0)

# some simple models
m1 <- lm( childHeight ~ gender + mother, data = dat )
m2 <- lm( childHeight ~ gender + father, data = dat )
m3 <- lm( childHeight ~ gender + midparentHeight, data = dat )
m4 <- lm( childHeight ~ gender + father + mother, data = dat )
m5 <- lm( childHeight ~ gender * midparentHeight, data = dat )
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

# you can compare these with F test because midparentHeight is a
# linear combination of father and mother
anova(m1,m4)
anova(m2,m4)
anova(m3,m4)

# include family
m6 <- lm( childHeight ~ gender + father + mother + family, data = dat)
summary(m6)
m6$coefficients[1:10]

# family as a random effect 
library("lme4")
m7 <- lmer( 
    childHeight ~ gender + father + mother + (1|family), 
    data = dat 
)
summary(m7)

sqrt( 0.9073 + 3.8197 )

m8 <- lmer( 
    childHeight ~ gender + father + mother + (gender|family), 
    data = dat 
)
summary(m8)





m6 <- lmer( childHeight ~ children + gender + father + mother + (1|family), data = dat )
summary(m6)

# create a genderNum variable, indicating order within gender
fam <- unique( dat$family )
dat$genderNum <- dat$childNum
for(j in 1:length(fam)){
    ii <- dat$family == fam[j] & dat$gender == "female"
    first_female <- dat$childNum[ii][1]
    dat$genderNum[ii] <- dat$childNum[ii] - first_female + 1
}

m7 <- lmer(
    childHeight ~ children + genderNum + gender + father + mother + (1|family),
    data = dat
)
summary(m7)




# look at conditional expectation and variance of successive children's heights
summary(m5)


# Conditional expectation of Yk+1 given Y1, ..., Yk ( denoted Y1:k ) 
# = E(Yk+1) + Cov( Yk+1, Y1:k ) %*% Cov( Y1:k, Y1:k )^{-1}( E(Y1:k) - Y1:k )

# conditional variance of Yk+1 given Y1:k
# = Var( Yk+1 ) - Cov( Yk+1, Y1:k ) Cov( Y1:k, Y1:k )^{-1} Cov( Y1:k, Yk+1 )
k <- 1

# calculate the relevant matrices
# variance of Yk+1
C0 <- matrix( 0.9073, 1, 1 ) + 3.8197*diag(nrow=1)
# Cov( Yk+1, Y1:k )
C1 <- t( rep( 0.9073, k ) )
# Cov( Y1:k, Y1:k )
C2 <- matrix( 0.9073, k, k ) + 3.8197*diag(nrow=k)

# linear combination used in conditional expectation
C1 %*% solve(C2)
1/k
( C1 %*% solve(C2) )[1,1]/(1/k)

# conditional variance
C0 - C1 %*% solve(C2, t(C1) )
sqrt( C0 - C1 %*% solve(C2, t(C1) ) )
sqrt( C0 )


# do the conditional variance for a sequence of children.
for( k in 1:1000 ){
    C0 <- matrix( 0.9073, 1, 1 ) + 3.8197*diag(nrow=1)
    C1 <- t( rep( 0.9073, k ) )
    C2 <- matrix( 0.9073, k, k ) + 3.8197*diag(nrow=k)
    #print( ( C1 %*% solve(C2) )[1,1]/(1/k) )
    #cat("\n")
    print( sqrt( C0 - C1 %*% solve(C2, t(C1) ) ) )
    #cat("\n\n")
}

summary(m5)
