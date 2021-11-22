n <- 100
xbar <- 500
sigma <- 40
alpha_95 <- 0.05
se <- sigma/sqrt(100)
me_95 <- qnorm(1-alpha_95/2)*se
xbar + c(-me_95, me_95)

alpha_99 <- 0.01 
me_99 <- qnorm(1-alpha_99/2)*se
xbar + c(-me_99, me_99)

n <- 100
xbar <- 120
s <- 20
alpha_95 <- 0.05
se <- s/10
me_95 <- qnorm(1-alpha_95/2) * se
xbar + c(-me_95, me_95)

n <- 16
xbar <- 18.3
s <- 1.8
alpha_90 <- 0.1
se_1 <- s/sqrt(n)
ms_90 <- qt(1-alpha_90/2, n-1) * se_1
xbar + c(-ms_90, ms_90)

n <- 5
a <- c(3.8, 4.5, 5.2, 4.0, 5.5)
xbar <- mean(a)
s <- sd(a)
alpha_90 <- .1
se_1 <- s/sqrt(n)
ms_90 <- qt(1-alpha_90/2, n-1) * se_1
xbar + c(-ms_90, ms_90)

b <- c(8.1, 8.7, 7.6, 7.8, 8.5, 7.9)
n <- length(b)
s <- sd(b)
alpha_95 <- 0.05
se_1 <- s/sqrt(n)
((n-1)*s^2)/c(qchisq(c(1-alpha_95/2, alpha_95/2), df=n-1))

n <- 4000
x <- 50
phat <-  x/n
se <- sqrt(phat*(1-phat)/n)
me_95 <- qnorm(1-alpha_95/2) *se
phat + c(-me_95, +me_95)
