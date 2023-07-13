# Example 1: simultaneous 90% confidence intervals for trivariate data
library(jocre)
trivar <- mvtnorm::rmvnorm(n=20, mean=rep(0.05, 3), sigma=toeplitz(c(0.05, 0.04, 0.03)))
colnames(trivar) <- c("AUCinf", "AUCt", "Cmax")

tost <- jocre::cset(dat=trivar, method="tost", alpha=0.1)
summary(tost)
