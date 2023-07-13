# pred.int.mvnorm <- function(x, alpha=.05) {
#   p <- ncol(x)
#   n <- nrow(x)
#   Sigmahat <- var(x)
#   xbar <- apply(x,2,mean)
#   xbar
#   theta <- seq(0, 2*pi, length=100)
#   polygon <- xbar + 
#     sqrt(p*(n-1)/(n-p)*(1 + 1/n)*qf(alpha, p, n - p, lower.tail = FALSE))*
#     t(chol(Sigmahat)) %*% 
#     rbind(cos(theta), sin(theta))
#   t(polygon)
# }
# x <- matrix(c(-0.9,2.4,-1.4,2.9,2.0,0.2,0.7,1.0,-0.5,-1.0),ncol=2)
# plot(pred.int.mvnorm(x), type="l",xlab=expression(x[1]),ylab=expression(x[2]))
# points(x)
# 
# 
# head(pred.int.mvnorm(x))
# http://127.0.0.1:45983/graphics/2c82555c-8b2b-47ca-8d76-1641f7b2cb57.png


library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()
set.seed(17)

# Set the covariance matrix
sigma2 <- matrix(c(5, 2, 2, 5), ncol=2)

# Set the means
mu <- c(5,5)

# Get the correlation matrix
P <- cov2cor(sigma2)

# Generate the data
p <- rmvnorm(n=50, mean=mu, sigma=sqrt(sigma2))

# Plot the data
plot(p)

# Plot the ellipse
lines( ellipse( P, centre = c(5,5)) , col='red')


# teste 3 -----------------------------------------------------------------



dmvnorm(x=c(0,0))
dmvnorm(x=c(0,0), mean=c(1,1))

sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
colMeans(x)
var(x)
dS <- dmvnorm(x, sigma = sigma)

### alternative interface
C <- t(chol(sigma))
(C <- ltMatrices(C[lower.tri(C, diag = TRUE)], diag = TRUE))
dC <- exp(ldmvnorm(obs = t(x), chol = C, logLik = FALSE))
all.equal(dS, dC)

x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
colMeans(x)
var(x)

plot(x)


# teste 4 -----------------------------------------------------------------


library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()
set.seed(17)
# Set the covariance matrix
sigma2 <- matrix(c(5, 2, 2, 5), ncol=2)
# Set the means
mu <- c(5,5)
# Get the correlation matrix
P <- cov2cor(sigma2)
# Generate the data
p <- rmvnorm(n=50, mean=mu, sigma=sqrt(sigma2))
# Plot the data
plot(p)
# Plot the ellipse
lines( ellipse( P, 
                centre = c(5,5)), 
       col='red')

evals <- eigen(P)$values
evecs <- eigen(P)$vectors

# Angles of a circle
a <- seq(0, 2*pi, len=100); a

# Get critical value
c2 <- qchisq(0.95, 2)
c <- sqrt(c2)

# Get the distances
xT <- c * sqrt(evals[1]) * cos(a)
yT <- c * sqrt(evals[2]) * sin(a)


M <- cbind(xT, yT)


# Covert the coordinates
transM <- evecs %*% t(M)
transM <- t(transM)

lines(transM + mu)

# teste 5 -----------------------------------------------------------------
  
library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()
set.seed(17)
# Set the covariance matrix
sigma2 <- matrix(c(5, 2, 2, 5), ncol=2)

# Set the means
mu <- c(5,5)
# Get the correlation matrix
P <- cov2cor(sigma2)
# Generate the data
p <- rmvnorm(n=50, mean=mu, sigma=sqrt(sigma2))

# Plot the data
plot(p)
# Plot the ellipse
lines( ellipse( P, centre = c(5,5)) , col='red')

evals <- eigen(P)$values
evecs <- eigen(P)$vectors

# Angles of a circle
a <- seq(0, 2*pi, len=100); a

# Get critical value
c2 <- qchisq(0.95, 2)
c <- sqrt(c2)

# Get the distances
xT <- c * sqrt(evals[1]) * cos(a)
yT <- c * sqrt(evals[2]) * sin(a)


M <- cbind(xT, yT)


# Covert the coordinates
transM <- evecs %*% t(M)
transM <- t(transM)

lines(transM + mu)


# teste 6 -----------------------------------------------------------------



# teste 5 -----------------------------------------------------------------

library(mvtnorm) # References rmvnorm()
library(ellipse) # References ellipse()
# Set the covariance matrix
sigma2 <- cov(base_bivariada)


# Set the means
mu <- as.vector(apply(base_bivariada, 2, mean))

# Get the correlation matrix
P <- round(cor(base_bivariada), 1)

# Generate the data
p <- base_bivariada

# Plot the data
plot(p, xlim = c(1,10))

# Plot the ellipse
lines(ellipse(P, centre = c(mu[1], mu[2])) , col='red')

evals <- eigen(P)$values
evecs <- eigen(P)$vectors

# Angles of a circle
a <- seq(0, 2*pi, len=100); a

# Get critical value
c2 <- qchisq(0.95, 2)
c <- sqrt(c2)

# Get the distances
xT <- c * sqrt(evals[1]) * cos(a)
yT <- c * sqrt(evals[2]) * sin(a)


M <- cbind(xT, yT)


# Covert the coordinates
transM <- evecs %*% t(M)
transM <- t(transM)

lines(transM + mu)


# teste 7 -----------------------------------------------------------------


# Example 2: simultaneous 90% confidence regions for bivariate data

bivar <- mvtnorm::rmvnorm(n=20, mean=rep(0.05, 2), sigma=toeplitz(c(0.05, 0.04)))
colnames(bivar) <- c("AUC", "Cmax")

hotelling <- cset(dat=bivar, 
                  method="hotelling",
                  alpha=0.1)
summary(hotelling)
plot(hotelling, main="90% Hotelling Region")

limacon <- cset(dat=bivar, method="limacon.asy", alpha=0.1)
summary(limacon)
plot(limacon, main="90% Limacon Region")

tseng <- cset(dat=bivar, method="tseng", alpha=0.1)
summary(tseng)
plot(tseng, main="90% Tseng Region")



# Example 2: simultaneous 90% confidence regions for bivariate data

bivar <- base_bivariada

hotelling <- jocre::cset(dat=bivar, method="hotelling", alpha=0.01)
summary(hotelling)
plot(hotelling, main="99% Hotelling Region",
     xlim = c(min(base_bivariada$Sepal.Length),
              max(base_bivariada$Sepal.Length)),
     ylim = c(min(base_bivariada$Sepal.Width),
              max(base_bivariada$Sepal.Width)))
points(base_bivariada)
points(mean(base_bivariada$Sepal.Length), 
       mean(base_bivariada$Sepal.Width)
       , pch = 16, col = "red")
points(medias[1] + eixos_vertices[1,1],
       medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")

# hotelling <- cset(dat=bivar, method="hotelling", alpha=0.1)
# summary(hotelling)
# plot(hotelling, main="90% Hotelling Region",
#      xlim = c(min(base_bivariada$Petal.Length),
#               max(base_bivariada$Petal.Length)),
#      ylim = c(min(base_bivariada$Sepal.Length),
#               max(base_bivariada$Sepal.Length)))

points(base_bivariada)
points(medias[1] + eixos_vertices[1,1],
       medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")

points(medias[1] - eixos_vertices[1,1],
       medias[2] - eixos_vertices[2,1], pch = 16, col = "blue")

points(medias[1] + eixos_vertices[1,2],
       medias[2] + eixos_vertices[2,2], pch = 16, col = "blue")

points(medias[1] - eixos_vertices[1,2],
       medias[2] - eixos_vertices[2,2], pch = 16, col = "blue")


# teste 8 -----------------------------------------------------------------


x.bar <- ability.cov$center[5:6]
Sigma <- ability.cov$cov[5:6,5:6]
n <- ability.cov$n.obs
p <- length(ability.cov$center)

MVQuickGraphs::confidenceEllipse(X.mean = x.bar,
                  eig = eigen(Sigma),
                  n = n, p = p,
                  alpha = 0.10)


x.bar <- as.vector(medias)
Sigma <- matriz_cov
n <- n
p <- p


MVQuickGraphs::confidenceEllipse(X.mean = x.bar,
                                 eig = eigen(Sigma),
                                 n = n, p = p,
                                 alpha = 0.01, 
                                 xl = c(min(base_bivariada$Sepal.Length),
                                        max(base_bivariada$Sepal.Length)),
                                 yl = c(min(base_bivariada$Sepal.Width),
                                        max(base_bivariada$Sepal.Width))
                                 )

points(base_bivariada$Sepal.Length,
     base_bivariada$Sepal.Width, pch = 20
)
points(mean(base_bivariada$Sepal.Length), 
       mean(base_bivariada$Sepal.Width)
       , pch = 18, col = "red")
points(medias[1] + eixos_vertices[1,1],
       medias[2] + eixos_vertices[2,1], pch = 18, col = "blue")

points(medias[1] - eixos_vertices[1,1],
       medias[2] - eixos_vertices[2,1], pch = 18, col = "blue")

points(medias[1] + eixos_vertices[1,2],
       medias[2] + eixos_vertices[2,2], pch = 18, col = "blue")

points(medias[1] - eixos_vertices[1,2],
       medias[2] - eixos_vertices[2,2], pch = 18, col = "blue")



# teste 3 - trivariado ----------------------------------------------------



