iris$Petal.Length

media <- mean(iris$Petal.Length)
variancia <- (iris$Petal.Length - media)^2
variancia_amostral <- sum(variancia)/(n-1)
n <- length(iris$Petal.Length)
media + qnorm(0.95,0,1)


media + (qt(.95, df = n-1) *(sqrt(variancia_amostral)/ sqrt(n)))
media - (qt(.95, df = n-1) * (sqrt(variancia_amostral)/ sqrt(n)))

media

# Grafico com area destacada
x <- seq(70, 130, l = 200) 
fx <- fn(x) 
plot(x, fx, type = "l") 
ax <- c(70, 70, x[x <= 95], 95, 95) 
ay <- c(0, fn(70), fx[x <= 95], fn(95), 0) 
polygon(ax, ay, dens = 10)

k <- iris$Petal.Length
f1 = function(x) stats::dt(x, n-1)
plot(f1,to = 4,-4, ylab = "f(x)", lwd = 3)


ax <- c(70, 70, x[x <= qt(.95, df = n-1)], qt(.95, df = n-1)
, qt(.95, df = n-1)
) 
ay <- c(0, fn(70), fx[x <= 95], fn(95), 0) 
polygon(ax, ay, dens = 10)


fn <- function(x) { 
  fx <- (1/sqrt(2 * pi * 100)) * exp((-1/200) * (x - 100)^2) 
  return(fx) 
}

# Grafico com area destacada
x <- seq(70, 130, l = 200) 
fx <- fn(x) 
plot(x, fx, type = "l") 
ax <- c(70, 70, x[x <= 95], 95, 95) 
ay <- c(0, fn(70), fx[x <= 95], fn(95), 0) 
polygon(ax, ay, dens = 10)

fn(70)
