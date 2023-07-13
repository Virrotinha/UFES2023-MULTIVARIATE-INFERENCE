#R code for 3D confidence error intervals
# install.packages("plotly")
library(plotly)

# Generate Simulated Data Points
x <- seq(-5,5, .1)
y <- seq(0, 10, .1)
y <- cbind(y, sin(y), cos(y))

# Generate Simulated Data Values
z1 = sin(x+y[,1])
z2 = sin((x+y[,1])/2.)
z3 = sin((x+y[,1])/3.)

z <- cbind(z1, z2, z3)
# Generate Simulated Standard Deviations
sd <- sqrt(abs(z) * .05)

n = length(x)

# Create Plots for each of three simulated trajectories
p <- plot_ly(type = 'scatter3d')
for (index in 1:3){
  p <- add_trace(p, x = x, y = y[,index], z = z[,index], mode = 'lines', line = list(width = 8, color=index))
  p <- add_trace(p, type = 'mesh3d',
                 # Setup triangle vertices
                 x = c(x, x),
                 y = c(y[,index], y[,index]),
                 z = c(z[,index] - 2 * sd[,index], z[,index] + 2 * sd[,index]),
                 # Create triangles
                 i = c(i[1:n - 1], i[1:n - 1]),
                 j = c(n + i[1:n - 1], n + i[2:n]) ,
                 k = c(n + i[2:n], i[2:n]),
                 color = index
  )
}

p
