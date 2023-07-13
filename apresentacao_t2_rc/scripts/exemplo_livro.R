n <- 42
p <- 2
matriz_cov <- cbind(c(.0144, 0.0117), c(0.0117, 0.0146))
matriz_inv_cov <- solve(matriz_cov)

medias <- c(.564,.603)
valorF <- round(qf(0.95,p, n - p),2)

autovalores <- eigen(matriz_cov)$values 
autovetores <- eigen(matriz_cov)$vectors %>% round(3)# cada coluna é um autovetor


eixos_tamanhos <- c()
eixos_vertices <- c()
for (i in 1:p) {
  tamanho <- sqrt(autovalores[i]) * 
    sqrt( ( p * (n - 1) / (n * (n - p)) ) * 
            valorF) 
  
  vertices <- tamanho * autovetores[,i]
  eixos_tamanhos <- cbind(eixos_tamanhos, tamanho)
  eixos_vertices <- cbind(eixos_vertices, vertices)
  
}

eixos_tamanhos
eixos_vertices
sqrt(max(autovalores))/ sqrt(min(autovalores))



plot(medias[1], medias[2])
points(medias[1], medias[2]
       , pch = 16, col = "red", x = lim)
points(medias[1] + eixos_vertices[1,1],
       medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")

points(medias[1] - eixos_vertices[1,1],
       medias[2] - eixos_vertices[2,1], pch = 16, col = "blue")

points(medias[1] + eixos_vertices[1,2],
       medias[2] + eixos_vertices[2,2], pch = 16, col = "blue")

points(medias[1] - eixos_vertices[1,2],
       medias[2] - eixos_vertices[2,2], pch = 16, col = "blue")


