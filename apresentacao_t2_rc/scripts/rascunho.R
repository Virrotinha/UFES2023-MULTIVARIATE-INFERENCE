rm(list = ls())

library(dplyr)


# teste de normalidade univariado -----------------------------------------

shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)



# tratamento inicial ------------------------------------------------------

base <- iris %>% 
  select(-Species)

base_bivariada <- base %>% 
  select(Sepal.Length, Sepal.Width)


# valores -----------------------------------------------------------------

vetor_medias <- as.vector(rbind(mean(base_bivariada$Sepal.Length), 
                                mean(base_bivariada$Sepal.Width)))


n <- nrow(base_bivariada)
p <- ncol(base_bivariada)
matriz_cov <- cov(base_bivariada)
matriz_inv_cov <- solve(matriz_cov)

alpha <- 0.01
valorF <- qf(1-alpha, p, n - p)

autovalores <- eigen(matriz_cov)$values
autovetores <- eigen(matriz_cov)$vectors # cada coluna é um autovetor

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


# indicador da proporção entre os eixos -----------------------------------

sqrt(max(autovalores))/ sqrt(min(autovalores))

# intervalo de simultaneo não corrigido -----------------------------------

intervalos <- c()
for (i in 1:p) {
  estatistica <- sqrt( ((p * (n-1)) / (n-p)) * valorF) * sqrt(matriz_cov[i,i]/n)
  lim_inf <- vetor_medias[i]-estatistica
  lim_sup <- vetor_medias[i]+estatistica
  limites <- c(lim_inf,lim_sup )
  intervalos <- rbind(intervalos,limites)
}

rownames(intervalos) <- rownames(matriz_cov)
colnames(intervalos) <- c("Limite Inferior", "Limite Superior")

# intervalo de simultaneo corrigido ---------------------------------------

m <- p
valorT <- qt(1 - (alpha/(2*m)), df = n-1)

intervalos_bonferroni <- c()
for (i in 1:p) {
  estatistica <- valorT * sqrt(matriz_cov[i,i]/n)
  lim_inf <- vetor_medias[i] - estatistica
  lim_sup <- vetor_medias[i] + estatistica
  limites <- c(lim_inf, lim_sup)
  intervalos_bonferroni <- rbind(intervalos_bonferroni,limites)
}

rownames(intervalos_bonferroni) <- rownames(matriz_cov)
colnames(intervalos_bonferroni) <- c("Limite Inferior", "Limite Superior")



# plot1 -------------------------------------------------------------------


# plot(base_bivariada$Petal.Length,
#      base_bivariada$Sepal.Length)
# points(mean(base_bivariada$Petal.Length), 
#        mean(base_bivariada$Sepal.Length)
#        , pch = 16, col = "red")
# points(vetor_medias[1] + eixos_vertices[1,1],
#        vetor_medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")
# 
# points(vetor_medias[1] - eixos_vertices[1,1],
#        vetor_medias[2] - eixos_vertices[2,1], pch = 16, col = "blue")
# 
# points(vetor_medias[1] + eixos_vertices[1,2],
#        vetor_medias[2] + eixos_vertices[2,2], pch = 16, col = "blue")
# 
# points(vetor_medias[1] - eixos_vertices[1,2],
#        vetor_medias[2] - eixos_vertices[2,2], pch = 16, col = "blue")


# vertices ----------------------------------------------------------------


# vetor_medias[1] + eixos[1,1]
# vetor_medias[2] + eixos[2,1]
# 
# vetor_medias[1] - eixos[1,1]
# vetor_medias[2] - eixos[2,1]
# 
# vetor_medias[1] + eixos[1,2]
# vetor_medias[2] + eixos[2,2]
# 
# 
# vetor_medias[1] - eixos[1,2]
# vetor_medias[2] - eixos[2,2]



# plot4 -------------------------------------------------------------------


# plot(base_bivariada$Petal.Length,
#      base_bivariada$Sepal.Length, 
#      xlim = c(3,4.5), 
#      ylim = c(5.5,6.3))
# points(mean(base_bivariada$Petal.Length), 
#        mean(base_bivariada$Sepal.Length)
#        , pch = 16, col = "red")
# points(vetor_medias[1] + eixos_vertices[1,1],
#        vetor_medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")
# 
# points(vetor_medias[1] - eixos_vertices[1,1],
#        vetor_medias[2] - eixos_vertices[2,1], pch = 16, col = "blue")
# 
# points(vetor_medias[1] + eixos_vertices[1,2],
#        vetor_medias[2] + eixos_vertices[2,2], pch = 16, col = "blue")
# 
# points(vetor_medias[1] - eixos_vertices[1,2],
#        vetor_medias[2] - eixos_vertices[2,2], pch = 16, col = "blue")
# 



plot(base_bivariada$Sepal.Length,
     base_bivariada$Sepal.Width, 
     # xlim = c(3,4.5), 
     # ylim = c(5.5,6.3)
     )
points(mean(base_bivariada$Sepal.Length), 
       mean(base_bivariada$Sepal.Width)
       , pch = 16, col = "red")
points(vetor_medias[1] + eixos_vertices[1,1],
       vetor_medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")

points(vetor_medias[1] - eixos_vertices[1,1],
       vetor_medias[2] - eixos_vertices[2,1], pch = 16, col = "blue")

points(vetor_medias[1] + eixos_vertices[1,2],
       vetor_medias[2] + eixos_vertices[2,2], pch = 16, col = "blue")

points(vetor_medias[1] - eixos_vertices[1,2],
       vetor_medias[2] - eixos_vertices[2,2], pch = 16, col = "blue")



# GRAFICO MVQUICKGRAPHS ---------------------------------------------------


# x.bar <- as.vector(vetor_medias)
# Sigma <- matriz_cov

# sem pontos
MVQuickGraphs::confidenceEllipse(X.mean = vetor_medias,
                                 eig = eigen(matriz_cov),
                                 n = n, p = p,
                                 alpha = 0.01, 
)
points(mean(base_bivariada$Sepal.Length), 
       mean(base_bivariada$Sepal.Width)
       , pch = 18, col = "red")
points(vetor_medias[1] + eixos_vertices[1,1],
       vetor_medias[2] + eixos_vertices[2,1], pch = 18, col = "blue")

points(vetor_medias[1] - eixos_vertices[1,1],
       vetor_medias[2] - eixos_vertices[2,1], pch = 18, col = "blue")

points(vetor_medias[1] + eixos_vertices[1,2],
       vetor_medias[2] + eixos_vertices[2,2], pch = 18, col = "blue")

points(vetor_medias[1] - eixos_vertices[1,2],
       vetor_medias[2] - eixos_vertices[2,2], pch = 18, col = "blue")

## com os pontos

MVQuickGraphs::confidenceEllipse(X.mean = vetor_medias,
                                 eig = eigen(matriz_cov),
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
points(vetor_medias[1] + eixos_vertices[1,1],
       vetor_medias[2] + eixos_vertices[2,1], pch = 18, col = "blue")

points(vetor_medias[1] - eixos_vertices[1,1],
       vetor_medias[2] - eixos_vertices[2,1], pch = 18, col = "blue")

points(vetor_medias[1] + eixos_vertices[1,2],
       vetor_medias[2] + eixos_vertices[2,2], pch = 18, col = "blue")

points(vetor_medias[1] - eixos_vertices[1,2],
       vetor_medias[2] - eixos_vertices[2,2], pch = 18, col = "blue")



# grafico JOCRE -----------------------------------------------------------

# Example 2: simultaneous 90% confidence regions for bivariate data
# 
# bivar <- base_bivariada
# 
# hotelling <- jocre::cset(dat=bivar, method="hotelling", alpha=0.01)
# summary(hotelling)
# plot(hotelling, main="99% Hotelling Region",
#      xlim = c(min(base_bivariada$Sepal.Length),
#               max(base_bivariada$Sepal.Length)),
#      ylim = c(min(base_bivariada$Sepal.Width),
#               max(base_bivariada$Sepal.Width)))
# points(base_bivariada)
# points(mean(base_bivariada$Sepal.Length),
#        mean(base_bivariada$Sepal.Width)
#        , pch = 16, col = "red")
# points(vetor_medias[1] + eixos_vertices[1,1],
#        vetor_medias[2] + eixos_vertices[2,1], pch = 16, col = "blue")


# grafico de intervalos simultaneos ---------------------------------------


MVQuickGraphs::confidenceEllipse(X.mean = vetor_medias,
                                 eig = eigen(matriz_cov),
                                 n = n, p = p,
                                 alpha = 0.01
)
abline(v = intervalos[1,1], h = intervalos[2,1], lwd=1, lty=3, col = "blue")
abline(v = intervalos[1,2], h = intervalos[2,2], lwd=1, lty=3, col = "blue")
abline(v = intervalos_bonferroni[1,1], h = intervalos_bonferroni[2,1], lwd=1, lty=3, col = "red")
abline(v = intervalos_bonferroni[1,2], h = intervalos_bonferroni[2,2], lwd=1, lty=3, col = "red")


