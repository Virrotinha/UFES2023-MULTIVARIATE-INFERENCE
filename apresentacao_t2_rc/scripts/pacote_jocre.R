# install.packages("jocre")
library(jocre)
library(dplyr)
marzo
plot(cset(dat=marzo, method="limacon"))
data("wires")
plot(csetMV(dat=iris[,1:2], method="mood"))

data("marzo")
cset(dat=iris[,1:2], method = "")

marzo  %>% mutate()


trivar <- mvtnorm::rmvnorm(n=20, 
                           mean=rep(0.05, 3), 
                           sigma=toeplitz(c(0.05, 0.04, 0.03)))
colnames(trivar) <- c("AUCinf", "AUCt", "Cmax")
trivar
tost <- cset(dat=trivar, 
             method="tost", 
             alpha=0.5)
summary(tost)

tost <- cset(dat=base_bivariada, 
             method="tost", 
             alpha=0.1)
summary(tost)


library(MVQuickGraphs)
confidenceEllipse(X.mean = vetor_medias,
                  eig = eigen(matriz_cov),
                  n = n, 
                  p = p,
                  alpha = alpha)
title("Intervalos de Confiança Simultâneos com 99% de Confiança",
      xlab = "Sepal Lenght",
      ylab = "Sepal Width")

abline(v = intervalos[1,], 
       h = intervalos[2,], 
       lwd=1, lty=3, col = "blue")

abline(v = intervalos_bonferroni[1,], 
       h = intervalos_bonferroni[2,], 
       lwd=1, lty=3, col = "red")

legend("bottomright", 
       legend = c("Teste de Hotteling", "Bonferroni"),
       lwd = c(1,1), lty=3, col = c("blue", "red"))


abline(v = intervalos_bonferroni[1,2],
       h = intervalos_bonferroni[2,2],
       lwd=1, lty=3, col = "red")
legend("bottomright", legend = c("Teste de Hotteling", "Bonferroni"),
       lwd = c(1,1), lty=3, col = c("blue", "red"))
