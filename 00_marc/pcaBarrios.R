library(factoextra)
library(FactoMineR)
library(knitr)

dataBarrios <- read.csv("data/info_general_barrio_final.csv", sep = ",")
dataBarrios[is.na(dataBarrios)] <- 0
rownames(dataBarrios) <- dataBarrios$barrio
dataBarrios2 <- subset(dataBarrios, select = -c(barrio, geo_shape, X))
# PCA 

res.pca = PCA(dataBarrios2, scale.unit = TRUE, ncp = Inf, graph = FALSE, quanti.sup = c("X0_15_años", "X16_64_años", "X65_o_más"), quali.sup = "distrito")

eig.val <- get_eigenvalue(res.pca)

VPmedio = 100 * (1/nrow(eig.val))

fviz_eig(res.pca, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

kable(eig.val)

K=4  #75.16546|

res.pca = PCA(dataBarrios2, scale.unit = TRUE, ncp = K, graph = FALSE, quanti.sup = c("X0_15_años", "X16_64_años", "X65_o_más"), quali.sup = "distrito")

misScores = res.pca$ind$coord[,1:K]
miT2 = colSums(t(misScores**2)/eig.val[1:K,1])
I = nrow(dataBarrios)
F95 = K*(I**2 - 1)/(I*(I - K)) * qf(0.95, K, I-K)
F99 = K*(I**2 - 1)/(I*(I - K)) * qf(0.99, K, I-K) #15.2

plot(1:length(miT2), miT2, type = "p", xlab = "Barrios", ylab = "T2")
abline(h = F95, col = "orange", lty = 2, lwd = 2)
abline(h = F99, col = "red3", lty = 2, lwd = 2)


sort(miT2, decreasing=TRUE)
anomalos=which(miT2>F99)

anomalos
miT2[anomalos]

max = which.max(miT2)
max
miT2[max]

dataBarrios2[max,]

contribT2 = function (X, scores, loadings, eigenval, observ, cutoff = 2) {
  # X is data matrix and must be centered (or centered and scaled if data were scaled)
  misScoresNorm = t(t(scores**2) / eigenval)
  misContrib = NULL
  for (oo in observ) {
    print(rownames(misScores)[oo])
    print(misScores[oo,])
    misPCs = which(as.numeric(misScoresNorm[oo,]) > cutoff)
    lacontri = sapply(misPCs, function (cc) (misScores[oo,cc]/eigenval[cc])*loadings[,cc]*X[oo,])
    lacontri = rowSums((1*(sign(lacontri) == 1))*lacontri)
    misContrib = cbind(misContrib, lacontri)
  }
  colnames(misContrib) = rownames(misScoresNorm[observ,])
  return(misContrib)
}

barriosCE = subset(dataBarrios2, select=-c(X0_15_años, X16_64_años, X65_o_más, distrito))
barriosCE = scale(barriosCE, center = TRUE, scale = TRUE)
X = as.matrix(barriosCE)
# Calculamos los loadings a partir de las coordenadas de las variables
# ya que la librería FactoMineR nos devuelve los loadings ponderados 
# por la importancia de cada componente principal.
misLoadings = sweep(res.pca$var$coord, 2, sqrt(res.pca$eig[1:K,1]), FUN="/")
# Calculamos las contribuciones
mycontrisT2 = contribT2(X = X, scores = misScores, loadings = misLoadings, 
                        eigenval = eig.val[1:K,1], observ = which(miT2>F99),
                        cutoff = 2)

kable(mycontrisT2)

miT2[anomalos]

#LA PUNTA, EL GRAU, BENICALP, RUSSAFA,

par(mar = c(10,2.3,3,1))
barplot(mycontrisT2[,3],las=2, #cex.names = 0.5,
        main= paste0("Observación: ", "LA PUNTA"))#GRANDES #35.42

barplot(mycontrisT2[,2],las=2, #cex.names = 0.5,
        main = "Observación: EL GRAU")# GRANDES #21.69

barplot(mycontrisT2[,1],las=2, #cex.names = 0.5,
        main = "Observación: BENICALP") #Poblacion y plazas_mob #18.32

barplot(mycontrisT2[,4],las=2, #cex.names = 0.5,
        main = "Observación: RUSSAFA") #POR MONUMENTS #21.255

mycontrisT2[,2]

barrios_sin_mierda1<-subset(dataBarrios2, !(rownames(dataBarrios2) == c("LA PUNTA")))
barrios_sin_mierda2<-subset(dataBarrios2, !(rownames(dataBarrios2) == c("LA PUNTA", "EL GRAU")))
barrios_sin_mierda3<-subset(dataBarrios2, !(rownames(dataBarrios2) == c("LA PUNTA", "EL GRAU", "BENICALP", "RUSSAFA")))


res.pca1 = PCA(barrios_sin_mierda1, scale.unit = TRUE, ncp = 4, graph = FALSE, quanti.sup = c("X0_15_años", "X16_64_años", "X65_o_más"), quali.sup = "distrito")
res.pca2 = PCA(barrios_sin_mierda2, scale.unit = TRUE, ncp = 4, graph = FALSE, quanti.sup = c("X0_15_años", "X16_64_años", "X65_o_más"), quali.sup = "distrito")
res.pca3 = PCA(barrios_sin_mierda3, scale.unit = TRUE, ncp = 4, graph = FALSE, quanti.sup = c("X0_15_años", "X16_64_años", "X65_o_más"), quali.sup = "distrito")

eig.val1 <- get_eigenvalue(res.pca1)
eig.val2 <- get_eigenvalue(res.pca2)
eig.val3 <- get_eigenvalue(res.pca3)

VPmedio = 100 * (1/nrow(eig.val))

library(grid)
library(gridExtra)

p1 <- fviz_eig(res.pca1, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

p2 <- fviz_eig(res.pca2, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

p3 <- fviz_eig(res.pca3, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

grid.arrange(p1, p2, p3, ncol=3)

kable(eig.val1)
kable(eig.val2)
kable(eig.val3)

#quitando solo la punta --> 76.07290| ESTA LA BUENA
#quitando la punta y el grau -->   75.16546|
#quitando todos los lim99 -->75.16546|

res.pca1 = PCA(barrios_sin_mierda1, scale.unit = TRUE, ncp = 4, graph = FALSE, quanti.sup = c("X0_15_años", "X16_64_años", "X65_o_más"), quali.sup = "distrito")

myE = X - misScores %*% t(misLoadings) 

mySCR = rowSums(myE^2)

g = var(mySCR)/(2*mean(mySCR))
h = (2*mean(mySCR)^2)/var(mySCR)

chi2lim = g*qchisq(0.95, df = h)#13.7
chi2lim99 = g*qchisq(0.99, df = h)#22.03 


plot(1:length(mySCR), mySCR, type = "l", main = "Distancia al modelo", ylab = "SCR", xlab = "Cereales")
abline(h = chi2lim, col = "orange", lty = 2, lwd = 2)
abline(h = chi2lim99, col = "red3", lty = 2, lwd = 2)

raros = which(mySCR > chi2lim)
raros
mySCR[raros]

fviz_pca_var(res.pca1, axes = c(1,2), repel = TRUE, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_pca_var(res.pca1, axes = c(3,4), repel = TRUE, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_pca_ind(res.pca1, axes = c(1,2), geom = c("point", "text"), habillage = "distrito", repel = TRUE, labelsize = 2)

fviz_pca_ind(res.pca1, axes = c(3,4), geom = c("point", "text"), habillage = "distrito", repel = TRUE, labelsize = 2)

