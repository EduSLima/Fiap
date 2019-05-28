
install.packages("corrgram")
install.packages("tclust")
install.packages("cluster")
install.packages("fpc")
# Ler arquivo csv

carros <- read.table ("F:/LABIA&ML2018/AtividadeAulaR/Arquivo_AvaliaçãoAutomóveis_2.csv", sep=";", row.names=1 , header=T)
View(carros)

#mostrar as variï¿½veis
str(carros)
#mostra as variï¿½veis
names(carros)

#Estatï¿½sticas descritivas
summary(carros)

# Analise de componentes principais excluindo variï¿½veis
partcarros <- carros[,c(2,3,4,5,6,7,8,9,10,11,12)]


attach(partcarros)

str(partcarros)
#comando para gerar em 3 linhas e 4 colunas os histogramas
par (mfrow=c(3,4))
hist(cilindr)
hist(PotLiqMx)
hist(TorqLqMx)
hist(Ac_0_100)
hist(Vel_Max)
hist(Comp)
hist(Disteixo)
hist(Larg)
hist(Alt)
hist(Volcarga)
hist(Tanque)
par (mfrow=c(1,1))
     

# matriz de correlaï¿½ï¿½es
matcor <- cor(partcarros)
print(matcor, digits = 2)

library(corrgram)
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

panel.cor <- function(x, y, digits=2, prefix ="", cex.cor,
                      ...)  {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y , use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits) [1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  # abs(r) ï¿½ para que na saï¿½da as correlaï¿½ï¿½es ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
#pdf(file = "grafico.pdf")
pairs(partcarros, lower.panel=panel.smooth, upper.panel=panel.cor)
 
     
# padronuizar!!!!

geraclus_car_car <- scale(partcarros)
summary(geraclus_car)
#mï¿½todo hierarquico

hier_cluster<-hclust(dist(geraclus_car),method='ward.D2')
d <- dist(geraclus_car, method = "euclidean") # distance matrix
plot(hier_cluster, ylab='distancia', cex=0.6)

groups <- cutree(hier_cluster, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=5, border="red") 

groups <- cutree(hier_cluster, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=3, border="blue") 

# Outros mï¿½todos que podem ser usados sï¿½o: "ward", "single", "complete", "average", "mcquitty", "median" ou "centroid".
# A definiï¿½ï¿½o de qual mï¿½todo usar varia com o objetivo do estudo e com o tipo de matriz de distï¿½ncia usada.



# Determine number of clusters
wss <- (nrow(geraclus_car)-1)*sum(apply(geraclus_car,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(geraclus_car,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



attach(geraclus_car)
library(tclust)
clus_teste <- tkmeans(geraclus_car, k = 4, alpha = 0.03)
plot(clus_teste)


set.seed(33)
output_cluster<-kmeans(geraclus_car,5)
segmento<-output_cluster$cluster
table (segmento)

# Mostrando Resulados
aggregate(geraclus_car,by=list(segmento),FUN=mean)


# Mostrando Resultados em grï¿½ficos

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(geraclus_car, output_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0 , cex=0.75)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(geraclus_car, output_cluster$cluster) 

matriz_fim<-cbind(carros,segmento)
fix(matriz_fim)

aggregate(matriz_fim,by=list(segmento),FUN=mean)



write.table(file='E:/LabBDT2018/fator_carros_clus.csv',matriz_fim, sep=';',dec=',')





### Componentes principais ###

acpcor_car_car <- prcomp(geraclus_car, scale = TRUE) 
summary(acpcor_car)

plot(1:ncol(geraclus_car), acpcor_car$sdev^2, type = "b", xlab = "Componente",
     ylab = "Variância", pch = 20, cex.axis = 0.8, cex.lab = 0.8)

sum(acpcor_car$sdev^2)

acpcor_car$rotation[, 1:11]

biplot(acpcor_car, xlab = "CP1", ylab = "CP2",cex.lab = 1.0, cex.axis = 1.0)

acpcor_car <- prcomp(geraclus_car, scale = TRUE, retx = TRUE)

escore1 <- acpcor_car$x[, 1]
print(escore1)
hist(escore1)

escore2 <- acpcor_car$x[, 2]

escore3 <- acpcor_car$x[, 3]

par (mfrow=c(1,3))
hist(escore1)
hist(escore2)
hist(escore3)
par (mfrow=c(1,1))


carros_cpa <-cbind(escore1,escore2,escore3)




# usar os escores em uma segmentação, por exemplo.

#método hierarquico

hier_cluster<-hclust(dist(carros_cpa),method='ward.D2')
d <- dist(carros_cpa, method = "euclidean") # distance matrix
plot(hier_cluster, ylab='distancia', cex=0.6)

groups <- cutree(hier_cluster, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=5, border="blue") 

# Outros métodos que podem ser usados são: "ward", "single", "complete", "average", "mcquitty", "median" ou "centroid".
# A definição de qual método usar varia com o objetivo do estudo e com o tipo de matriz de distância usada.




#método não hierarquico

# Determine number of clusters
wss <- (nrow(carros_cpa )-1)*sum(apply(carros_cpa ,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(carros_cpa ,iter.max=100,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



attach(carros_cpa)

set.seed(333)
output_cluster<-kmeans(carros_cpa,5,iter=100)
output_cluster

centros<-output_cluster$centers
centros

clus_carros<-output_cluster$cluster
table (clus_carros)

matrizcarros_cpa<-cbind(carros,carros,clus_carros)

matriz_juntos<-cbind(carros,segmento,clus_carros)

table(segmento,clus_carros)






