# limpar memória do R
rm(list=ls(all=TRUE))

# mostrar até 2 casas decimais
options("scipen" = 2)
 

# Ler arquivo csv

paises <- read.csv("E:/LABIA&ML2018/AtividadeAulaR/DADOS_Papercsv_1.csv", row.names=1, sep=";")
fix(paises)

#Verificando o formato das variáveis
str(paises)

#Estatísticas descritivas
summary(paises)


par (mfrow=c(1,2))
hist(paises$p100ms)
boxplot(paises$p100ms)
par (mfrow=c(1,1))



#comando para gerar em 4 linhas e duas colunas os histogramas
par (mfrow=c(3,3))

hist(paises$p100ms)
hist(paises$p200ms)
hist(paises$p400ms)
hist(paises$p800mm)
hist(paises$p1500mm)
hist(paises$p3000mm)
hist(paises$pmaratm)

par (mfrow=c(3,3))

boxplot(paises$p100ms)
boxplot(paises$p200ms)
boxplot(paises$p400ms)
boxplot(paises$p800mm)
boxplot(paises$p1500mm)
boxplot(paises$p3000mm)
boxplot(paises$pmaratm)


boxplot.stats(paises$p100ms)
boxplot.stats(paises$p200ms)$out
boxplot.stats(paises$p400ms)$out
boxplot.stats(paises$p800mm)$out
boxplot.stats(paises$p1500mm)$out
boxplot.stats(paises$p3000mm)$out
boxplot.stats(paises$pmaratm)$out



par (mfrow=c(2,3))
plot (paises$p100ms,paises$p200ms)
plot (paises$p100ms,paises$p400ms)
plot (paises$p100ms,paises$p800mm)
plot (paises$p100ms,paises$p1500mm)
plot (paises$p100ms,paises$p3000mm)
plot (paises$p100ms,paises$pmaratm)

par (mfrow=c(2,3))
plot (paises$p200ms,paises$p400ms)
plot (paises$p200ms,paises$p800mm)
plot (paises$p200ms,paises$p1500mm)
plot (paises$p200ms,paises$p3000mm)
plot (paises$p200ms,paises$pmaratm)

par (mfrow=c(2,2))
plot (paises$p400ms,paises$p800mm)
plot (paises$p400ms,paises$p1500mm)
plot (paises$p400ms,paises$p3000mm)
plot (paises$p400ms,paises$pmaratm)

par (mfrow=c(2,3))
plot (paises$p800mm,paises$p1500mm)
plot (paises$p800mm,paises$p3000mm)
plot (paises$p800mm,paises$pmaratm)
plot (paises$p1500mm,paises$p3000mm)
plot (paises$p1500mm,paises$pmaratm)
plot (paises$p3000mm,paises$pmaratm)

par (mfrow=c(1,1))



par (mfrow=c(1,1))
# matriz de correlações
matcor <- cor(paises)
print(matcor, digits = 2)
 
install.packages("corrgram")
library(corrgram)
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

install.packages("corrplot")
library(corrplot)

corrplot::corrplot(matcor, method="circle", order="hclust")

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
# abs(r) é para que na saída as correlações ficam proporcionais
    text(0.5, 0.5, txt, cex = cex * abs(r))
}


#pdf(file = "grafico.pdf")
pairs(paises, lower.panel=panel.smooth, upper.panel=panel.cor)


# criar grupos de paises de  acordo com o desempenho dos recoredes femininos.

Padr_paises <- scale(paises)

fix(Padr_paises)


summary(Padr_paises)

#método hierarquico

hier_cluster<-hclust(dist(Padr_paises),method='ward.D2')
d <- dist(Padr_paises, method = "euclidean") # distance matrix
plot(hier_cluster, ylab='distancia', cex=0.6)

groups <- cutree(hier_cluster, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=5, border="red") 

groups <- cutree(hier_cluster, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=6, border="blue") 

# Outros métodos que podem ser usados são: "ward", "single", "complete", "average", "mcquitty", "median" ou "centroid".
# A definição de qual método usar varia com o objetivo do estudo e com o tipo de matriz de distância usada.

#método não hierarquico

# Determine number of clusters - Elbow method
wss <- (nrow(Padr_paises )-1)*sum(apply(Padr_paises ,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Padr_paises ,iter.max=100,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


# utilizando uma forma gráfica
install.packages("tclust")
library(tclust)
clus_teste <- tkmeans(Padr_paises , k = 5, alpha = 0.01)
plot(clus_teste)

# Gerando a quantidade de cluster com Kmeans
attach(Padr_paises)
set.seed(333)
output_cluster<-kmeans(Padr_paises,5,iter=100)
output_cluster

# quantas entidade dentro de cada cluster
segmento<-output_cluster$cluster
table (segmento)

# quais características  de cada cluster
centros<-output_cluster$centers
centros

# quantas rodadas até chegar nos clusters
Qte_iter<-output_cluster$iter
Qte_iter


# Mostrando Resultados
aggregate(paises,by=list(segmento),FUN=mean)


# Mostrando Resultados em gráficos

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
install.packages("cluster")

library(cluster)
clusplot(Padr_paises, output_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0 , cex=0.75)

# Centroid Plot against 1st 2 discriminant functions
install.packages("fpc")
library(fpc)
plotcluster(Padr_paises, output_cluster$cluster) 


# junta os arquivos em colunas
matriz<-cbind(paises,Padr_paises,segmento)
fix(matriz)

# append cluster assignment
matriz<- data.frame(arq01,Padr_paises, segmento) 
fix(matriz)







# Componentes Principais.
acpcor <- prcomp(Padr_paises, scale = TRUE) 
summary(acpcor)

plot(1:ncol(Padr_paises), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Variância", pch = 20, cex.axis = 0.8, cex.lab = 0.8)

sum(acpcor$sdev^2)

acpcor$rotation[, 1:7]

biplot(acpcor, xlab = "CP1", ylab = "CP2",cex.lab = 1.0, cex.axis = 1.0)

acpcor <- prcomp(Padr_paises, scale = TRUE, retx = TRUE)

escore1 <- acpcor$x[, 1]
print(escore1)
hist(escore1)

escore2 <- acpcor$x[, 2]

par (mfrow=c(1,2))
hist(escore1)
hist(escore2)
par (mfrow=c(1,1))

attach(paises)
paises_cpa <-cbind(escore1,escore2)
 



# usar os escores em uma segmentação, por exemplo.

#método hierarquico

hier_cluster<-hclust(dist(paises_cpa),method='ward.D2')
d <- dist(paises_cpa, method = "euclidean") # distance matrix
plot(hier_cluster, ylab='distancia', cex=0.6)

groups <- cutree(hier_cluster, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hier_cluster, k=5, border="blue") 
 
# Outros métodos que podem ser usados são: "ward", "single", "complete", "average", "mcquitty", "median" ou "centroid".
# A definição de qual método usar varia com o objetivo do estudo e com o tipo de matriz de distância usada.




#método não hierarquico

# Determine number of clusters
wss <- (nrow(paises_cpa )-1)*sum(apply(paises_cpa ,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(paises_cpa ,iter.max=100,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



attach(paises_cpa)

set.seed(333)
output_cluster<-kmeans(paises_cpa,5,iter=100)
output_cluster

centros<-output_cluster$centers
centros

clus_paises<-output_cluster$cluster
table (clus_paises)

matriz_cpa<-cbind(paises,paises_cpa,clus_paises)

matriz_juntos<-cbind(paises,segmento,clus_paises)

table(segmento,clus_paises)

aggregate(paises,by=list(clus_paises),FUN=mean)


# Mostrando Resultados
aggregate(matriz_cpa,by=list(clus_paises),FUN=mean)


# Mostrando Resultados em gráficos

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph'
install.packages("cluster")
library(cluster)
clusplot(Padr_paises, output_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0 , cex=0.75)






write.table(file='E:/Paises_fator_cluster.csv',matriz, sep=';',dec=',')

