# limpar memória do R
rm(list=ls(all=TRUE))

# mostrar até 2 casas decimais
options("scipen" = 2)
 

# Ler arquivo csv

paises <- read.csv("E:/LABIA&ML2018/DADOS_Papercsv_1.csv", row.names=1, sep=";")
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


Padr_paises <- scale(paises)

fix(Padr_arq01)
summary(Padr_arq01)


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
