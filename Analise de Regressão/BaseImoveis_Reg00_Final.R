# limpar mem�ria do R
rm(list=ls(all=TRUE))


# mostrar at� 2 casas decimais
options("scipen" = 2)



# Ler arquivo xls.
install.packages("readxl")

library(readxl)

imoveis1 <- read_excel("E:/LABIA&ML2018/AtividadeAulaR/Arquivo_Valorizacao_Ambiental_2.xlsx")

View(imoveis1)

imoveis <- subset(imoveis1, select=c(-Ordem))
 
View(imoveis)
 


# trabalhar com as vari�veis 
attach(imoveis)

#Verificando o formato das vari�veis
str(imoveis)


#Estat�sticas descritivas - Medidas resumo
summary(imoveis)

#comando para gerar em 3 linhas e duas colunas os histogramas
par (mfrow=c(3,3))
hist(imoveis$Valor)
hist(imoveis$�rea)
hist(imoveis$IA)
hist(imoveis$Andar)
hist(imoveis$Su�tes)
hist(imoveis$Vista)
hist(imoveis$DistBM)
hist(imoveis$Semru�do)
hist(imoveis$AV100m)

par (mfrow=c(3,3))
boxplot(imoveis$Valor, main="Valor")
boxplot(imoveis$�rea  , main="�rea")
boxplot(imoveis$IA , main="IA")
boxplot(imoveis$Andar , main="Andar")
boxplot(imoveis$Su�tes , main="Su�tes")
boxplot(imoveis$Vista , main="Vista")
boxplot(imoveis$DistBM , main="DistBM")
boxplot(imoveis$Semru�do , main="Semru�do")
boxplot(imoveis$AV100m , main="AV100m")


par (mfrow=c(1,1))

boxplot(imoveis$Valor~imoveis$Vista, main='Valor Vs Vista',col=c('red','blue'))

boxplot(imoveis$Valor~imoveis$Semru�do, main='Valor vs sem ruido',col=c('red','blue'))


# matriz de correla��es
matcor <- cor(imoveis)
print(matcor, digits = 2)


#visualizar correlacao

install.packages("corrgram")
library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)


install.packages("corrplot")
library(corrplot)

corrplot::corrplot(matcor, method="circle", order="hclust")

#Gr�fico de dispersao para a associa��o entre �rea m2 e valor
plot (x = imoveis$�rea, y = imoveis$Valor,
      main = "Gr�fico de dispers�o",
      xlab = "�rea",
      ylab = "Valor")

#Gr�fico de dispersao para a associa��o entre IA e valor
plot (x = imoveis$IA, y = imoveis$Valor,
      main = "Gr�fico de dispers�o",
      xlab = "IA",
      ylab = "Valor")



# Op��es de gr�ficos: Gr�fico de dispersao com o plotly

install.packages("plotly")
library(plotly)
plot_ly ( x=imoveis$�rea, y=imoveis$Valor  , type="scatter")
plot_ly ( x=imoveis$IA, y=imoveis$Valor, type="scatter")


#Gr�fico de dispersao com o ggplot2
install.packages("ggplot2")

library(ggplot2)
ggplot (data= imoveis, aes(x=imoveis$�rea, y=imoveis$Valor )) + 
  geom_point(size=0.4) +
  geom_smooth(method="lm", color ="red", linetype=2) +
  labs(title = "Gr�fico de dispers�oo, Valor e �rea", x="�rea", y="Valor")


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
  # abs(r) � para que na sa�da as correla��es ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
#pdf(file = "grafico.pdf")


pairs(imoveis, lower.panel=panel.smooth, upper.panel=panel.cor)

par (mfrow=c(1,1))

str(imoveis)
attach(imoveis)
# Modelo de regress�o linear simples

modelo0 <- lm(Valor ~ �rea)
summary(modelo0)
modelo1 <- lm(Valor ~ �rea+Semru�do+IA)
summary(modelo1)
modelo2 <- lm(Valor ~ �rea+Semru�do+IA+Andar+Su�tes+DistBM+AV100m+Vista)
summary(modelo2)




install.packages("lattice")
install.packages("latticeExtra")
install.packages("asbio")
install.packages("car")

library(lattice)
library(latticeExtra)
library(asbio)
library(car)

# length(coef(x))= qte de coeficientes 
# df.residual(x)  = graus de liberdade do res�duos
# length(fitted(x)) = qte de observa�oes ajustadas 
# RMSE = summary(x)$sigma = Erro quadr�tico m�dio
# R2 = summary(x)$r.squared  = R2 
# R2adj = summary(x)$adj.r.squared = R2 ajustado
# press(x) c�lculo PREdiction Sum of Squares

# para modelos logisticos:
# logLik(x) Extract Log-Likelihood  
# AIC(x)
# BIC(x)
# When comparing models fitted by maximum likelihood to the same data, 
# the smaller the AIC or BIC, the better the fit
 

measures <- function(x) {
  L <- list(npar = length(coef(x)),
            dfres = df.residual(x),
            nobs = length(fitted(x)),
            RMSE = summary(x)$sigma,
            R2 = summary(x)$r.squared,
            R2adj = summary(x)$adj.r.squared,
            PRESS = press(x),
            logLik = logLik(x),
            AIC = AIC(x),
            BIC = BIC(x))
  unlist(L)
}


modl <- list(m0 = modelo0, m1 = modelo1, m2 = modelo2)
round(t(sapply(modl, measures)), 3)

str(imoveis)




attach(imoveis)


modelo1 <- lm(Valor ~ �rea+Semru�do+IA+Andar+Su�tes+DistBM+AV100m+Vista)
summary(modelo1)

# selecionando vari�veis por m�todo autom�tico

stepwise<-step(modelo1,direction="both")
 
stepwise
summary(stepwise)



# Modelo final.
modelo_fim <- lm(Valor ~ �rea+IA+Andar+Su�tes+Semru�do+Vista)
summary(modelo_fim)


Val_pred <- predict(modelo_fim,interval = "prediction", level = 0.95) 

fix(Val_pred)


# intervalo de confianca - grafico para media
fit <- Val_pred[,1] # valores preditos
lower <- Val_pred[,2] # limite inferior
upper <- Val_pred[,3] # limite superior


mse <- mean((imoveis$Valor - fit)^2)
sqrt(mse)

erro_usando_media <- mean((imoveis$Valor - mean(imoveis$Valor))^2)
sqrt(erro_usando_media)


# grafico residuo
rs <- resid(modelo_fim)
plot(predict(modelo_fim), rs, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE viola��o da suposi��o de que os erros aleat�rios t�m distribui��o Normal
  
qqnorm(residuals(modelo_fim), ylab="Res�duos",xlab="Quantis te�ricos",main="")
qqline(residuals(modelo_fim))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste � pequeno, rejeita-se a hip�tese de normalidade dos res�duos e,
# por consequ�ncia, conclui-se que os erros n�o s�o normalmente distribu�dos.

shapiro.test(residuals(modelo_fim))


#### se o teste de shapiro apontar que os res�duos n�o est�o adequados.

attach(imoveis)

imoveis$sqrt_valor<-sqrt(imoveis$Valor)
  
#####  ou 

imoveis$IA<- 1/IA

hist(imoveis$Valor)
hist(imoveis$sqrt_valor)
plot (imoveis$�rea, imoveis$sqrt_valor)
plot (imoveis$�rea, imoveis$Valor)
summary(imoveis)


attach(imoveis)


str(imoveis)

modelo2 <- lm(sqrt_valor ~ �rea+Semru�do+IA+Andar+Su�tes+DistBM+AV100m+Vista)
summary(modelo2)


modelo_fim2 <- lm(sqrt_valor ~ �rea+Semru�do+IA+Andar+Su�tes+Vista)
summary(modelo_fim2)

# grafico residuo
rs2 <- resid(modelo_fim2)
plot(predict(modelo_fim2), rs2, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


qqnorm(residuals(modelo_fim2), ylab="Res�duos",xlab="Quantis te�ricos",main="")
qqline(residuals(modelo_fim2))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste � pequeno, rejeita-se a hip�tese de normalidade dos res�duos e,
# por consequ�ncia, conclui-se que os erros n�o s�o normalmente distribu�dos.

shapiro.test(residuals(modelo_fim2))


attach(imoveis)


Imoveis_Final<-cbind(imoveis,Val_pred)

fix(Imoveis_Final)

# op��o

write.table(file='E:/LabBDT2018/Arquivo_Valorizacao_Ambiental_saida.csv',Imoveis_Final, sep=';',dec=',')


## �rvore de Regress�o


install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

modelo_Valor_tree <- rpart (Valor ~ �rea+IA+Andar+Su�tes+DistBM+Semru�do+AV100m+Vista, data=imoveis, 
                     cp = 0.001,minsplit = 15,maxdepth=10)

# Faz o Gr�fico
rpart.plot(modelo_Valor_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE, 
           snip=FALSE)

Val_pred_tree <- predict(modelo_Valor_tree,interval = "prediction", level = 0.95) 
str(Val_pred_tree)


mse_tree <- mean((imoveis$Valor - Val_pred_tree)^2)
sqrt(mse_tree)

erro_usando_media <- mean((imoveis$Valor - mean(imoveis$Valor))^2)
sqrt(erro_usando_media)

# grafico residuo
rs_tree <- Val_pred_tree- imoveis$Valor
rs_tree_padron <-scale(rs_tree )
plot(predict(modelo_Valor_tree), rs_tree_padron, xlab = "Com �rvore de Regress�o",ylab = "Residuos")
abline(h = 0, lty = 2)
