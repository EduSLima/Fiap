# limpar memória do R
rm(list=ls(all=TRUE))


# mostrar até 2 casas decimais
options("scipen" = 2)



# Ler arquivo xls.
install.packages("readxl")
library(readxl)
salarios <- read_excel("E:/PeopleAnalytics/Arquivo_Salarios_Colaboradores_2019.xlsx")
 
View(salarios)
 


attach(salarios)

#Verificando os nomes e formatos das variáveis
names(salarios)

str(salarios)


#Estatísticas descritivas - Medidas resumo
summary(salarios)

#comando para gerar em 3 linhas e duas colunas os histogramas
par (mfrow=c(3,3))
hist(salarios$salario )
hist(salarios$idade)
hist(salarios$tempocasa)
hist(salarios$escolar)
hist(salarios$qproj_estra)
hist(salarios$proj_sustent)
hist(salarios$proj_6sigma)
hist(salarios$proj_social)
hist(salarios$notaavalia)

par (mfrow=c(1,1))

boxplot(salarios$salario ~salarios$proj_sustent, main='proj_sustent',col=c('red','blue'))

boxplot(salarios$salario ~salarios$proj_6sigma, main='proj_6sigma',col=c('red','blue'))


# matriz de correlações
matcor <- cor(salarios)
print(matcor, digits = 2)


#visualizar correlacao

install.packages("corrgram")
library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

#Gráfico de dispersao para a associação entre idade  e Salario 
plot (x = salarios$idade, y = salarios$salario,
      main = "Gráfico de dispersão",
      xlab = "Idade (anos)",
      ylab = "Salario (R$)")

#Gráfico de dispersao para a associação entre tempo de casa e Salario 
plot (x = salarios$tempocasa, y = salarios$salario ,
      main = "Gráfico de dispersão",
      xlab = "tempo de casa",
      ylab = "Salario ")

#Gráfico de dispersao para a associação entre tempo de casa e Salario 
plot (x = salarios$notaavalia, y = salarios$salario ,
      main = "Gráfico de dispersão",
      xlab = "Score de Avaliação",
      ylab = "Salario ")





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


pairs(salarios, lower.panel=panel.smooth, upper.panel=panel.cor)

par (mfrow=c(1,1))





# Opções de gráficos: Gráfico de dispersao com o plotly

install.packages("plotly")
library(plotly)
plot_ly (x=salarios$salario   , y=salarios$tempocasa, type="scatter")

plot_ly (x=salarios$salario   , y=salarios$idade, type="scatter")


#Gráfico de dispersao com o ggplot2
install.packages("ggplot2")

library(ggplot2)
ggplot (data= salarios, aes(x=salarios$tempocasa, y=salarios$salario  )) + 
  geom_point(size=0.4) +
  geom_smooth(method="lm", color ="red", linetype=2) +
  labs(title = "Gráfico de dispersãoo, Salario  e Tempo de casa", x="Tempo de casa", y="Salario ")


names(salarios)
attach(salarios)
# Modelo de regressão linear  



modelo01 <- lm(salario  ~ idade+tempocasa+escolar+qproj_estra+proj_sustent+
                         proj_6sigma+proj_social+notaavalia)
summary(modelo01)




forward<-step(modelo01,direction="forward")

forward

summary(forward)

backward<-step(modelo01,direction="backward")
backward
summary(backward)

stepwise<-step(modelo01,direction="both")
 
stepwise
summary(stepwise)



# Modelo final.
modelo_fim <- lm(salario  ~ idade+escolar+qproj_estra+proj_6sigma+proj_social)
summary(modelo_fim)



Val_pred <- predict(modelo_fim,interval = "prediction", level = 0.95) 
View(Val_pred)
# intervalo de confianca - grafico para media
fit <- Val_pred[,1] # Salario es preditos
lower <- Val_pred[,2] # limite inferior
upper <- Val_pred[,3] # limite superior

mse <- mean((salarios$salario  - fit)^2)
sqrt(mse)

erro_usando_media <- mean((salarios$salario  - mean(salarios$salario ))^2)
sqrt(erro_usando_media)


# grafico residuo
rs <- resid(modelo_fim)
plot(predict(modelo_fim), rs, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
  
qqnorm(residuals(modelo_fim), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo_fim))

#Teste de Normalidade de Shapiro Wilk

# sE Salario  P do teste é pequeno, rejeita-se a hipótese de normalidade dos resíduos e,
# por consequência, conclui-se que os erros não são normalmente distribuídos.

shapiro.test(residuals(modelo_fim))

## perform Durbin-Watson test
dwtest(residuals(modelo_fim))

attach(salarios)
salarios_Final<-cbind(salarios,Val_pred)

fix(salarios_Final)


write.table(file='E:/LabBDT2018/Arquivo_Salario izacao_Ambiental_saida.csv',salarios_Final, sep=';',dec=',')


## Árvore de Regressão


install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

modelo_Salario_tree <- rpart (salario  ~ idade+tempocasa+escolar+qproj_estra+proj_sustent+
                                 proj_6sigma+proj_social+notaavalia, data=salarios, 
                     cp = 0.001,minsplit = 15,maxdepth=10)




# Faz o Gráfico
rpart.plot(modelo_Salario_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,box.palette="Grays",
           snip=FALSE)

?rpart.plot

Val_pred_tree <- predict(modelo_Salario_tree,interval = "prediction", level = 0.95) 
str(Val_pred_tree)


mse_tree <- mean((salarios$salario  - Val_pred_tree)^2)
sqrt(mse_tree)

erro_usando_media <- mean((salarios$salario  - mean(salarios$salario ))^2)
sqrt(erro_usando_media)

# grafico residuo
rs <- Val_pred_tree- salarios$salario 
plot(predict(modelo_Salario_tree), rs, xlab = "Com Árvore de Regressão",ylab = "Residuos")
abline(h = 0, lty = 2)
