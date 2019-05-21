# Técnica de Discriminação: Árvore de decisão.

# limpar memória do R
rm(list=ls(all=TRUE))


Campanha <- read.csv("C:/Users/logonrmlocal/Fiap/logistic_regression/BaseCampanhaVarejo.csv", sep=";")

attach(Campanha)  

fix(Campanha)

str(Campanha )

summary(Campanha)

Campanha$RespCampanha <- factor(Campanha$RespCampanha)

# Frequência absoluta 
table(Campanha$RespCampanha)


#comando para gerar em 4 linhas e duas colunas os plots
par (mfrow=c(3,2))
plot(Campanha$RendaMensal, Campanha$RespCampanha,ylab="RespCampanha",xlab="RendaMensal",col=c('red','darkgreen'))
plot(Campanha$TempoRel, Campanha$RespCampanha , ylab="RespCampanha",xlab="TempoRel",col=c('red','darkgreen'))
plot(Campanha$VlMedio , Campanha$RespCampanha ,ylab="RespCampanha",xlab="VlMedio",col=c('red','darkgreen'))
plot(Campanha$Pct_Vestuario, Campanha$RespCampanha, ylab="RespCampanha",xlab="Pct_Vestuario",col=c('red','darkgreen'))
plot(Campanha$TempoDesdeUltCompra, Campanha$RespCampanha, ylab="RespCampanha",xlab="TempoDesdeUltCompra",col=c('red','darkgreen'))
plot(Campanha$RegValorCli , Campanha$RespCampanha, ylab="RespCampanha",xlab="RegValorCli",col=c('red','darkgreen'))


par (mfrow=c(1,1))




#Divisao do banco de dados completo em treinamento e teste
#definir % de casos de treino
prt <- 2/3

# amostra de casos de treino aleatória
set.seed(2018)
treino <- sample(1:NROW(Campanha), as.integer(prt*NROW(Campanha)))

trainData <- Campanha[treino,]
testData  <- Campanha[-treino,]

prop.table(table(trainData$RespCampanha))
prop.table(table(testData$RespCampanha))



# Carrega o pacote: árvore de decisão


install.packages("rpart") 
install.packages("rpart.plot") 

library(rpart) 
library(rpart.plot) 

# informações dos Parâmetros do Modelo
## Usa rpart para decision tree
 
modelo_tree <- rpart (RespCampanha ~ TempoDesdeUltCompra + TempoRel + VlMedio  + Pct_Vestuario + RegValorCli     
              + RendaMensal, data=trainData, cp = 0.006,minsplit = 150,maxdepth=10)


# Faz o Gráfico
rpart.plot(modelo_tree, type=4, extra=104, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-3, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,
           snip=FALSE)

 # veja as opções
?rpart.plot

rpart.plot(modelo_tree, type=2, extra="auto", under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=TRUE,   digits=2, varlen=-3, faclen=15,
           cex=NULL, tweak=1.7,
           compress=TRUE,box.palette="auto",
           snip=FALSE)

print(modelo_tree)

# Predict como funcao para trazer a probabilidade do cliente perfil da camapnha(0/1)
yprob <- predict(modelo_tree,testData )
hist(yprob)

## Predict com tipo 'classe' retorna se é um perfil comprador ou não
pred_class <- predict(modelo_tree ,testData , type = "class")
pred_class 
 
Campanha.matriz.de.confusão<-table(testData$RespCampanha, pred_class)
Campanha.matriz.de.confusão

diagonal <- diag(Campanha.matriz.de.confusão)
perc.erro <- 1 - sum(diagonal)/sum(Campanha.matriz.de.confusão)
perc.erro

#plotar regra do modelo preditivo

install.packages("rattle")

library(rattle)

fancyRpartPlot(modelo_tree, cex=0.60)

fancyRpartPlot(modelo_tree, cex=0.60,  palettes=c("Greys", "Oranges"))

?fancyRpartPlot


attach(trainData)
modelo_log<-glm(RespCampanha ~ TempoDesdeUltCompra + TempoRel + VlMedio  + Pct_Vestuario + RegValorCli     
               + RendaMensal,trainData, family=binomial(link=logit))
summary(modelo_log)

predito<-fitted(modelo_log)

summary(predito)

hist(predito)
# Criar variável faixa probabilidade
fx_predito <- cut(predito, breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1), right=F)
plot(fx_predito , trainData$RespCampanha)

attach(testData)
Predito_teste<-predict(modelo_log, testData)

### Matriz de confusão  

fx_predito1 <- cut(Predito_teste, breaks=c(0,0.50,1), right=F)

MC <- table( RespCampanha,  fx_predito1 , deparse.level = 2) # montar a matriz de confusão  
show(MC) # mostra os resultados  
ACC = sum(diag(MC))/sum(MC) # calcula a acurácia  
show(ACC) # mostra a acurácia  


# Criar variável faixa probabilidade
fx_predito2 <- cut(Predito_teste, breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1), right=F)


plot(fx_predito2 , trainData$RespCampanha)

# Frequência absoluta
table(fx_predito1,Resp$RespCampanha)

# Frequência relativa
prop.table(table(fx_predito1,trainData$Resposta),2)



fx_predito2 <- cut(predito, breaks=c(0,0.25,0.50,0.75,1), right=F)

plot(fx_predito2 , trainData$RespCampanha)



