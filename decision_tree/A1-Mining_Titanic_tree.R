# Técnica de Discriminação: árvore e Logistica.

# limpar memória do R
rm(list=ls(all=TRUE))


install.packages("rpart") 
install.packages("rpart.plot") 


Titanic_train <- read.csv("C:/Users/logonrmlocal/Documents/estatisticas/train.csv")

Titanic_test <- read.csv("C:/Users/logonrmlocal/Documents/estatisticas/test.csv")

fix(Titanic_train)

str(Titanic_train)

summary(Titanic_train)

# sibsp Number of Siblings/Spouses Aboard ( Número de irmãos / cônjuges a bordo)

# parch Number of Parents/Children Aboard (Número de pais / filhos a bordo)


attach(Titanic_train)

table(Titanic_train$Survived)

prop.table(table(Titanic_train$Survived))

#comando para gerar em 4 linhas e duas colunas os plots
par (mfrow=c(2,3))
plot(as.factor(Titanic_train$Pclass), as.factor(Titanic_train$Survived),main='Pclass')
plot(Titanic_train$Sex, as.factor(Titanic_train$Survived),main='Sex')
plot(as.factor(Titanic_train$Age), as.factor(Titanic_train$Survived),main='Age')
plot(Titanic_train$Embarked, as.factor(Titanic_train$Survived),main='Embarked')
plot(as.factor(Titanic_train$SibSp), as.factor(Titanic_train$Survived),main='SibSp')
plot(as.factor(Titanic_train$Parch), as.factor(Titanic_train$Survived), main='Parch')

par (mfrow=c(1,1))

# column percentages 
prop.table(table(as.factor(Titanic_train$Pclass),as.factor(Titanic_train$Survived)),2)
 

attach(Titanic_train)

table(as.factor(Age), Survived, useNA = "ifany")

boxplot(Age ~ Survived, main='Age')

boxplot(Fare ~ Survived , main='Fare')

aggregate(Titanic_train,
          by = list( as.factor(Titanic_train$Survived)),
          FUN = mean)


# Carrega o pacote: árvore de decisão
library(rpart) 
library(rpart.plot) 

attach(Titanic_train)

# informaçoes dos Parãmetros do Modelo
 
rpart.model01 <- rpart (as.factor(Survived) ~ Pclass+Sex+Age+Embarked+Parch+Fare, maxdepth=10, Titanic_train)


# Faz o Gráfico (type=0 a 4) (extra=0 a 9)
rpart.plot(rpart.model01, type=4, extra=104, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,
           digits=2, varlen=-8, faclen=10,
           cex=0.6, tweak=1,
           compress=TRUE,
           snip=FALSE)

# veja as opções
?rpart.plot



# printcp(rpart.model01) # display the results
# plotcp(rpart.model01) # visualize cross-validation results


summary(rpart.model01) # detailed summary of splits


print(rpart.model01)


## Predict com tipo 'classe' retorna se sobreviveu ou não.

previsto.com.modelo<-predict(rpart.model01,Titanic_train,type='class')

matriz.de.confusão<-table(Titanic_train$Survived, previsto.com.modelo)
matriz.de.confusão

diagonal <- diag(matriz.de.confusão)
Acc <-  sum(diagonal)/sum(matriz.de.confusão)
Acc

## Passando na base de teste

previsto.valid<-predict(rpart.model01,Titanic_test,type='class')

#nesta base não tem a variável resposta - pode avaliar no kaggle 

install.packages("rattle")
install.packages("RColorBrewer")
install.packages("ggplot2")

install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)

#plotar regra do modelo preditivo
library(rattle)



fancyRpartPlot(modelo1, cex=0.80)

fancyRpartPlot(modelo1, cex=0.70, main="Titanic", palettes=c("Greys", "Oranges"))

?fancyRpartPlot


