## Programa para ajustar um classificador Random Forest (atributo-meta categorico)
##
library(glmnet)
library(corrplot)
library(caret)
library(randomForest)

# Leitura do arquivo de dados
dados = read.csv(("body.csv"), sep=",", header = TRUE)

## Retendo as variaveis numericas
Ind_numericas <- colnames(dados)[sapply(dados, is.numeric)]
#Ind_categoricas <- colnames(treino)[sapply(treino, function(x) !is.numeric(x))]
numericas <- dados[,Ind_numericas]
#categorias <- dados[,Ind_categoricas]


## Analisando as correlacoes das variaveis numericas
M <- cor(numericas, use = 'complete.obs')
corrplot(M, diag = T)
summary(M[upper.tri(M)])

## Filtrando variaveis altamente correlacionadas
highlyCorDescr <- findCorrelation(M, cutoff = .7)
numericas <- numericas[,-highlyCorDescr]

## Imprimindo as correlacoes novamente
M <- cor(numericas, use = 'complete.obs')
summary(M[upper.tri(M)])
corrplot(M, method='circle')
save(M, file = 'M.rda')

## Train control
fitControl = trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  selectionFunction = "oneSE",
  returnResamp = "final")

## Creating all models
rfGrid <- expand.grid(mtry = seq(2,7, by = 1))

set.seed(62433)
rf = train(gender ~ ., data = dados,
           method = 'rf', 
           trControl = fitControl, tuneGrid = rfGrid,
           verbose = TRUE,
           importance = TRUE,
           metric = "Kappa", maximize = TRUE)

## Importancia (exibir quais sao as top N variaveis que contribuem no resultado)
plot(varImp(rf), top = 7)

# Mostra os valores de Accuracy and Kappa
rf

#Mostra a matriz de confusao
rf$finalModel

