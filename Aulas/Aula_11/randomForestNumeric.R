## Programa para ajustar um classificador Random Forest (atributo-meta numerico)
##
library(glmnet)
library(corrplot)
library(caret)
library(randomForest)

# Leitura do arquivo de dados
dados = read.csv(("cpu.csv"), sep=",", header = TRUE)

# Seleciona o percentual de dados para o teste -> hold-out
percentage = 0.3

# separacao dos conjuntos de treinamento e teste
set.seed(2)
inTrain = createDataPartition(dados[, length(dados)], p = 1 - percentage)[[1]]
treino = dados[inTrain, ]
teste = dados[-inTrain, ]

treino_resposta = treino[, length(treino)]
teste_resposta = teste[, length(teste)]
treino = treino[, -length(treino)]
teste = teste[, -length(teste)]

# verficacao da existencia de correlacoes entre as variaveis no conjunto de treinamento
tmp = cor(treino)
tmp[!lower.tri(tmp)] = 0
treino = treino[, !apply(tmp, 2, function(x) any(x > 1 | x < -1))]
M = cor(treino, use = 'complete.obs')

corrplot(M, tl.cex = 0.7, type = 'upper')

## Train control
fitControl = trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  ## repeated ten times
  repeats = 1)

## Creating all models
set.seed(62433)
rf = train(y = treino_resposta, x = treino, 
           method = 'rf', 
           trControl = fitControl, 
           verbose = TRUE,
           importance = TRUE)

## importancia (exibir quais sao as top N variaveis que contribuem no resultado)
plot(varImp(rf), top = 5)

observed = teste_resposta
predicted = predict(rf, newdata = teste)
residuos = predicted - observed

# Plotando os residuos
axisRange = extendrange(c(observed, predicted))
plot(observed, predicted,
     ylim = axisRange,
     xlim = axisRange,
     ylab = 'Predicted',
     xlab = 'Observed')

# Adicionando uma linhas de referencia
abline(0, 1, col = "darkgrey", lty = 2)

# Calculando Correlação, RMSE e Rsquared
inform = as.character(round(cor(predicted, observed), digits = 2))
error = postResample(predicted, observed)
legend("bottomright", yjust = 1, c(paste('Corr =', inform), paste("RMSE = ", signif(error[1], 4)), paste("R^2 = ", signif(error[2], 4))), bty="n")

