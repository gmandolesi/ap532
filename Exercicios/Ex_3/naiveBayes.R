##Implementacao do algoritmo Naive Bayes em R.

## Carregando os pacotes necess?rio
library(foreign)
library(e1071)
library(pander)
library(printr)

## Lendo o conjunto de dados
weather <-read.arff(file ='weather.nominal.arff')

## Aplicando o naive bayes para variaveis nominais
modelnaive = naiveBayes(play ~ ., data = weather)
modelnaive
