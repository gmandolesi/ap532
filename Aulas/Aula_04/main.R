
source ('funcoes.R')			
			

## Exemplo de matriz
	##matriz=mpg[,c(3,4,8,9)]
  matriz=read.csv('Tecnificacao-Cerrado-Gado-de-Corte.csv', header = T)
  matriz = matriz[,-1]

## Numero de clusters a serem avaliados
	num_clusters = 15

## Resultado
	## Se nao passar uma seed vai rodar aleatorio
	result = retorna_clusters (matriz, num_clusters=15, seed = NULL)			

## Plots
	plota_clusters (result$SS_int_VIZ, result$SS_ext_VIZ)


## Para obter os clusters, por exemplo 3
	numero_de_clusters <- 3
	clsuter_escolhido <- kmeans(matriz,centers = numero_de_clusters, iter.max = 20)		

	## Clusters
		clsuter_escolhido$cluster