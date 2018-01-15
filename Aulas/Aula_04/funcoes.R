
retorna_clusters = function (matriz, num_clusters, seed = NULL)
{	

	## SS total da SS INTER CLUSTER
		SS_int_VIZ    <- 0		
		
	## SS total EXTRA CLUSTER 
		SS_ext_VIZ    <- 0

		
	## Calcula os k clusters
		for (k in 1:num_clusters)
		{
			## Mesma semente
			if (!is.null(seed))			
				set.seed(seed)
	
			cluster_ajuste <- kmeans(matriz,centers = (k+1), iter.max = 20)			
			
			## SS total da SS INTER CLUSTER 
				SS_int_VIZ[k] <- cluster_ajuste$tot.withinss
				
			## SS total EXTRA CLUSTER 
				SS_ext_VIZ[k] <- cluster_ajuste$betweenss			
		}
		
	return ( list ( SS_int_VIZ = SS_int_VIZ,
					SS_ext_VIZ = SS_ext_VIZ) )
}		
		
		
		


plota_clusters = function (SS_int_VIZ, SS_ext_VIZ)
{				

	## Plots Dissimilaridade Interna e Externa
		par(mfrow=c(2,2))
		par(pty="m")
			
	
		plot(SS_int_VIZ,type="b", pch=19, xlab="Número de clusters", 
			 ylab="Dissimilaridade Interna", axes=F, lwd=2, main="(A) Dissimilaridade Interna")	
		axis(1, 1:length(SS_int_VIZ), 2:(length(SS_int_VIZ)+1))
		axis(2)
		box()
			
			
		plot(SS_ext_VIZ,type="b", pch=19, xlab="Número de clusters", 
			 ylab="Dissimilaridade Externa", axes=F, lwd=2, main="(B) Dissimilaridade Externa")				
		axis(1, 1:length(SS_ext_VIZ), 2:(length(SS_ext_VIZ)+1))
		axis(2)
		box()
					


	## Calculo dos deltas
		difs_int_VIZ <- ((SS_int_VIZ[2:length(SS_int_VIZ)] - SS_int_VIZ[1:(length(SS_int_VIZ)-1)])/max(SS_int_VIZ)*100)		
		difs_ext_VIZ <- ((SS_ext_VIZ[2:length(SS_ext_VIZ)] - SS_ext_VIZ[1:(length(SS_ext_VIZ)-1)])/max(SS_ext_VIZ)*100)
	



	## Plota os deltas 	
	
		plot(difs_int_VIZ,type="b", pch=19, xlab="Número de clusters", 
			 ylab="Valores Deltas", axes=F, lwd=2, main="(C) Deltas da Dissimilaridade Interna")	
		axis(1, seq(1,length(difs_int_VIZ)), seq(3,( length(difs_int_VIZ)) +2))
		axis(2)
		box()
										
		plot(difs_ext_VIZ,type="b", pch=19, xlab="Número de clusters", 
			 ylab="Valores Deltas", axes=F, lwd=2, main="(D) Deltas da Dissimilaridade Externa")	
		axis(1, seq(1,length(difs_ext_VIZ)), seq(3,(length(difs_ext_VIZ))+2) )
		axis(2)
		box()
		
	
	
	## % acumulada do delta SS interno das diferencas dos k's	
			acum_int_VIZ = 0
			acum_int_VIZ[1] <- difs_int_VIZ[1]			
			for (i in 2:length(difs_int_VIZ))
			{
				acum_int_VIZ[i] <- acum_int_VIZ[i-1] + difs_int_VIZ[i] 
			}
										
	
			acum_ext_VIZ = 0
			acum_ext_VIZ[1] <- difs_ext_VIZ[1]			
			for (i in 2:length(difs_ext_VIZ))
			{
				acum_ext_VIZ[i] <- acum_ext_VIZ[i-1] + difs_ext_VIZ[i] 
			}
			
	
	
		
		
	## Printa na tela		
		
					
		## Interna		
			cat('\n\n')
			print('CALCULOS Dissimilaridade INTERNA')
			cat('\n')
			
			print('(A) Dissimilaridade Interna')
			cat('\n')
			print (SS_int_VIZ)
			cat('\n\n')
			
			print('(C) Deltas da Dissimilaridade Interna ')
			cat('\n')
			print (difs_int_VIZ)
			cat('\n\n')
			
			print('Acumulada Delta da Dissimilaridade Interna ')
			cat('\n')
			print (acum_int_VIZ/acum_int_VIZ[length(difs_int_VIZ)])
			cat('\n\n')
		
				
				
		## Externa
			cat('\n\n')
			print('CALCULOS Dissimilaridade EXTERNA')
			cat('\n')
			
			print('(B) Dissimilaridade Externa')
			cat('\n')
			print (SS_ext_VIZ)
			cat('\n\n')
			
			print('(C) Deltas da Dissimilaridade Externa ')
			cat('\n')
			print (difs_ext_VIZ)
			cat('\n\n')
			
			print('Acumulada Delta da Dissimilaridade Externa ')
			cat('\n')
			print (acum_ext_VIZ/acum_ext_VIZ[length(difs_ext_VIZ)])
			cat('\n\n')
}
