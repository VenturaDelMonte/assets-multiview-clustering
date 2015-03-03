source("NNSVD.R")
if(!require(plotrix))
{
	install.packages("plotrix")
	library(plotrix)
}
if(!require(vioplot))
  {
    install.packages("vioplot")
    library(vioplot)
  }
#' 
#' @details This function calculates the matrix such that: 
#' the cell (i,j) = 1 if elements j is in the cluster 1
#' else 0
#' @param bestClust vector with clusters
#' @param bestK	value of K
#' @return indices matrix of size sum(k) x size(cluster) 
#' @references
#' @export 
#' 
buildMultiViewMatrix <- function(bestClust, bestK)
{
	nView = length(best_clusters);
	symbolsCount = length(best_clusters[[1]])
	M = NULL
	for (i in 1:nView)
	{
		clust = bestClust[[i]]
		n = length(clust)
		tmp = matrix(0, nrow = bestK[i], ncol = symbolsCount)
		for (j in 1:n)
		{
			tmp[clust[j], j] = 1
		}
		
		M = rbind(M, tmp)
	
	}
	return (M)
}

#' 
#' @details This function calculates the error  (Frobenius norm (the Euclidean norm of x treated as if it were a vector))
#' @return Frobenius norm's result
#' @references
#' @export 
#' 
error.internal <- function(X, PH)
{
	return (norm(X-PH, type="F"))
}

#' 
#' @details This function calculates matrix' factoritation of
#'  indices matrix which containes all clusters
#' @param X multiview indices matrix 
#' @param k	number of cluster of multiview
#' @param eps accuracy 
#' @param iter_max	iteration max
#' @return P * H 
#' @references
#' @export 
#' 
matrix_factoritation <- function(X, k, eps, iter_max)
{

	l <- dim(X)[1];
	n <- dim(X)[2];

	PH <- .nndsvd.internal(X, k,  flag=0) 
	P <- PH$W
	H <- PH$H

	PH <- P%*%H
	old_err = error.internal(X, PH)
	n_iter  = 0;
	delta   = 10;
	while(delta > eps && n_iter <= iter_max)
	{
		XHt <- X %*% t(H)
		PHHt <- P %*% H %*% t(H)
		P <- P * XHt / PHHt
		P[is.nan(P)]<-0;
		PtX <- t(P) %*% X
		PtPH <- t(P) %*% P %*% H
		H <- H * PtX / PtPH
		H[is.nan(H)]<-0
		PH <- P %*% H
		PH[is.nan(PH)]<-0;
		err=error.internal(X, PH)
		n_iter <- n_iter +1;
		delta = abs(err - old_err);
		old_err = err;
		#print(paste(delta, n_iter, sep=" "))
	}
	if(delta > eps)
		warning("Algoritmo non converge \n");

	toRet <- list(P=P, H=H, delta = delta, n_iter = n_iter)
}

#' 
#' @details Based on the values in the projection matrix P, 
#' we can calculate a matrix T indicating the contribution of the views 
#' to each meta-cluster.
#' @param P projection matrix
#' @param k	number of cluster of multiview
#' @param bestK vector of cluster
#' @return matrix T
#' @references
#' @export 
#' 
find_T <- function(P, k, bestK){   
  
	nView = length(bestK)
	
	T <- matrix(0, nView, k)

	for(i in 1:nView)
	{
		min = sum(bestK[i-1:i])+1
		max = sum(bestK[1:i])
		for(j in 1:k)
		{
			T[i,j]=(sum(P[min:max ,j]))/sum(P[,j])
		}
	}
	rownames(T) = getLabelsView();
	colnames(T) = paste("Clst",c(1:k), sep= " ")
	return(T)
}

plotHist <- function(confusion_matrices, labels = getLabelsView())
{
	
	for( i in 1:length(confusion_matrices))
	{	
		mat = t(confusion_matrices[[i]])
		colour = dim(mat)[1]+1
		png(paste("Hist_", labels[i], "_View.png", sep=""),width=16,height=6,units="in",res=300)
		barp(mat, main = paste("Hist value of assets in a single cluster (", labels[i], " View)", sep=""), ylim=c(1,60), ylab="Value", col=2:colour, xlab = "Clusters", legend.pos = "topright", legend.lab = c("1","2","3","4","5","6","7","8","9","10"))
	  	dev.off()
	}
}

plotPie <- function(mvSClusterMembershipVector, clusters, data, nclust)
{
	layout(1)
	
	#length(unique(data$sector[which(data$class == "Consumer Staples")]))
	a_sector = table(data$sector) #asset per sector
	sectors  = unique(data$sector)
	
	
	
	for( i in 1:nclust)
	{
		tmp = mvSClusterMembershipVector[clusters[[i]]]
		ret = list()
		perc= c()
		label_dataset = c()
	
		ret = table(tmp)
		
		name = as.integer(names(ret))
		
		tot = a_sector[sectors[name]]
		
		for(j in 1:length(tot))
		{
			label_dataset = c(label_dataset, 
							  paste(round(ret[[j]] * 100 / tot[[j]], digits=0), "%", sep=""))
		}
		
		png(paste("barplot_clust",i,"sector.png", sep=""),
			width=32,height=6,units="in",res=300)
		mp = barplot(ret, ylim=c(0,11), space=3,
				     main=paste("subsectors cluster",i, sep=" "))
		text(mp, 
			 ret+1, labels=label_dataset)
		
		dev.off()
	}
}

getClusters <- function(H)
{
	nAssets = dim(H)[2]

	clusters = list(c1=NULL,c2=NULL,c3=NULL,c4=NULL,c5=NULL,c6=NULL,
	                c7=NULL,c8=NULL)
	for(i in 1:nAssets)
	{
		clusters[[which.max(H[,i])]] = c(clusters[[which.max(H[,i])]], i)
	}
  return (clusters)
}


plot_T <- function(T)
{
  png("T_for_Clusters.png",width=16,height=6,units="in",res=300)
  barp(T, main="T for Clusters", col=2:5, legend.pos = "topright", legend.lab = c("Yield", "Range", "Volatility", "Volume"))
  dev.off()
  png("T_for_Views.png",width=16,height=6,units="in",res=300)
  barp(t(T), main="T for views", col=2:9, legend.pos = "topright", legend.lab = c("1","2","3","4","5","6","7","8"))
  dev.off()
}

#which classes each elements of a meta-cluster belong to
which_classes <- function(clusters, classes)
{
  nClust = length(clusters)
  for(i in 1:nClust)
  {
    c1 = clusters[[i]]
    tcc1=table(classes[c1])
    png(paste("which_", i, ".png", sep=""), width = 480, height = 380, units = "px")
    barplot(tcc1, col=rainbow(9), las=2, axisnames=T)
    dev.off()
  }
}


buildClusterMembershipVector <- function(clusters)
{
	ret <- c()
	for (i in 1:length(clusters))
	{
		ret[clusters[[i]]] = i
	}
	return (ret)
}

buildClusterMembershipVectorSector <- function(clusters, data)
{
	ret  <- c()
	for (i in 1:length(clusters))
	{
		ret[clusters[[i]]] = data$sector[clusters[[i]]]
	}
	
	for(i in 1:length(ret))
	{
		ret[i] = which(ret[i] == unique(data$sector))		
	}
	return (ret)
}

plot_vio <- function(mvClustersFeatures, clusters, mvClustersSize, colnames)
{
	#plot violinplot
	n=length(clusters)
	colours = rainbow(n)
	for(j in 1:n)
	{
		params=list()
# 		offset   = sum(mvClustersSize[j-1:j])+1 #1
# 		end = offset + mvClustersSize[j] - 1
# 		for(i in 1:length(colnames)){#39
#  			params[[i]] = (cdat[[i]][offset:end])
# 		}
		cl1 = mvClustersFeatures[mvClusterMembershipVector==j,]
		for(i in 1:length(colnames)){
			params[[i]] = cl1[,i]
		}
		params['names'] = list(rep("", 39))
		params['col'] = colours[j]
		png(paste("MVvioplot",j,".png",sep=""), width=16, height=6, units="in", res=300)
		par(mar =  c(9, 4, 4, 2) + 0.1)
		#par(mar=c(8, 4, 4, 2) + 0.1)
		do.call(vioplot, params)
		axis(1, las=2, labels = colnames, at=1:39)
		title(paste("meta-cluster", j, sep=" "))
		dev.off()
	}
}
