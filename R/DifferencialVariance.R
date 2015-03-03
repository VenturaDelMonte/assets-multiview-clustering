if(!require(matrixStats))
{
	install.packages("matrixStats")
	library(matrixStats)
}
if(!require(plotrix))
{
	install.packages("plotrix")
	library(plotrix)
}



DifferencialVariance <- function(mvClustersMemberShip, mvClustersFeatures, path){
	l.rank <-c()
	l.rank2 <- c()
	for(i in names(table(mvClustersMemberShip))){
		cl1 <- mvClustersFeatures[mvClustersMemberShip==i,]
		c2 <- mvClustersFeatures[!(mvClustersMemberShip==i),]
		apply(X=cl1,2,FUN=sd) -> featContribVariance
		apply(X=c2,2,FUN=sd)  -> featOutsideCluster
				
		par(mfrow=c(1,2))
		png(paste(path,"/clust",i,".png",sep=""),width=2048,height=1024)
		layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

		#write.table(featContribVariance,paste(path,"/inside_cluster",i,".txt",sep=""),sep="\t")
		bp <- barplot(featContribVariance,axes= FALSE, axisnames = FALSE, main=paste("Standard Deviation in cluster ",i,sep=""), col="red")
		labs <- colnames(mvClustersFeatures)
		text(bp, par("usr")[3], labels = labs, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
		mtext(paste("Features Contribution Cluster",i), side = 1,line=-5, outer = TRUE)

		axis(2) 

		#write.table(featOutsideCluster,paste(path,"/outside_cluster",i,".txt",sep=""),sep="\t")
		bp<- barplot(featOutsideCluster,axes= FALSE, axisnames = FALSE, main=paste("Standard Deviation Outside Cluster ",i,sep=""), col="blue")
		labs <- colnames(mvClustersFeatures)
		text(bp, par("usr")[3], labels = labs, srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
		mtext(paste("Features Contribution Cluster",i), side = 1,line=-5, outer = TRUE)
		axis(2) 

		ranking <- featOutsideCluster - featContribVariance
		ranking[order(ranking,decreasing=TRUE)] -> ranking
		
		ranking2 <- featContribVariance
		ranking2[order(ranking2,decreasing=FALSE)] -> ranking2		
		#write.table(ranking,paste(path,"/ranking_cluster",i,".txt",sep=""),sep="\t")
		par(mar =  c(9, 4, 4, 2) + 0.1)
		bp <- barplot(ranking, main = "Feature Ranking",axes= FALSE, axisnames = FALSE, col="green")
		labs <- names(ranking)
		labs2 <- names(ranking2)
		text(bp, par("usr")[3], labels = labs, srt = 90, adj = c(1.1,1.8), xpd = TRUE, cex=.9)
		mtext(paste("Features Contribution Cluster",i), side = 1,line=-5, outer = TRUE)
		axis(2) 
		dev.off()
		l.rank <- rbind(l.rank,labs)
	 	l.rank2 <- rbind(l.rank2,labs2)
	}
	return(list(l.rank,l.rank2))
}
#plot distribution of only N features that we want
plotDistr <- function(mvClustersFeatures, mvClusterMembership, lab, N, suffix="")
{
	for(i in 1: length(names(table(mvClusterMembership))))
	{
		cl1 <- mvClustersFeatures[mvClusterMembership==i,]
		firstNFea = lab[i,1:N]
		valueFea = cl1[, firstNFea]
		png(paste("distr_clust",i,suffix,".png",sep=""),width=1300,height=1200)
		plot(valueFea[1,], type="l", axes=F, main=paste("cluster", i),
			 xlab="", ylab="", ylim=range(0:1))
		par(mar =  c(9, 4, 4, 2) + 0.1)
		
		for(j in 2:dim(valueFea)[1])
		{
			lines(valueFea[j,])
		}
		axis(2)
		axis(1, las=2, labels = firstNFea, at=1:N)
		box()
		dev.off()
	}
}


# 
# plotDistrEx <- function(mvK, mvClustersFeatures, mvClusterMembership, featureScore, eps=1/100)
# {
# 	colours = rainbow(mvK)
# 	lgnd = c()
# 	png("feature_median.png",width=2048,height=1024)
# 	par(mar =  c(9, 4, 4, 2) + 0.1)
# 	
# 	M=matrix(NA,ncol=length(featureScore$tmp),nrow=mvK)
# 	SD=matrix(NA,ncol=length(featureScore$tmp),nrow=mvK)
# 	for(i in 1: mvK)
# 	{
# 		cl1 <- mvClustersFeatures[mvClusterMembership==i,]
# 		#firstNFea = lab[i,]
# 		features = row.names(featureScore)
# 		valueFea = cl1[, features]
# 		
# 		M[i,] = colMedians(valueFea)
# 		SD[i,] = colSds(valueFea) 
# 	}
# 	
# 	for(i in 1:mvK)
# 	{
# 		noise = c()
# 		
# 		#noise = runif(length(M))
# 		for (j in 1:length(features))
# 		{
# 			noise[j]=0
# 			f.max = max(M[,j])
# 			f.min = min(M[,j])
# 			if (abs(f.max - f.min) < eps)
# 			{
# 				noise[j]=0.01*i	
# 			}
# 		}
# 		cat(noise,'\n')
# 		# 	if (i==1)
# 		# 	{
# 		# 		plot(M, axes=F, xlab="", ylab="", ylim=range(0:1),col=colours[i])
# 		#	plotCI(M+noise,uiw=SD,err="x",add=(i!=1),col=colours[i],axes=F, xlab="", ylab="")
# 		#	plotCI(M+noise,uiw=noise,err="y",add=(i!=1),col=colours[i],axes=F, xlab="", ylab="")
# 		#	plotCI(M,uiw=SD,err="y",add=(i!=1),col=colours[i],axes=F, xlab="", ylab="")
# 		plotCI(M[i,],uiw=SD[i,],err="y",add=(i!=1),col=colours[i],axes=F, xlab="", ylab="",ylim=range(0:2))
# 		#plotCI(M[i,]+noise,uiw=noise,err="y",add=(i!=1),col=colours[i],axes=F, xlab="", ylab="",ylim=range(0:2))
# 		# 	}
# 		# 	else
# 		# 	{
# 		# 		points(M,col=colours[i])
# 		# 	}
# 		box()
# 		lgnd = cbind(lgnd, paste("cl", as.character(i), sep=""))
# 		
# 	}
# 	
# 	axis(2)
# 	axis(1, las=2, labels = features, at=1:length(features))
# 	legend("topright", legend = lgnd, fill = colours)
# 	dev.off()	
# 	return (list(M=M,SD=SD))
# }

plotDistrEx <- function(mvK, mvClustersFeatures, mvClusterMembership, featureScore)
{
	colours = rainbow(mvK)
	lgnd = c()
	png("features_median.png",width=2048,height=1024)
	par(mar =  c(9, 4, 4, 2) + 0.1)
	
	M=matrix(NA,ncol=length(featureScore$tmp),nrow=mvK)
	SD=matrix(NA,ncol=length(featureScore$tmp),nrow=mvK)
	features = row.names(featureScore)

	for(i in 1:mvK)
	{
		cl1 <- mvClustersFeatures[mvClusterMembership==i,]
		valueFea = cl1[, features]		
		M[i,] = colMedians(valueFea)
		SD[i,] = colSds(valueFea)
	}
	for (i in 1:mvK)
	{
		tmpM = rep(NA, mvK * length(features))
		tmpSD = rep(NA, mvK * length(features))
		for (j in 1:length(features))
		{
			tmpM[(i - 1) + (j - 1) * mvK + 1] = M[i, j]
			tmpSD[(i - 1) + (j - 1) * mvK + 1] = SD[i, j]
		}
# 		if (i==1)
# 		{
# 			plot(tmp, axes=F, xlab="", ylab="", ylim=range(0:1),col=colours[i])
# 		}
# 		else
# 		{
# 			points(tmp,col=colours[i])
# 		}
	#	cat(tmpM,"\n",tmpSD,"\n")
		plotCI(tmpM,uiw=tmpSD,err="y",add=(i!=1),col=colours[i],axes=F, xlab="", ylab="")
		lgnd = cbind(lgnd, paste("cl", as.character(i), sep=""))
	}
	box()
	axis(2)
	tmpFeatures = rep("", mvK * length(features))
	
	for (j in 1:length(features))
	{
		tmpFeatures[(mvK / 2) + (j - 1) * mvK + 1] = paste(featureScore$tmp[j], features[j], sep=" ")
	}
	axis(1, las=2, labels = tmpFeatures, at=1:length(tmpFeatures))
	legend("topright", legend = lgnd, fill = colours, cex=1.3)
#	grid(length(features), NA)
	
	abline(v=seq(from=mvK+0.5,to=length(tmpFeatures),by=mvK), lty=6, col = "black")
#	abline(h=seq(yaxp[1], yaxp[2], (yaxp[2]-yaxp[1])/yaxp[3]), lty=6, col = "cornsilk2")
	dev.off()
	return (M)
}


plotDistrEx2 <- function(mvK, mvClustersFeatures, mvClusterMembership, featureScore)
{
	colours = rainbow(mvK)
	lgnd = c()
	png("feature_median2.png",width=2048,height=1024)
	par(mar =  c(9, 4, 4, 2) + 0.1)
	
	M=matrix(NA,ncol=length(featureScore$tmp),nrow=mvK)
	SD=matrix(NA,ncol=length(featureScore$tmp),nrow=mvK)
	features = row.names(featureScore)
	
	for(i in 1:mvK)
	{
		cl1 <- mvClustersFeatures[mvClusterMembership==i,]
		valueFea = cl1[, features]		
		M[i,] = colMedians(valueFea)
		SD[i,] = colSds(valueFea)
	}
	for (i in 1:mvK)
	{

		if (i==1)
		{
			plot(M[i,], axes=F, xlab="", ylab="", ylim=range(0:1),col=colours[i])
		}
		else
		{
			points(M[i,],col=colours[i])
		}
		
		lgnd = cbind(lgnd, paste("cl", as.character(i), sep=""))
	}
	box()
	axis(2)
	axis(1, las=2, labels = features, at=1:length(features))
	legend("topright", legend = lgnd, fill = colours, cex=0.9)
	dev.off()
	return (M)
}



