(WD <- getwd())
if (!is.null(WD)) setwd(WD)
source("Extractor.R")
source("cluster.R")
source("utils.R")
source("multiview.R")
source("DifferencialVariance.R")

primeSeed = 1500450271
set.seed(primeSeed)

if(!file.exists("measures.Rdata"))
{
	# Read csv and call function to obtain the dataframe, 
	#
	#spComp		= read.csv("raw-data/S&P500symbols.csv" ) 
	spComp		= read.csv("raw-data/sp500.csv" )
	#spComp$Symbol to get  Symbol
	#spComp$Sector to get  Section
	section		= as.character(spComp$Sector)
	symbols 		= as.character(spComp$Symbol)
	ssectors 		= as.character(spComp$Subsector)

	# from="2014-01-01", to="2015-01-01"
	data		= extractTS(symbols, section, ssectors)
	
	# for each of the 4 variables,
	# measures.Rdata contains the 13 measures
	save(data,file = "measures.Rdata")
} 

load("measures.Rdata")
#load("som.Rdata") #everyone have always same results
# #print boxplot and violinplot
#
# print_box		(data$meas)
# print_box2	(data$meas)
print_vioplot(data$meas)

view  		= getLabelsView()
nView 		= length(view)
classes  	= data$class
# 
# 
# ###    Cluster  Analysis    ###
# 

data_extract		= extraction.Feature(data$meas) 
drawVioplot(data_extract)
# # 
# # #Hierarch clustering
# # #results_hier[[i]][[1]]		euclidean distance
# # #results_hier[[i]][[2]]  	pearson measure
results_hier		= clustWard(data_extract, FALSE)
plotDend(results_hier)
# # 
# # #SOM clustering
# # #results_som[[i]][[1]] 		euclidean distance
# # #results_som[[i]][[2]] 		pearson measure

results_som			= clustSOM(data_extract, 6, 9, FALSE)
#save(results_som,file = "som.Rdata")
#load("somAnna.Rdata")
# 
drawSom(results_som)
# # #best k for hierarch and SOM methods with euclidean  

best_K_hier_eucl	= clusterWithBestKEuclidean(10, results_hier, data_extract, FALSE, TRUE)
best_K_som_eucl		= clusterWithBestKEuclidean(10, results_som, data_extract, TRUE, TRUE)

# # #best k for hierarch and SOM methods with pearson misure

best_K_hier_corr      = clusterWithBestKCorrelation(10, results_hier, data_extract, FALSE, TRUE)
best_K_som_corr       = clusterWithBestKCorrelation(10, results_som, data_extract, TRUE, TRUE)

# #Plot table with best k founded

# if(!require(gplots))
# {
#  	install.packages("gplots")
#  	library(gplots)
# }

# data = c(2,8,2,2,5,8,2,3,2,6,2,2,2,5,2,8)
# data_cluster = matrix(data, nrow = 4,ncol = 4,byrow = T)
# rownames(data_cluster) = c("yield", "range", "volatility", "volume")
# colnames(data_cluster) = c("HierEucl", "HierCorr", "SOMEucl", "SOMCorr")
# textplot(data_cluster, halign = "center", main="best K")




# # 
# # for(i in 1:nView)
# # {
# #   plot(results_som[[i]][[1]], type ="dist.neighbours",
# #        main=paste(view[i]," euclidean ", best_K_eucl_som[[1]][[i]]))
# #   som.hc.e <- cutree(hclust(dist(results_som[[i]][[1]]$codes)), best_K_eucl_som[[1]][[i]])
# #   add.cluster.boundaries(results_som[[i]][[1]], som.hc.e)
# #   
# #   CorKohonen::plot.kohonen(results_som[[i]][[2]], type ="dist.neighbours",
# #          main=paste(view[i]," correlation ",  best_K_corr_som[[1]][[i]]))
# #   som.hc.c <- cutree(hclust(dist(results_som[[i]][[2]]$codes)), best_K_corr_som[[1]][[i]])
# #   add.cluster.boundaries(results_som[[i]][[2]], som.hc.c)
# # }
#clust_SOM = getClusterSOM(results_som, list(best_K_som_eucl$results, best_K_som_corr$results))

# 
# clust_hier_eucl = best_K_hier_eucl$results
# clust_hier_corr = best_K_hier_corr$results
# clust_som_eucl  = NULL
# clust_som_corr  = NULL
# for(i in 1:nView)
# {
#   clust_som_eucl[[i]] = clust_SOM[[i]]$eucl
# }
# for(i in 1:nView)
# {
#   clust_som_corr[[i]] = clust_SOM[[i]]$corr
# }

#taking the best cut 

best_clusters = list(best_K_hier_corr$results[[1]], best_K_hier_corr$results[[2]], 
					 best_K_som_corr$results[[3]], best_K_som_corr$results[[4]])

best_k = c(best_K_hier_corr$kmax[[1]], best_K_hier_corr$kmax[[2]], 
		   best_K_som_corr$kmax[[3]], best_K_som_corr$kmax[[4]])

save(best_clusters,file = "bestclusters1.Rdata")
save(best_k,file = "bestk1.Rdata")

# 
# #Jaccard, Rand, NMI index

ind_eH_eC = calculateIndex(clust_hier_eucl, clust_hier_corr)
ind_eH_sH = calculateIndex(clust_hier_eucl, clust_som_eucl)
ind_eH_sC = calculateIndex(clust_hier_eucl, clust_som_corr)
ind_eC_sH = calculateIndex(clust_hier_corr, clust_som_eucl)
ind_eC_sC = calculateIndex(clust_eucl_corr, clust_som_corr)
ind_sH_sC = calculateIndex(clust_som_eucl,  clust_som_corr)




# #error (confusion matrix) (I create table with errors)
# #for each view I have 4 types of clustering => 4 errors


error  = NULL
globalConfusionMatrix = NULL
#quanti elementi del cluster i appartengono alla classe j, per ogni vista
for(i in 1:nView)
{
  #calculate conf matrix
  cM_eh 	= confusionMatrix(view[i], classes, clust_hier_eucl[[i]])
  cM_ec 	= confusionMatrix(view[i], classes, clust_hier_corr[[i]])
  cM_sh 	= confusionMatrix(view[i], classes, clust_som_eucl[[i]])
  cM_sc 	= confusionMatrix(view[i], classes, clust_som_corr[[i]])
  globalConfusionMatrix[[i]] = list(cM_eh, cM_ec, cM_sh, cM_sc)
  #calculate error
  #somma per riga
  error[[i]] 	= c(calculateError(cM_eh), 
  				    calculateError(cM_ec), 
  				    calculateError(cM_sh), 
  				    calculateError(cM_sc))
}

# #print table only for choosen cluster

 dat = c(error[[1]][2], error[[2]][2], error[[3]][4], error[[4]][4])
 mat_errors = matrix(dat, nrow = 4, ncol = 1, byrow = F)
 rownames(mat_errors)=getLabelsView()
 colnames(mat_errors)="error"
 textplot(mat_errors)
 title("Errors")

#conf matrix (only for choosen clusters)

#conf_mat =list(globalConfusionMatrix[[1]][[2]], globalConfusionMatrix[[2]][[4]],
#               globalConfusionMatrix[[3]][[4]], globalConfusionMatrix[[4]][[2]])

conf_mat = list(confusionMatrix(view[1], classes, best_clusters[[1]]), 
				confusionMatrix(view[2], classes, best_clusters[[2]]),
				confusionMatrix(view[3], classes, best_clusters[[3]]),
				confusionMatrix(view[4], classes, best_clusters[[4]]))

#print hist of confusion matrix (only clusters choosen)
plotHist(conf_mat)

errors 	= c(calculateError(conf_mat[[1]]), 
				calculateError(conf_mat[[2]]), 
				calculateError(conf_mat[[3]]), 
				calculateError(conf_mat[[4]]))


#views' order : ("yield", "range", "volatility", "Volume")


#load("bestclusters.Rdata") 
#load("bestk.Rdata")

mvK = 8
multi_view_matrix = buildMultiViewMatrix(best_clusters, best_k)
PH = matrix_factoritation(multi_view_matrix, mvK, 1/100000, 10000)       
#contribution matrix
T = find_T(PH$P, mvK, best_k)
plot_T(T)

#features 
H = PH$H

#obtain cluster
clusters = getClusters(H);
#get value of features
nc=c()
colnames=c()
for(i in 1:length(data_extract))
{
  nc = c(nc, dim(data_extract[[i]])[2])
  ncol = sum(nc)
  colnames = c(colnames, colnames(data_extract[[i]]))
}
mvClustersFeatures = matrix(data = 0, nrow = dim(data_extract[[1]])[1], ncol = ncol)
colnames(mvClustersFeatures) = colnames

mvClustersSize = c(length(clusters[[1]]),
					length(clusters[[2]]), 
        			length(clusters[[3]]), 
        			length(clusters[[4]]), 
        			length(clusters[[5]]), 
        			length(clusters[[6]]),
        			length(clusters[[7]]),
        			length(clusters[[8]]))

for(i in 1:length(clusters))
{
	clst 	= clusters[[i]]
	min 	= sum(mvClustersSize[i-1:i])+1 #1
  	j 		= min
	for(k in 1:length(clst))
	{
	  	ind 		= clst[k]
	  	first_view 	= (data_extract[[1]][ind,])
	  	second_view = (data_extract[[2]][ind,])
	  	third_view 	= (data_extract[[3]][ind,])
	  	fourth_view = (data_extract[[4]][ind,])
	 	mvClustersFeatures[j,]		= c(first_view, second_view, third_view, fourth_view)
    	j = j+1
	}
}




#plotting hist a quale classe appartengono ogni elemento in un meta-cluster
which_classes(clusters, classes)


mvClusterMembershipVector  <- buildClusterMembershipVector(clusters)

mvSClusterMembershipVector <- buildClusterMembershipVectorSector(clusters, data)
mvSClusterMembershipVector <- as.integer(mvSClusterMembershipVector)

mvConfusionMatrix <- confusionMatrix("multi", classes, mvClusterMembershipVector)
plotHist(list(mvConfusionMatrix), c("multi"))


plotPie(mvSClusterMembershipVector, clusters, data, 8)

labV = DifferencialVariance(mvClusterMembershipVector, mvClustersFeatures, "./")
lab = labV[[1]]
#plot distribution only of N features of ranking for each cluster
#plotDistr(mvClustersFeatures, mvClusterMembershipVector, labV[[2]], N=5, suffix="A")
plotDistr(mvClustersFeatures, mvClusterMembershipVector, labV[[1]], N=5, suffix="T")

cdat <- as.list(as.data.frame((as.matrix(mvClustersFeatures))))
selectedFea =c()
featureScore = list()
for(i in 1:dim(lab)[1])
{
	for(j in 1:5)
	{
		ft = lab[i, j]
		selectedFea = union(selectedFea, ft)
		if (is.null(featureScore[[ft]]))
		{
			featureScore[[ft]] = 0
		}
		featureScore[[ft]] = featureScore[[ft]] + 1
	}	
}

featureScore = as.sorted.data.frame(featureScore)
	
colours = rainbow(dim(lab)[1])

for(i in 1:dim(lab)[1])
{
	params=list()
#	offset   = sum(mvClustersSize[i-1:i])+1 #1 
	
	# 	for(j in 1:length(selectedFea))
	# 	{
	# 		indice = which(colnames==selectedFea[j])
	# 		params[[j]] = (cdat[[indice]][offset:mvClustersSize[i]])
	# 	}	
	
	count = 1;
	selLabel = c()
	for(j in 1:dim(lab)[2])
	{
		for(k in 1:length(selectedFea))
		{
			if(lab[i,j] == selectedFea[k])
			{
				selLabel = c(selLabel, lab[i,j])
				index = which(colnames==selectedFea[k])
# 				end = (offset+mvClustersSize[i]-1)
# 				params[[count]] = (cdat[[indice]][offset:end])
				cl = mvClustersFeatures[mvClusterMembershipVector==i,]
				params[[count]] = cl[,index] #(cdat[[names[j]]][offset:end])
				count = count+1
			}
		}
	}
	params['names'] = list(rep("", length(selLabel)))
	params['col'] = list(colours[i])
	png(paste("union",i,".png",sep=""), width=16, height=6, units="in", res=300)
	par(mar =  c(9, 4, 4, 2) + 0.1)
	#par(mar=c(8, 4, 4, 2) + 0.1)
	do.call(vioplot, params)
	axis(1, las=2, at=1:length(selLabel), labels = selLabel, )
	dev.off()
}

names = row.names(featureScore)
colours=rainbow(mvK)
for (j in 1:6)
{
	cat(names[j],'\n')
	png(paste("Density_", names[j],".png",sep=""), width=16, height=6, units="in", res=300)
	par(mar =  c(9, 4, 4, 2) + 0.1)
	params = list()
	labels = c()
	for (i in 1:mvK)
	{
	#	offset   = sum(mvClustersSize[i-1:i])+1
	#	end 	 = (offset+mvClustersSize[i]-1)
	#	cat(cdat[[names[j]]][offset:end],'\n')
	#	cat(sd(cdat[[names[j]]][offset:end]), '\n')
		cl1 = mvClustersFeatures[mvClusterMembershipVector==i,]
		params[[i]] = cl1[,names[j]] #(cdat[[names[j]]][offset:end])
		labels = cbind(labels, paste("cl", as.character(i), sep=""))
	}
	plot.multi.dens(params, colours)
	#hist(params[[1]])
	legend("topleft", legend = labels, fill = colours, cex=0.9)
	title(paste("Density - ", names[j]))
	dev.off()
}


# for(j in 1:length(selectedFea))
# {
# 	params=list()
# 	
# 	for(i in 1:dim(lab)[1])
# 	{
# 		offset   = sum(mvClustersSize[i-1:i])+1 #1
# 		indice = which(colnames==selectedFea[j])
# 		params[[i]] = (cdat[[indice]][offset:mvClustersSize[i]]) 
# 	}
# 	params['names'] = list(rep("", 8))
# 	png(paste("unionB",selectedFea[j],".png",sep=""), width=16, height=6, units="in", res=300)
# 	#par(mar=c(8, 4, 4, 2) + 0.1)
# 	do.call(vioplot, params)
# 	axis(1, las=2, labels = c("1","2","3","4","5","6","7","8"), at=1:8)
# 	dev.off()
# }


m_sd = plotDistrEx(mvK, mvClustersFeatures, mvClusterMembershipVector, featureScore)

names = read.csv("raw-data//names.csv")
Names = c()
for (i in 1:dim(names)[1])
{
	
}

toXLS = list()
for (i in 1:mvK)
{
	Symbol = data$symbols[mvClusterMembershipVector==i]
	Sector = data$class[mvClusterMembershipVector==i]
	Subsector = data$sector[mvClusterMembershipVector==i]
	dd = data.frame(Symbol, Sector, Subsector)
	WriteXLS("dd", ExcelFileName = paste("metacluster", i, ".xls", sep=""), AdjWidth = TRUE)
}
