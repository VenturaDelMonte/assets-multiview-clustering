# The kohonen Package
	# http://www.jstatsoft.org/v21/i05/paper
	if(!require(kohonen)){
		install.packages("kohonen")	#for SOM with euclidean as measure
		library(kohonen)
	}
	require(CorKohonen) #for SOM with Pearson correlation as measure (Angela's Package)
#http://cran.r-project.org/web/packages/amap/amap.pdf
	if(!require(amap)){
		install.packages("amap") 	#for function hcluster
		library(amap)
	}
	if(!require(cluster)){
		install.packages("cluster")
		library(cluster)
	}

	if(!require(clusterCrit)) 	#for Jaccard and Rand indices
	{
		install.packages("clusterCrit")
		library(clusterCrit)
	}
	if(!require(SNFtool))		#for NMI index
	{
		install.packages("SNFtool")
		library(SNFtool)
	}
	if(!require(gplots)) 	#for Jaccard and Rand indices
	{
		install.packages("gplots")
		library(gplots)
	}
#' Extraction of features 
#'
#' @details For each variables (yield, range, volatility, 
#' volume, closing.price.adjusted),
#' extract only the features which are relevant, 
#' thus, delete those that are more or less uniformly distribuited.
#' 
#' For variables Yield delete: Periodicity, TSA Trend, TSA Seasonality,
#' Self-sim, Chaos.
#' For variables Volume delete: Serial Corr, Self-sim
#' For variables closing.price.adjusted delete: Periodicity, TSA Trend, Serial-Correlation,
#'  TSA Seasonality, Chaos
#' @param  storage The data-frame with the variables and features
#' @return storage.extract The data-frame with the variables and features extract
#' @export 
#'

extraction.Feature <- function(storage)
{
  storage.extract = NULL
  label = getLabelsFeatures()
  
  storage.extract$Yield 				= storage[[1]][,c(3,4,5,6,9,10,11,12)]  
  colnames(storage.extract$Yield) 		= paste(label[c(3,4,5,6,9,10,11,12)],"Y", sep="")

  storage.extract$range                	= storage[[2]]
  colnames(storage.extract$range) 		= paste(label,"R", sep="")

  storage.extract$volatility          		= storage[[3]][,c(4,5,6,7,10,11,12)]
  colnames(storage.extract$volatility) 		= paste(label[c(4,5,6,7,10,11,12)],"VY", sep="")

  storage.extract$volume                = storage[[4]]
  colnames(storage.extract$volume) 		= paste(label,"VL", sep="")

  #storage.extract$closing.price.adjusted = storage[[5]]
 
  return(storage.extract)  
}

#' Find clusters' centers
#'
#' @details The function find the clusters' centers
#' @param data matrix data
#' @param cluster vector with cluster calculated
#' @return center vector with centers' cluster
#' @export 
#'
findCenter <- function(data, cluster)
{
    center            = NULL #centers' matrix
    name_center       = c()
    elem_par_cluster  = table(cluster)

    for(i in names(elem_par_cluster))
    {
      if(elem_par_cluster[i] > 1)
      {
        clustElem   = data[which(cluster==i), ]
        matCor      = cor(t(clustElem), method="pearson");
        bestCenter  = which.max(apply(matCor, 1, FUN=function(riga){
                                sum(riga) 
                                }))
        center      = rbind(center, clustElem[bestCenter,])
        name_center = c(name_center, attr(bestCenter,which="names"))
      }
      else #one element in the cluster
      { 
        clustElem   = data[which(cluster==i),]
        center      = rbind(center,clustElem)
        name_center = c(name_center,rownames(data)[which(cluster==i)])
      }
    }
    rownames(center)= name_center

    return(center)
}


#' Hierarchical Cluster Analysis
#'
#' @details This function clusters each assets for each view (yield, range, volatility, 
#' volume, closing.price.adjusted), using Hierarchical Cluster with 
#' METHOD: 	Ward
#' MEASURE: euclidean and Pearson
#'
#' @param  meas The data-frame with the variables and features exctract
#' @return results the vector with clustering's results. 
#'			results[[i]][[1]] 	to get cluster with euclidean measure
#'			results[[i]][[2]]	to get cluster with pearson measure
#'
#'          results[[1]] is for yield
#'          results[[2]] is for range
#'          results[[3]] is for volatility
#'          results[[4]] is for volume
#'          results[[5]] is for closing.price.adjusted
#' @export 
#'
clustWard <- function(meas, drawPlot = FALSE)
{
	labView	= getLabelsView()
	results = NULL
	for(i in 1:length(meas))	# 1:5
	{
		#Cluster Ward's method and distance euclidean
		#	png(paste(paste("Dend", labView[i], "eucl", sep="_"), "png",sep="."),
		#             width=16, 
		#             height=9, 
		#             units="in",
		#             res=300)
		hcl_euclidean = hcluster(meas[[i]], method="euclidean", link="ward")
		if (drawPlot == TRUE){
			plot(hcl_euclidean, main = paste(labView[i], "with distance euclidean", sep="."))
		}
		#     dev.off()

		#Cluster Ward's method and distance correlation (Centered Pearson)
		#     png(paste(paste("Dend", labView[i], "corr", sep="_"), "png",sep="."),
		#         width=16, 
		#         height=9, 
		#         units="in",
		#         res=300)
		hcl_correlation = hcluster(meas[[i]], method="correlation", link="ward")
		if (drawPlot == TRUE){
			plot(hcl_correlation, main = paste(labView[i], "with correlation", sep="."))
		}
		#     dev.off()

		results[[i]] 	= list(eucl = hcl_euclidean, corr = hcl_correlation)
		print(paste("Hierarchical Clusters for the view: ", labView[i]))
	}
	return(results)
}
	
plotDend <- function(clusters)
{
	labView	= getLabelsView()
	n = length(clusters)
	for (i in 1:n)
	{
		png(paste(paste("Dend_", labView[i], "_eucl", sep="_"), "png",sep="."),
			width=16, 
			height=9, 
			units="in",
			res=300)
		plot(clusters[[i]]$eucl, main = paste(labView[i], "with euclidean distance", sep=" "))
		dev.off()
		png(paste(paste("Dend_", labView[i], "_pear", sep="_"), "png",sep="."),
			width=16, 
			height=9, 
			units="in",
			res=300)
		plot(clusters[[i]]$corr, main = paste(labView[i], "with euclidean distance", sep=" "))
		dev.off()
	}
		
}

#' SOM
#'
#' @details This function clusters each assets for each view (yield, range, volatility, 
#' volume, closing.price.adjusted), using SOM. 
#' METHOD: 	SOM
#' MEASURE: euclidean and Pearson
#'
#' @param  meas The data-frame with the variables and features exctract
#' @param  x_grid grid's xlim
#' @param  y_grid grid's ylim
#' @return results the vector with clustering's results. 
#'			results[[i]][[1]] 	to get cluster with euclidean measure
#'			results[[i]][[2]]	to get cluster with pearson measure
#'
#'          results[[1]] is for yield
#'          results[[2]] is for range
#'          results[[3]] is for volatility
#'          results[[4]] is for volume
#'          results[[5]] is for closing.price.adjusted
#' @export 
#'
clustSOM <- function(data, x_grid, y_grid, plotSOM = FALSE)
{

	
	labView		= getLabelsView()
	results 	= NULL

	# Create the SOM Grid - you generally have to specify the size of the 
	# training grid prior to training the SOM. Hexagonal and Circular 
	# topologies are possible.
	# 10*15 nodes
	som_grid 	= somgrid(xdim = x_grid, ydim = y_grid, top="rectangular")

	for(i in 1:length(data)) # 1:5
	{
	  	# Create a training data set (rows are samples, columns are variables
		# 
		# Here we are selecting a subset of my variables available in "data"
		#
		data_train = data[[i]]

		# Change the data frame with training data to a matrix
		# Also center and scale all variables to give them equal importance during
		# the SOM training process. 
		#
		# In our case, we are doing clustering, so our training matrix is original matrix
		#
		data_train_matrix = as.matrix((data_train))

		# Finally, train the SOM (with euclidean and correlation as measureS), options for the number of iterations,
		# the learning rates, and the neighbourhood are available
		#
		som_model_eucl	= kohonen::som(data_train_matrix, 
						   grid      	= som_grid, 
						   rlen      	= 100,                 	# number iteration
						   alpha     	= c(0.05,0.01),        	# default
						   keep.data	= TRUE,         
						   n.hood		= "square" )         	# the shape of the neighbourhood
		som_model_corr 	= CorKohonen::som(data_train_matrix, 
										grid 		= som_grid, 
										rlen      	= 100,                 	# number iteration
										alpha     	= c(0.05,0.01),        	# default
										keep.data 	= TRUE,         
										n.hood		= "square" )      		# the shape of the neighbourhood
										
		# This plot option shows the progress over time. 
		# If the curve is continually decreasing,
		# more iterations are required. 
		#
		# plot(som_model_eucl, type="changes", main = labView[i])
		# plot(som_model_corr, type="changes", main = labView[i])
		#Visualise the count of how many 
		# samples are mapped to each node on the map.
		#This metric can be used as a measure of map quality – 
		# ideally the sample distribution is relatively uniform. 
		#Large values in some map areas suggests that a larger map 
		# would be benificial. Empty nodes indicate that your map size is too 
		# big for the number of samples
		#
		if (plotSOM == TRUE)
		{
			plot(som_model_eucl, type ="counts", main="euclidean")
			CorKohonen::plot.kohonen(som_model_corr, type ="counts", main="correlation")
		}
	
    
		# Plot the UMatrix
		# In this plot it is possible visualize how each neuron is distant from other
		# IMPORTANT!!
		#
		#plot(som_model_eucl, type ="dist.neighbours", main = labView[i])
		#plot(som_model_corr, type ="dist.neighbours", main = labView[i])

		#The node weight vectors, or “codes”, 
		# are made up of normalised values ofy the original variables 
		# used to generate the SOM. Each node’s weight vector is 
		# representative / similar of the samples mapped to that node. 
		#By visualising the weight vectors across the map, 
		# we can see patterns in the distribution of samples and variables. 
		#The default visualisation of the weight vectors is a “fan diagram”, 
		# where individual fan representations of the magnitude of each variable 
		#in the weight vector is shown for each node.
		#   
		# Shows distribution of variables across map
		# Can see patterns by examining dominant colours

		#plot(som_model_eucl, type="codes")
		#plot(som_model_corr, type="codes")
		#   A SOM heatmap allows the visualisation of the distribution of 
		#   a single variable across the map. Typically, a SOM investigative 
		#   process involves the creation of multiple heatmaps, 
		#   and then the comparison of these heatmaps to identify 
		#   interesting areas on the map

		# plot(som_model, type = "property", property = som_model$codes[,4], 
		#        main=names(som_model$data)[4], palette.name=coolBlueHotRed )
		# 
		#   plot(som_model, type="quality", palette.name = coolBlueHotRed)
		#   plot(som_model, type="mapping", 
		#        labels = as.factor(c(1:10)),
		#        main = "mapping plot")
		#   

		## use hierarchical clustering to cluster the codebook vectors
		# som_cluster_eucl = cutree(hclust(dist(som_model_eucl$codes)), 6)
		# som_cluster_corr = cutree(hclust(dist(som_model_corr$codes)), 6)

		# plot these results:
		#   pretty_palette = c("green", "yellow","red", "#1f77b4","#ff7f0e","#2ca02c","#d62728")
		#   plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
		#   add.cluster.boundaries(som_model, som_cluster)
		results[[i]]	= list(eucl = som_model_eucl, corr = som_model_corr)
		print(paste("SOM Clusters for the view: ", labView[i]))
	}
	return (results)
}

drawSom <- function (soms)
{
	labView	= getLabelsView()
	n = length(labView)
	for (i in 1:n)
	{
		png(paste(paste("Som_", labView[i], "_eucl", sep="_"), "png",sep="."),
			width=9, 
			height=9, 
			units="in",
			res=300)
		
		plot(soms[[i]]$eucl, type ="counts", main="euclidean")
		dev.off()
		
		png(paste(paste("Som_", labView[i], "_pear", sep="_"), "png",sep="."),
			width=9, 
			height=9, 
			units="in",
			res=300)
		CorKohonen::plot.kohonen(soms[[i]]$corr, type ="counts", main="correlation")
		dev.off()
	}
}
	
	
# drawSomUMatrix <- function (soms)
# {
# 	labView	= getLabelsView()
# 	n = length(labView)
# 	for (i in 1:n)
# 	{
# 		png(paste(paste("uSom_", labView[i], "_eucl", sep="_"), "png",sep="."),
# 			width=9, 
# 			height=9, 
# 			units="in",
# 			res=300)
# 		
# 		plot(soms[[i]]$eucl, type ="dist.neighbours", main="euclidean")
# 		dev.off()
# 		
# 		png(paste(paste("uSom_", labView[i], "_pear", sep="_"), "png",sep="."),
# 			width=9, 
# 			height=9, 
# 			units="in",
# 			res=300)
# 		CorKohonen::plot.kohonen(soms[[i]]$corr, type ="dist.neighbours", main="correlation")
# 		dev.off()
# 	}
# }

#' Calculate best cutree for Cluster Analysis only for 
#' that used euclidean distance
#'
#' @details This function Calculate best cutree for each view (yield, range, volatility, 
#' volume, closing.price.adjusted) using the silhouette coefficient, ONLY for custer that utilize euclidean measure.
#' 
#' http://stats.stackexchange.com/questions/10540/how-to-interpret-mean-of-silhouette-plot
#' 
#' @param kmax max value that k might assume
#' @param clust_hier clusters calculated previously with hierarchical method
#' @param clust_som clusters calculated previously 	with SOM
#' @param data_mat The data-frame with all view
#' @param is_som  boolean value. If it is F then the cluster is hierarch,
#'			else it is SOM
#' @return results the list with vector obtained by cutree ([1:502]) with best k 
#' 			results[[i]][[1]] for hierarchical method 
#'			results[[i]][[2]] for SOM
#'
#'          results[[1]] is for yield
#'          results[[2]] is for range
#'          results[[3]] is for volatility
#'          results[[4]] is for volume
#'          results[[5]] is for closing.price.adjusted
#' @return k_max value of k for each view cluster
#' @export 
clusterWithBestKEuclidean <- function(kmax, cluster, data_mat, is_som=FALSE, plotGraph=T)
{
	
	labView		= getLabelsView()
	results 	= NULL
	nView		= length(cluster)
	k_max 		= NULL
	for(j in 1:nView)  
	{
		
		sil   = rep(0, kmax)
		groups.objects = list()
		for(i in 2:kmax)  #iteration to calculate coefficient
		{ 
			if(!is_som) {
				groups 	= cutree(cluster[[j]][[1]], i)
				sl		= silhouette(groups, dist(data_mat[[j]]))  
			}
			else{
				
				codes = cluster[[j]][[1]]$codes
				winning.units = cluster[[j]][[1]]$unit.classif
				
				groups.neurons 	= cutree(hclust(dist(codes)), i)
				groups.objects[[i]] = rep(NA, length(winning.units))
				
				for(o in 1:length(groups.neurons))
				{
					groups.objects[[i]][which(winning.units == o)] = groups.neurons[o]
				}
				
				sl 		= silhouette(groups.objects[[i]], dist(data_mat[[j]])) 
			}
			
			sil[i]		= mean(sl[,3]) #width sl
		}
		k_max = c(k_max, which.max(sil) )
		if(!is_som)
		{
			results[[j]] =  cutree(cluster[[j]][[1]], k_max[j])
		}
		else
		{
			results[[j]] = 	groups.objects[[k_max[j]]] #cutree(hclust(dist(cluster[[j]][[1]]$codes)), k_max[j])
		}
		#if you plot silhouette's value
		if (plotGraph==TRUE)
		{
			png(paste(paste("ChoosenK", labView[j], "eucl", ifelse(is_som, "Som", "Hier"), sep="_"), "png",sep="."),
					width=9, 
					height=9, 
					units="in",
					res=300)
			x=1:kmax
			labels = as.character(c(1:kmax))
			plot (x, sil,col=ifelse(x==k_max[j], "red", "black"), main=paste(labView[j], as.character(c("K chosen")), "eucl", ifelse(is_som, "Som", "Hier"), sep=" "), axes = F, xlab = "Value of K", ylab = "")
			lines(x, sil)
			axis(1, at=1:length(labels), labels=labels)
			axis(2)
			box()
			legend('bottomright', paste(as.character(c("K chosen")), "eucl", ifelse(is_som, "Som", "Hier"), sep=" "), lty=1, col=c('red'), bty='n', cex=.75, lwd=1, pch=c(15))
			dev.off()
		}
	
	}
	return (list(kmax = k_max, results = results))
}


#' Calculate best cutree for Cluster Analysis only for 
#' that used pearson distance
#'
#' @details This function Calculate best cutree for each view (yield, range, volatility, 
#' volume, closing.price.adjusted) using the correlation coefficient as misure
#' 
#' #http://www.cs.kent.edu/~jin/DM08/ClusterValidation.pdf
#' 
#' @param kmax max value that k might assume
#' @param cluster clusters calculated previously 
#' @param data The data-frame with all view
#' @param is_som boolean value. If it is F then the cluster is hierarch,
#'			else it is SOM
#' @return results the list with vector obtained by cutree [1:502] with best k 
#' 			results[[i]][[1]] for hierarchical method 
#'			results[[i]][[2]] for SOM
#'
#'          results[[1]] is for yield
#'          results[[2]] is for range
#'          results[[3]] is for volatility
#'          results[[4]] is for volume
#'          results[[5]] is for closing.price.adjusted
#' @return k_max value of k for each view cluster
#' @export 
clusterWithBestKCorrelation <- function(kmax, cluster, data, is_som = FALSE, plotGraph = T)
{
	if(!require(gdata)){
		install.packages("gdata")# for the lower triangle matrix
		library(gdata)
	}  
	results_m   = NULL
  	mean_intra	= NULL
  	mean_inter	= NULL
  	results 	= NULL
  	n 			= length(cluster)
  	k_max		= NULL

	for(j in 1:n)  # 1:5
	{
		groups.objects = list()
		for(k in 2:kmax)  #iteration to calculate coefficient
		{

			corr_cluster 	= NULL
			corr_mean 		= NULL
			# correlation matrix 502x502 
			# how each elements is correlated with each other
			corr_matrix = cor(t(data[[j]]), use="pairwise.complete.obs")  

			group = NULL
			if(!is_som){
				group 	= cutree(cluster[[j]][[1]], k) 
			}
			else{
			#	group	= cutree(hclust(dist(cluster[[j]][[1]]$codes)), k)
				codes = cluster[[j]][[1]]$codes
				winning.units = cluster[[j]][[1]]$unit.classif
				
				groups.neurons 	= cutree(hclust(dist(codes)), k)
				groups.objects[[k]] = rep(NA, length(winning.units))
				
				for(o in 1:length(groups.neurons))
				{
					groups.objects[[k]][which(winning.units == o)] = groups.neurons[o]
				}
				group = groups.objects[[k]]
			}

			for(z in 1:k)
			{
				cl 					= which(group==z)
				corr_mean	[[z]] 	= mean(corr_matrix[cl, cl])
			}


			# calculate correlation's mean intra cluster 
			
			mean_intra[[k]]   	= mean(corr_mean)


			# calculate correlation's mean inter center
			if(!is_som)
			{	
				center_matrix  	= findCenter(data[[j]], group)
			}
			else
			{
			#	center_matrix  	= findCenter(cluster[[j]][[1]]$codes, group)
				center_matrix  	= findCenter(data[[j]], group)
			}
			correl_center		= cor(t(center_matrix), use="pairwise.complete.obs")
			mean_inter[[k]]  	= mean(lowerTriangle(correl_center))
		}
		groups.objects = NULL
		results_m[[j]] 	= list(intra = mean_intra [2:kmax], inter = mean_inter [2:kmax])    #I need it for plot

		#value of best k for each view. We take k such that it is max the distance between 
		#intra correlation line and inter correlation line
		k_max = c(k_max, which.max(abs(mean_intra - mean_inter)))
	}

	#Plot to decide which k is best
	if (plotGraph == TRUE)
	{
		main 	= getLabelsView()
		labels 	= as.character(c(2:10))
		for (i in 1:n) 
		{
			png(paste(paste("ChoosenK", main[i], "pear", ifelse(is_som, "Som", "Hier"), sep="_"), "png",sep="."),
				width=9, 
				height=9, 
				units="in",
				res=300)
			x 	= results_m[[i]][[1]] #tra
			y 	= results_m[[i]][[2]] #inter
			plot(x, ylim = range(c(x, y)), axes = F, xlab = paste("Value of K - ", "pear", ifelse(is_som, "Som", "Hier"), sep=" "), ylab = "", type="l", main = main[i], col = "green")
			lines(y, col = "red")
			points(k_max[i]-1, x[k_max[i]-1])
			points(k_max[i]-1, y[k_max[i]-1])
			axis(2)
			axis(1, at=1:length(x), labels=labels)
			box()
			legend(ifelse(k_max[i]>5,'bottomleft','bottomright'), as.character(c("INTER-CORRELATION", "INTRA-CORRELATION")), lty=1, col=c('red', 'green'), bty='n', cex=.75)
			dev.off()
		}
	}
	

	for(j in 1:n)
	{
		cut = NULL
		if(!is_som)
		{
			cut = cutree(cluster[[j]][[1]], k_max[j] )
		}
		else
		{
		#	cut = cutree(hclust(dist(cluster[[j]][[1]]$codes)), k_max[j])
			codes = cluster[[j]][[1]]$codes
			winning.units = cluster[[j]][[1]]$unit.classif
			
			groups.neurons 	= cutree(hclust(dist(codes)), k_max[j])
			groups.objects = rep(NA, length(winning.units))
			
			for(o in 1:length(groups.neurons))
			{
				groups.objects[which(winning.units == o)] = groups.neurons[o]
			}
			
			cut = groups.objects
		}
		results[[j]] = cut
	}
	return (list(kmax = k_max, results = results))
}

#' Calculate best cutree for Cluster Analysis only for 
#' that used pearson distance
#'
#' @details This function Calculate best cutree for each view (yield, range, volatility, 
#' volume, closing.price.adjusted) using the correlation coefficient as misure
#' 
#' #http://www.cs.kent.edu/~jin/DM08/ClusterValidation.pdf
#' 
#' @param clust_som cluster obtained with SOM
#' @param best_cluster. this vector's size is equal SOM grid's size. 
#'
#' @return results the list with vector with cluster 
#' 			results[[i]][[1]] for euclidean
#'			results[[i]][[2]] for correlation
#'
#'          results[[1]] is for yield
#'          results[[2]] is for range
#'          results[[3]] is for volatility
#'          results[[4]] is for volume
#'          results[[5]] is for closing.price.adjusted
#' @export 
getClusterSOM <- function(clust_som, best_cluster)
{
	n 		= length(clust_som)
	results = NULL
	for(i in 1:n)
	{
		unif_classif_eucl 	= clust_som[[i]][[1]]$unit.classif  #1:502
		unif_classif_corr 	= clust_som[[i]][[2]]$unit.classif
		neur_clust_eucl		= best_cluster[[1]][[i]]			#1:54
		neur_clust_corr		= best_cluster[[2]][[i]]			
		
		n_c 				= length(neur_clust_corr)

		tmp_eucl 			= rep(NA, length(unif_classif_eucl))
		tmp_corr 			= rep(NA, length(unif_classif_corr))

		for(j in 1:n_c)
		{

			ind_titles 	= which(unif_classif_eucl == j) # all objects associated to the j-th neuron
			clust 		= neur_clust_eucl[j] # j-th neuron -> k-th clust
			for(k in 1:length(ind_titles))
			{
				tmp_eucl[ind_titles[k]] = clust
			}
			ind_titles 	= NULL
			clust 		= NULL
			ind_titles 	= which(unif_classif_corr == j) # all objects associated to the j-th neuron
			clust 		= neur_clust_corr[j] # j-th neuron -> k-th clust
			for(k in 1:length(ind_titles))
			{
				tmp_corr[ind_titles[k]] = clust
			}
		}
		results[[i]] = list(eucl = tmp_eucl, corr = tmp_corr)
	}
	return(results)
}


	
#'
#' @details This function calculateJaccard, Rand, NMI indices for each view (yield, range, volatility, 
#' volume, closing.price.adjusted)  
#'
#' You can evaluate the goodness of the obtained clustering results by calculate Normalized mutual information (NMI): 
#' if NMI is close to 1, 
#'		it indicates that the obtained clustering is very close to the "true" cluster information; 
#' if NMI is close to 0,  
#'		it indicates the obtained  clustering is not similar to the "true" cluster information.
#' 
#' @param group_eucl group with euclidean measure
#' @param group_pear group with correlation measure
#' @return results is the list with: 
#' 			results[[i]][[1]] for Jaccard index
#'			results[[i]][[2]] for Rand index
#'			results[[i]][[3]] for NMI index
#'
#'          results[[1]] is for yield
#'          results[[2]] is for range
#'          results[[3]] is for volatility
#'          results[[4]] is for volume
#'          results[[5]] is for closing.price.adjusted
#' @export 
calculateIndex <- function(clust_euc, clust_corr)
{
	n 		= length(clust_euc)
	results = NULL

	for(i in 1:n)
	{
		jaccard 		= extCriteria(clust_euc[[i]], clust_corr[[i]], "Jaccard")[[1]]
		rand			= extCriteria(clust_euc[[i]], clust_corr[[i]], "Rand")[[1]]
		nmi				= calNMI(clust_euc[[i]], clust_corr[[i]])			
		results[[i]]	= (list(jacc = jaccard, rand = rand, nmi = nmi))
	}
	return (results)
}

#'
#' @details This function calculate the confusion matrix 
#'
#' @param classes classes original
#' @param cluster clusters calculated
#' @return Confusion_Matrix confusion matrix
#' @export 
#'
confusionMatrix <- function(view, classes, cluster){
	
	nClass 			= length(table(classes))
	nCluster 		= length(table(cluster))
	labCLasses		= unique(classes)
	matrix_summary 	= matrix(classes, ncol=1, nrow = length(classes));

	for(i in 1:nClass)
	{
		index = which(matrix_summary[,1] == attr(table(classes)[i], which="name"));
		matrix_summary[index,1] = i;
	}

	confusion_Matrix = matrix(0,nrow=nCluster,ncol=nClass)
	for(i in 1:nCluster)
	{
		tab_i 	= table(matrix_summary[which(cluster==i),1]);
		n 		= length(tab_i)
		for(j in 1:n)
		{
			confusion_Matrix[i, as.integer(attr(tab_i[j], which = "name"))] = tab_i[j]
		}
	}
	rownames(confusion_Matrix) = paste("clst", 1:nCluster, sep = "")
	colnames(confusion_Matrix) = paste("classe", 1:length(labCLasses), sep = " ")
	textplot(confusion_Matrix)
	title(paste("Confusion Matrix of", view, "view", sep=" "))
	return (confusion_Matrix)
}

#'
#' @details This function calculate the impurity(error) of confusion matrix
#'
#' To get the error you must calculate 1-purity
#'
#' @param confusion_matrix confusion matrix
#'
#' @return confusion matrix's impurity
#' @export 
#'
calculateError <- function(confusion_matrix)
{
	#total elements in the matrix
	total_elements 		= sum(confusion_matrix)

	total_without_max 	= NULL

	#sum of elements for each row without the max value
	apply(confusion_matrix, 1, FUN = function(row){
		sum(row[-which.max(row)])
	}) -> total_without_max

	#calculate the purity of matrix
	purity = sum(total_without_max)/total_elements

	#return the error
	return (err = 1-purity)
}
