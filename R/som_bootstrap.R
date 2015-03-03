if(!require(parallel))
{
	install.packages("parallel")
	library(parallel)
}
if(!require(doParallel))
{
	install.packages("doParallel")
	library(doParallel)
}


bootstrap <- function(data_extract, numIter = 100)
{	
	init_done <- F
	cores <- detectCores(T, T)
	cl <- makeCluster(cores)

	this.env <- environment()
	while( identical( this.env, globalenv() ) == FALSE )
	{
		clusterExport(cl, ls(all.names=TRUE, env=this.env), envir=this.env)
		this.env <- parent.env(environment())
	}
	clusterExport(cl, ls(all.names=TRUE, env=globalenv()), envir=globalenv())

	
	start = Sys.time()
	registerDoParallel(cl)
	
	
  
	m <- foreach (i = 1:numIter, .combine = rbind) %dopar% 
	{
		if (!init_done)
		{
			library(parallel)
			source("cluster.r")
			source("utils.r")
			as.numeric(Sys.time()) -> t
			s <- (t - floor(t)) * 1e8 * (2^(i/cores))
			set.seed(s)
      		init_done <- T
		}
		Sys.sleep(0.15)	
		results_som 	<- clustSOM						(data_extract, 6, 9, F)
		best_K_som_eucl	<- clusterWithBestKEuclidean	(10, results_som, data_extract, T, F)
		best_K_som_corr <- clusterWithBestKCorrelation	(10, results_som, data_extract, T, F)
		c(best_K_som_eucl['kmax'][[1]],best_K_som_corr['kmax'][[1]])
	}
  
  
	stopCluster(cl)
	diff = Sys.time() - start
	print(diff)
	size = dim(m)
	ret = list()
	for (i in 1:size[2])
	{
		ret[i] = list(mapply(function(x){return (x/numIter)}, tabulate(m[,i], 12)))
	}
	
	return(list(ret = ret, m = m))
}