if(!require(vioplot))
  {
    install.packages("vioplot")
    library(vioplot)
  }
  
if(!require(rlist))
{
	install.packages("rlist")
	library(rlist)
}


#this file contains the functions we have created
#for the project

#' Calculate the yield 
#' 
#' @details The yield is calculated as the difference between 
#' the natural logarithm of the closed adjust price at time t and
#' the natural logarithm of the closed adjust price at time t-1.
#' The yield when t = 1 is NA.
#' @param data the dataframe that contains the 4 temporal series
#' @return The yield. 
#' @references 
#'  \url{http://www.statistica.unimib.it/utenti/manera/Bee_1_2.pdf}
#' @export 
#' 
calculateYield <- function(data)
{
  n      <- dim(data)[1]
  yield  <- c(NA)
  for(i in 2:n)
  {
    yield <- c(yield, 
               log(data[,3][i])-log(data[,3][i-1]))
  }
  return(yield)
}


getLabelsFeatures <- function()
{
  return(c("Periodicity", "TSATrend",
           "SerialCorr","NonLinearity","Skewness","Kurtosis","SelfSim","Chaos",
           "TSASerialCorr","TSANonLinearity","TSASkewness","TSAKurtosis")
  )
}

getLabelsView <- function()
{
  return(c("yield", "range", "volatility", "Volume"))
}

#' Boxplot of the 13 misures
#'
#' @details For each variable 
#' (Yield,Range,Volatility,Adjusting closing price),
#' the function plot the boxplot of the 13 measures calculated with
#' the function measures(). 
#' @param df The table with the variables
#' @return void
#' @export 
#' 
print_box <- function(df)
{
  labVar = getLabelsView()
  par(mar=c(8, 4, 4, 2) + 0.1)
  for(i in 1:length(df))
  {
    #png(paste(paste("Boxplot", lab[i], sep="_"), "png",sep="."),
#         width=16, 
#         height=9, 
#         units="in",
#         res=300)
    boxplot(df[[i]], axes=F)
    title(labVar[i])
    axis(1, labels = getLabelsFeatures(), at=1:13, las=2)
    axis(2)
    box()
    #dev.off()
    print(i)
  }
  par(mar=c(5, 4, 4, 2) + 0.1)
  layout(1)
}


#' Boxplot of the 5 variables
#'
#' @details For each measure i,
#' the function plot the boxplot of the 
#' measure i of the 5 variables
#' @param data The table with the variables
#' @return void
#' @export 
#'
print_box2 <- function(data)
{
  par(mar=c(8, 4, 4, 2) + 0.1)
  labView     = getLabelsView()
  labFea      = getLabelsFeatures()
  n           = length(labFea)
  for(i in 1:n)
  {
    #png(paste(label[i], "png", sep="."), width=4, height=4, units="in", res=300)
    boxplot(data$yield[,i],
            data$range[,i],
            data$volatility[,i], 
            data$volume[,i],
            axes=F)
    title(labFea[i])
    axis(1, labels = labView, at=1:4, las=2)
    axis(2)
    box()
    #dev.off()
  }
  layout(1)
  par(mar=c(5, 4, 4, 2) + 0.1)
}

#' Vioplot of 13 features 
#'
#' @details For each measure i,
#' the function plot the vioplot of the 
#' measure i of the 4 variables
#' @param data data frame
#' @return void
#' @export 
#'
print_vioplot <- function(data)
{
  
  labView   = getLabelsView()
  n         = length(data)
  col       = rainbow(n)
  
  for(i in 1:n)
  {
    par(col.axis=NA)
    png(paste(paste("Vioplot", labView[i], sep="_"), "png",sep="."),
              width  =16, 
              height =10, 
              units  ="in",
              res    =300)
    par(mar =  c(9, 4, 4, 2) + 0.1)
    vioplot(unique(data[[i]][,1]),
            unique(data[[i]][,2]),
            unique(data[[i]][,3]),
            unique(data[[i]][,4]),
            unique(data[[i]][,5]),
            unique(data[[i]][,6]),
            unique(data[[i]][,7]),
            unique(data[[i]][,8]),
            unique(data[[i]][,9]),
            unique(data[[i]][,10]),
            unique(data[[i]][,11]),
            unique(data[[i]][,12]),
            col = col[i],
    		names = rep("",12))
    par(col.axis=1)
    axis(1, labels=getLabelsFeatures(), at=1:12, las=2, cex=0.9)
    axis(2)
    title(labView[i])
    dev.off()
    print(i)
  }
  
  layout(1)
  par(mar=c(5, 4, 4, 2) + 0.1)
}

coolBlueHotRed <- function(n, alpha = 1) { 
  rainbow(n, end=4/6, alpha=alpha)[n:1] 
}

as.sorted.data.frame = function(list)
{
	list = do.call("cbind", list)
	tmp = as.vector(list)
	list = as.data.frame(tmp, row.names = colnames(list))
	return (list[order(list$tmp, decreasing = T),,drop=FALSE])
}

drawVioplot <- function(data)
{
	labView   = getLabelsView()
	n         = length(data)
	clr		  = rainbow(n)
	for (i in 1:n)
	{
		png(paste(paste("RVioplot", labView[i], sep="_"), "png",sep="."),
			width  =16, 
			height =10, 
			units  ="in",
			res    =300)
		par(mar =  c(9, 4, 4, 2) + 0.1)
		params = list()
	
		names = colnames(data_extract[[i]])
		l = length(names)
	
	#	
		for (j in 1:l)
		{
			params[[j]] = (data[[i]][,j])
		}
		params['names'] = list(rep("", l))
		params['col'] = list(clr[i])
		do.call('vioplot', params)
 		axis(1, labels=names, at=1:l, las=2, cex=0.9)
		axis(2)
		title(labView[i])
		dev.off()
	}
}

plot.multi.dens <- function(s, clr)
{
	junk.x = NULL
	junk.y = NULL
	for(i in 1:length(s)) {
		junk.x = c(junk.x, density(s[[i]])$x)
		junk.y = c(junk.y, density(s[[i]])$y)
	}
	xr <- range(junk.x)
	yr <- range(junk.y)
	plot(density(s[[1]]), xlim = xr, ylim = yr, main = "", col = clr[1], xlab = " ")
	for(i in 1:length(s)) {
		lines(density(s[[i]]), xlim = xr, ylim = yr, col = clr[i], main = "")
	}
}

